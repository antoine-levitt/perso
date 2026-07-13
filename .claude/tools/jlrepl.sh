#!/usr/bin/env bash
# jlrepl.sh — a warm, per-project Julia REPL (Revise-enabled), driven without
# screen-scraping. Kills Julia start-up + compile latency on the edit/run loop.
#
#   ~/.claude/tools/jlrepl.sh eval '<julia code>'   # run in the project's warm REPL
#   ~/.claude/tools/jlrepl.sh start|attach|status|restart|kill
#
# Run it from inside the project (session keyed to your current git repo). It
# tracks nothing on its own — in your first eval, includet the repo's source:
#   ~/.claude/tools/jlrepl.sh eval 'includet("functions.jl")'
# Values aren't auto-echoed — print them (@show x); defs persist across evals; a
# heavy first precompile (e.g. CairoMakie) can take minutes. Parallel callers
# share one session/Main (safe, not isolated); set JLREPL_ID=<x> for a private one.
#
# Interrupting an eval (Ctrl-C, a rejected tool call) or letting it time out also stops
# the computation in the REPL, rather than leaving it running and blocking every later
# eval. The session is always left idle: by ^C if Julia takes it (Main survives), else
# by a restart (Main is lost — a tight loop with no allocation never sees the SIGINT).
# Note this ^Cs the *session*: on a shared session it also stops whatever another caller
# is running, so give parallel work its own JLREPL_ID.
set -u
PROG="$(basename "$0")"
SESSION_FILE="$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")/jl_session.jl"

PROJDIR="${JLREPL_PROJECT:-$(git -C "$PWD" rev-parse --show-toplevel 2>/dev/null || pwd)}"
NAME="jl-$(basename "$PROJDIR" | tr -c 'A-Za-z0-9' '_')-$(printf '%s' "$PROJDIR" | cksum | cut -d' ' -f1)"
[ -n "${JLREPL_ID:-}" ] && NAME="$NAME-$(printf '%s' "$JLREPL_ID" | tr -c 'A-Za-z0-9' '_')"
RUNDIR="${TMPDIR:-/tmp}/jlrepl-$NAME"; mkdir -p "$RUNDIR"
READY="$RUNDIR/ready"; TIMEOUT=600

running(){ tmux has-session -t "$NAME" 2>/dev/null; }

start(){
  running && return 0
  command -v tmux  >/dev/null || { echo "error: tmux not found"        >&2; return 1; }
  command -v julia >/dev/null || { echo "error: julia not on PATH"     >&2; return 1; }
  [ -f "$SESSION_FILE" ]      || { echo "error: missing $SESSION_FILE" >&2; return 1; }
  # Auto-install Revise into the shared env if missing (so a fresh machine works).
  julia --startup-file=no -e 'try; @eval using Revise; catch; import Pkg; Pkg.add("Revise"); end' >/dev/null 2>&1
  # Only the caller that creates the session launches Julia, so racing cold-starts
  # can't double-launch (new-session fails if the session already exists).
  if tmux new-session -d -s "$NAME" -x 200 -y 50 2>/dev/null; then
    rm -f "$READY"
    tmux send-keys -t "$NAME" -l "cd \"$PROJDIR\" && JLREPL_READY=\"$READY\" julia --project=. -i \"$SESSION_FILE\""
    tmux send-keys -t "$NAME" Enter
    echo "starting $NAME (project: $PROJDIR)" >&2
  fi
  local t=0
  while (( t < TIMEOUT*4 )); do [ -f "$READY" ] && return 0; sleep 0.25; ((t++)); done
  echo "warning: $NAME not ready after ${TIMEOUT}s" >&2; return 0
}

# Stop whatever the REPL is currently running and wait for it to come back to the
# prompt. ^C into the pane reaches Julia as a real SIGINT through the pty; jlx()
# catches the resulting InterruptException and still writes its .done file, so that
# file appearing is our proof the REPL is idle again. Julia cannot interrupt inside a
# non-yielding C call (BLAS, FFTW, a long precompile), hence several attempts.
interrupt_repl(){
  local out="$1" i j
  for i in 1 2 3; do
    tmux send-keys -t "$NAME" C-c 2>/dev/null || return 1
    for ((j = 0; j < 12; j++)); do
      [ -f "$out.done" ] && return 0
      sleep 0.25
    done
  done
  return 1
}

# A dying client used to leave Julia running: killing the waiter (Ctrl-C, a rejected
# tool call, a timeout) never signalled the REPL, so the abandoned computation kept
# burning CPU and every later eval silently queued behind it. Leave the session idle
# instead — by ^C if Julia will take it, by force if it won't. Either way the next
# eval gets a working REPL, which is the invariant callers actually depend on.
abandon(){
  local in="$1" out="$2" why="$3"
  echo "[jlrepl] $why — interrupting the Julia eval" >&2
  if interrupt_repl "$out"; then
    echo "[jlrepl] REPL is idle again" >&2
  else
    # SIGINT cannot land inside a tight loop with no allocation or safepoint (Julia
    # only checks for it at yield points), so ^C alone can't guarantee the invariant.
    # Restarting loses Main — worse than an interrupt, far better than a session
    # pinned at 100% CPU that silently queues every later eval behind it.
    echo "[jlrepl] REPL would not interrupt (tight non-yielding loop?) — restarting the session" >&2
    tmux kill-session -t "$NAME" 2>/dev/null
    start
    echo "[jlrepl] session restarted: Main is empty, redo your includet/using setup" >&2
  fi
  rm -f "$in" "$out" "$out.done"
}

eval_code(){
  running || start || return 1
  local in out t=0
  in="$(mktemp "$RUNDIR/in.XXXXXX")"; out="$(mktemp "$RUNDIR/out.XXXXXX")"; rm -f "$out.done"
  printf '%s\n' "$1" > "$in"
  trap 'abandon "$in" "$out" "client interrupted"; exit 130' INT TERM HUP
  # Per-call temp files keep concurrent evals from clobbering each other; the lock
  # keeps their keystrokes from interleaving in the shared pane. The REPL runs the
  # queued jlx() calls serially and each writes its own file.
  { flock 9; tmux send-keys -t "$NAME" -l "jlx(\"$in\", \"$out\")"; tmux send-keys -t "$NAME" Enter; } 9>"$RUNDIR/send.lock"
  while (( t < TIMEOUT*4 )); do
    [ -f "$out.done" ] && { trap - INT TERM HUP; cat "$out"; rm -f "$in" "$out" "$out.done"; return 0; }
    sleep 0.25; ((t++))
  done
  trap - INT TERM HUP
  echo "[jlrepl] timeout after ${TIMEOUT}s; partial:"; cat "$out" 2>/dev/null
  abandon "$in" "$out" "timed out after ${TIMEOUT}s"
  return 1
}

case "${1:-}" in
  eval)    eval_code "${2:?usage: $PROG eval '<code>'}" ;;
  start)   start ;;
  attach)  tmux attach -t "$NAME" ;;
  status)  running && echo "running: $NAME" || echo "not running: $NAME"; echo "project: $PROJDIR" ;;
  restart) tmux kill-session -t "$NAME" 2>/dev/null; start ;;
  kill)    tmux kill-session -t "$NAME" 2>/dev/null && echo "killed $NAME" || echo "not running" ;;
  *) echo "usage: $PROG {eval '<code>'|start|attach|status|restart|kill}"; exit 1 ;;
esac
