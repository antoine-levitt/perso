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

eval_code(){
  running || start || return 1
  local in out t=0
  in="$(mktemp "$RUNDIR/in.XXXXXX")"; out="$(mktemp "$RUNDIR/out.XXXXXX")"; rm -f "$out.done"
  printf '%s\n' "$1" > "$in"
  # Per-call temp files keep concurrent evals from clobbering each other; the lock
  # keeps their keystrokes from interleaving in the shared pane. The REPL runs the
  # queued jlx() calls serially and each writes its own file.
  { flock 9; tmux send-keys -t "$NAME" -l "jlx(\"$in\", \"$out\")"; tmux send-keys -t "$NAME" Enter; } 9>"$RUNDIR/send.lock"
  while (( t < TIMEOUT*4 )); do
    [ -f "$out.done" ] && { cat "$out"; rm -f "$in" "$out" "$out.done"; return 0; }
    sleep 0.25; ((t++))
  done
  echo "[jlrepl] timeout after ${TIMEOUT}s; partial:"; cat "$out" 2>/dev/null; rm -f "$in" "$out" "$out.done"; return 1
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
