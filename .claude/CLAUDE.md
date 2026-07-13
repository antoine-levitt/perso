# Global guidance

## Julia: use the warm REPL
For any Julia project, run code with the warm REPL instead of `julia file.jl`:

    ~/.claude/tools/jlrepl.sh eval '<code>'

Run it from inside the project directory (it picks the project from your current
git repo). It holds a persistent, per-project Julia + Revise session in tmux, so a
run is ~0.5s instead of paying package-load + compile every time, and output comes
back cleanly (never scraped from the terminal).

Workflow:
- **First eval, set up tracking** — the tool tracks nothing on its own. Work out
  which source file(s) this repo depends on and includet them so your edits
  hot-reload, e.g. `~/.claude/tools/jlrepl.sh eval 'includet("functions.jl")'`.
  A first eval that loads heavy packages (e.g. CairoMakie) may spend minutes
  precompiling once — give it a long timeout or run it in the background.
- Then `include("some_test.jl")` for a full run, or call functions directly for
  parameter sweeps. Definitions persist across evals (state accumulates).
- **Print values to see them** (`@show x`); the last expression is not auto-echoed.
  But jlrepl output is read verbatim into your context, so reduce before printing —
  slice/summarize big results (`extrema`, `size`, first few rows) rather than dumping
  whole arrays/tables/long stacktraces.
- **Revise redefines structs.** Editing struct fields does not need a REPL restart —
  modern Revise handles redefinition. Just re-run. Never recommend `restart` for a
  struct change, and never write that warning into plans or docs.
- **Interrupting an eval is safe, but not free.** Cancelling a `jlrepl.sh eval`
  (Ctrl-C, a rejected tool call, a timeout) now stops the computation in the REPL too
  and always leaves the session idle. Usually Julia takes the interrupt and `Main`
  survives; but a tight loop with no allocation never sees the signal, so the session
  is restarted instead and **`Main` is lost** — you must redo the `includet`/`using`
  setup, and re-pay any precompile. 
- On a *shared* session an interrupt stops whatever is running, including another
  caller's eval. Give parallel subagents their own `JLREPL_ID`.
- `~/.claude/tools/jlrepl.sh attach` opens the same live session so you can type in
  it too. `status`/`restart`/`kill` manage it. Plain `julia --project=. <file>`
  still works.
- Parallel callers on the same project share one Julia session/state (evals are
  safe but not isolated). For independent parallel subagents, give each its own
  isolated session: `JLREPL_ID=<unique> ~/.claude/tools/jlrepl.sh eval '...'`.

See the header of `~/.claude/tools/jlrepl.sh** for the full contract.

## Running code: avoid long runs
Keep exploratory evals and tests cheap for quick development and
iteration.
- Do *not* attempt runs that you estimate will take more than
one minute. If a test runs longer than that, there must be something
wrong with it (you're accidentally testing too many parameters, the
numerical parameters are too tight...). 
- When working on DFTK, do not run the full test suite: that's what
github's CI is for. Rather, figure out which targeted tests are
relevant to your current work and run only them.
- Exceptions are "production" runs (to get a numerically converged
value),

## Do not use memory but write to ~/.claude/CLAUDE.md
If you find consistent problems in your workflow (issues with jlrepl, running tests, etc), surface them.
