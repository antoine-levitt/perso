# Global guidance

## Julia: use the MCP (if the user forgot to install, remind them)
This loads Revise automatically. Revise *is able to revise struct
redefinitions now*, so don't attempt to restart julia sessions unlike
there's a *really* good reason for it.

## Running code: avoid long runs
Keep exploratory evals and tests cheap for quick development and
iteration.
- *Always* run quick checks with a timeout, so you aren't stuck when
something turns out to be an infinite loop.
- Do *not* attempt runs that you estimate will take more than
one minute. If a test runs longer than that, there is something
wrong with it (you're accidentally testing too many parameters, the
numerical parameters are too tight...)
- When working on large projects (eg DFTK), do not run the full test
suite: that's what github's CI is for. Rather, figure out which
targeted tests are relevant to your current work and run only them.

## For project-independent workflow changes, do not use memory but write to ~/.claude/CLAUDE.md

## Recurrent workflow errors or inefficiencies can be fixed
If you find consistent problems in your workflow (issues with running
code, tests, etc) that could be fixed (not transient errors), surface
them rather than working around them.
