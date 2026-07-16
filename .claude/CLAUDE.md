# General guidance

## Comments: write for the next reader, not the reviewer
When fixing a simple bug, the default number of new comment lines is
zero. Rationale goes in the commit message. Never a comment that only
makes sense to someone who saw the diff.

## Avoid accretive editing
A recent example of a diff you made on a tricky promotion bug

-    # equation (9)
-    f(ζ) = ((1 + ζ)^(4 / T(3)) + (1 - ζ)^(4 / T(3)) - 2) / (2^(4 / T(3)) - 2)  # == 0 for non-spin-polarised
+    # equation (9). The base has to be `T(2)` and not `2`: for a `Dual` exponent an integer
+    # base is differentiated via `log(2)` in Float64, which widens a Float32 evaluation to
+    # Float64 (the other two bases are already `T`, being built from ζ).
+    f(ζ) = ((1 + ζ)^(4 / T(3)) + (1 - ζ)^(4 / T(3)) - 2) / (T(2)^(4 / T(3)) - 2)  # == 0 for non-spin-polarised

The fix is correct, but the comment is much too detailed. The T()
pattern was already established, it was just missing at a single
place. As a result, the interesting stuff in the file (the actual
mathematical formula) gets bogged down by a very technical comment.

This is an instance of a general failure mode you have when editing
text (code comments, but not only): you tend to add comments relevant
to the particular issue you're trying to fix at the time, while losing
track of the broader context. Don't do that.

## For project-independent guidance, do not use memory but write to ~/.claude/CLAUDE.md

## Recurrent workflow errors or inefficiencies can be fixed
If you find consistent problems in your workflow (issues with running
code, tests, etc) that could be fixed (not transient errors), surface
them rather than working around them.

## Do not make PRs yourself
This is user-facing and it's rude for me to impose an AI on reviewers.
When I tell you to "make a PR", make the changes to the branch, push,
and then give me the github URL to open a PR so I can write the
message myself.

## I do not use .gitignore
Don't blindly `git add` stuff, just what you want committed.

# Julia-specific workflow

## Use the MCP (if the user forgot to install, remind them)
This loads Revise automatically. Revise *is able to revise struct
redefinitions now*, so don't attempt to restart julia sessions unlike
there's a *really* good reason for it.

### Writing scratch scripts to `include()` in the session: careful about scope
Top-level `for` loops open a *soft scope*: assigning to a variable that
also exists as a global warns ("Assignment to `x` in soft scope is
ambiguous") and treats it as a fresh local, so accumulators silently
break with `UndefVarError`. Wrap any loop that assigns to outer
variables in a function. Likewise, avoid top-level `const` in scripts
that get re-included: re-`include` after an edit errors with "invalid
assignment to constant" / "cannot declare constant; already declared
global". Prefer putting the whole script body in functions and calling
them at the end.

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

