# Retrospective: Slice 014 — MCP xref, CLOS inspector, trace

**Recorded:** 2026-04-14
**Implementation phases:** 1

## Delivery Summary

- Stories delivered: 9 of 9
- Acceptance criteria: 16/16 passing
- Quality criteria: all passing
- Full MCP test suite: 352/352 (100%)

## Phase Notes Summary

| Phase | Key deliverables | Deferred to backlog |
|-------|-----------------|---------------------|
| 1 | 10 new MCP tools, 3 bug fixes, 3 journey tests, UIOP discovery experiments | MCP reliability (slice 015) |

## Goal Progress

- **G5** closer — 40 MCP tools total. Xref + CLOS introspection + trace + impacted-test runner complete the introspection surface needed for safe refactoring.
- Remaining for G5: refactorings (slice 015+), domain diagnostics (slice 016+)

## What we learned

1. **`defvar` load order matters for dynamic binding.** When `*current-server*` was defined after its binding site in the dispatcher, the `let` created a lexical binding. Tests didn't catch this because they bypassed the dispatcher. **Lesson:** journey tests that exercise the real protocol path are essential — unit tests that call `handle-tool-call` directly bypass the binding context.

2. **UIOP handles subprocess I/O without threads.** Discovery experiments confirmed that `uiop:run-program` and `uiop:launch-program` support both stdout and stderr as function processors simultaneously on SBCL. The hand-rolled drain thread in `make-child-connection` is unnecessary. UIOP's design: redirect the less-important stream to a temp file to avoid pipe-buffer deadlock.

3. **The MCP server's broken-pipe recovery is broken.** `connection-alive-p` checks process liveness but not SWANK socket health. A dead SWANK socket with a live process causes persistent "broken pipe" errors requiring client reconnection. This was the most disruptive issue during development — triggered 5+ reconnection cycles.

4. **Trace output requires per-eval `*trace-output*` binding.** SBCL's `trace` writes to `*trace-output*`, not `*standard-output*`. SWANK's `eval-and-grab-output` only captures `*standard-output*`. Setting `*trace-output*` globally in `trace-function-data` doesn't work because SWANK creates a fresh capture stream per eval. The correct pattern is `(let ((*trace-output* *standard-output*)) ...)` at eval time. Proper separate capture is a slice 015 story.

## Backlog and roadmap changes

- Completed: backlog item #10 (xref + inspector + trace)
- Added: slice 015 (MCP reliability) — 6 stories for SWANK health, output capture, test speed, MCP kernel, targeted test execution
- UIOP reference written at `~/Workshop/agent/skills/common-lisp/references/uiop.md`
- Discovery experiments saved at `testsuite/discovery/uiop-output-handling.lisp` and `uiop-experiments.lisp`
