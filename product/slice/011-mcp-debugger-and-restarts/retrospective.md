# Retrospective: Slice 011 — MCP Debugger and Restarts

**Recorded:** 2026-04-13
**Implementation phases:** 1 total
  - Phase 1: `implementation-1.md` — Debugger tools (complete)

## Delivery Summary

- Stories delivered: 4 of 7 fully (S1–S4), 2 partially (S5–S6 tools exist, SWANK ops deferred), 1 coverage (S7)
- Acceptance criteria: 10 of 12 passed, 2 deferred (AC8 eval-in-frame, AC9 timeout)
- Quality criteria: all passing
- Full test suite: 647/647 (100%) in fresh SBCL subprocess

## Phase Notes Summary

| Phase | Key deliverables | Deferred to backlog |
|-------|-----------------|---------------------|
| 1 | `debug-state` class, refactored `swank-eval` (returns status keyword), `select-restart` tool, `abort-debug` tool, `backtrace-frames` tool, `eval-in-frame` tool (schema only), `%drain-post-abort-messages` with `throw-to-toplevel`, 24 new assertions. 4 new tools (14→18). | eval-in-frame SWANK hang, eval-form timeout interrupt delivery |

## Goal Progress

- **G5** (MCP server exposes REPL evaluation, debugger, CLOS introspection, rename refactorings): closer — debugger access, restart selection, and backtrace are now operational. eval-in-frame and timeout need SWANK thread model work. Debugger (slice 011 core) delivered. ASDF operations (slice 012), documentation (slice 013), and rename refactorings (slice 015) remain.

## What we learned

1. **SWANK's debug thread ID is non-negotiable.** All debug-context requests (backtrace, invoke-restart, eval-in-frame) must be sent on the thread that entered the debugger, not on `t` (the REPL thread). This was implicit in the SWANK protocol but never stated explicitly. Using `t` causes requests to hang silently. The debug thread ID comes from the `:debug` message's second element.

2. **SWANK re-enters the debugger after abort.** After `invoke-nth-restart-for-emacs` with the ABORT restart, SWANK sends three additional messages: `:debug-return`, a new `:debug`, and `:debug-activate`. This means the REPL catches the abort and re-enters the debugger. The fix is to drain these messages and send `throw-to-toplevel` (0 arguments) to force return to the REPL loop.

3. **CL package lock on tool names.** The `define-tool` macro interns `NAME-TOOL` in the current package. If `NAME` shadows a CL symbol (like `invoke-restart`), SBCL raises a package lock violation. Tool names must be checked against CL exports before use.

4. **Exploratory tests caught every SWANK surprise.** All 6 reworks were SWANK protocol issues. The exploratory test system (`testsuite/swank/`) was the diagnostic tool for each. The pattern "test the external system's behavior before writing production code" is now validated across two slices.

## User feedback since delivery

*(to be filled 2–4 weeks after release)*

## Backlog and roadmap changes

- Completed: backlog item #7 (MCP debugger access) — core debugger delivered, eval-in-frame and timeout deferred
- Added: eval-in-frame SWANK investigation and timeout interrupt delivery as polish items
- Deferred AC28 (eval-form timeout from slice 010) remains deferred — needs SWANK worker thread ID
- No reprioritization needed
