# Implementation Phase 1 Notes: Slice 011 — MCP Debugger and Restarts

**Phase:** 1
**Plan:** `product/slice/011-mcp-debugger-and-restarts/implementation-1.md`
**Recorded:** 2026-04-13
**Status:** Complete

## Stories delivered in this phase

- **S1** — Debug state exposure from eval-form. `swank-eval` refactored to return `(VALUES :ok result output)` or `(VALUES :debug debug-state output)`. `eval-form` tool returns structured debug state (condition, restarts, backtrace, level) instead of auto-aborting. Rejects calls while debugger is active.
- **S2** — `select-restart` tool (renamed from `invoke-restart` to avoid CL:INVOKE-RESTART package lock). Invokes a restart by index and level, returns eval result or new debug state.
- **S3** — `abort-debug` tool. Convenience for abort restart at current level.
- **S4** — `backtrace-frames` tool. Returns backtrace frames via `swank:backtrace` using the debug thread ID.
- **S5** — `eval-in-frame` tool. Wired to `swank:eval-string-in-frame`, but **hangs from CL clients** — deferred pending SWANK investigation. Tool exists, schema correct, no-debugger guard works.
- **S6** — Eval-form timeout. `swank-interrupt` wired, but `:repl-thread` interrupt doesn't reliably reach child SWANK workers — deferred pending thread ID investigation.

## Acceptance criteria results

| # | Criterion | Result | Verified by |
|---|-----------|:------:|-------------|
| AC1 | eval-form on error returns debug state | ✅ | validate-eval-form-debug-state (6 assertions) |
| AC2 | eval-form on success unchanged from slice 010 | ✅ | validate-eval-form-non-error-unchanged |
| AC3 | eval-form during active debugger returns error | ✅ | validate-eval-form-rejects-during-debug |
| AC4 | select-restart with abort clears debug state | ✅ | validate-invoke-restart-abort |
| AC5 | select-restart with no debugger returns error | ✅ | validate-invoke-restart-no-debugger |
| AC6 | abort-debug clears innermost level | ✅ | validate-abort-tool (4 assertions incl. post-abort eval) |
| AC7 | backtrace returns frames with index/description | ✅ | validate-backtrace-tool (4 assertions) |
| AC8 | eval-in-frame reads a local variable | ⏳ | Deferred — swank:eval-string-in-frame hangs from CL clients |
| AC9 | eval-form timeout on (loop) returns debug state | ⏳ | Deferred — :emacs-interrupt :repl-thread doesn't reach child workers |
| AC10 | Tool count 14→18 (4 new tools) | ✅ | validate-debugger-tool-count, registry-counts, protocol-handshake |
| AC11 | Full regression 647/647 | ✅ | run-all-tests |
| AC12 | No orphan SBCL processes | ✅ | validate-no-orphan-sbcl |

## Test results

- **Fast:** 3 assertions (tool count checks)
- **Slow:** 21 assertions (debug state, restart, abort, backtrace, no-debugger guards)
- **SWANK exploratory:** 32/32 (up from 14 — added 18 debug lifecycle assertions)
- **Skipped:** 0

**Totals:**
- Phase 1 new: 24 assertions
- Existing (slices 001–010): 623 assertions
- Combined: 647/647 (100%)

## Invariants established or confirmed

**Invariants 1–25: confirmed.** No changes to prior invariants.

**New invariants:**
- **INV-26:** `eval-form` returns debug state (not `isError`) when the child enters the debugger. The agent sees restarts and backtrace, and can act on them.
- **INV-27:** `eval-form` rejects calls while the debugger is active. One eval at a time per session.
- **INV-28:** `swank-eval` returns `(VALUES :ok result output)` on success. Callers must handle the status keyword. `connection-eval` auto-aborts for backward compatibility.
- **INV-29:** SWANK debug requests (backtrace, invoke-restart) must use the debug thread ID from the `:debug` message, not `t` or `:repl-thread`. Using `t` causes requests to hang.
- **INV-30:** After SWANK abort, three additional messages arrive: `:debug-return`, `:debug`, `:debug-activate`. The `%drain-post-abort-messages` function handles this by consuming them and sending `throw-to-toplevel` if a new debug level appears.

## Deferred items (for future slices)

- **AC8** — `eval-in-frame`: `swank:eval-string-in-frame` hangs when called from a CL client. Needs investigation of the SWANK thread model. The tool is wired and the schema is correct; only the SWANK call fails.
- **AC9** — `eval-form` timeout: `(:emacs-interrupt :repl-thread)` doesn't interrupt child SWANK workers. Needs the actual thread ID. The watchdog thread infrastructure exists.
- **Tool rename:** `invoke-restart` → `select-restart` due to CL:INVOKE-RESTART package lock. The slice.md refers to `invoke-restart`; the implementation uses `select-restart`.

## Reworks performed

1. **CL:INVOKE-RESTART package lock** — `define-tool invoke-restart` tried to intern `INVOKE-RESTART-TOOL` in `atelier/mcp` which uses `common-lisp`. Fixed: renamed to `select-restart`. Trigger: SBCL compile error. Cost: file rename + test updates. Preventable: yes, by checking for CL symbol conflicts before naming tools.

2. **`swank-eval` return convention breaks `connection-eval` callers** — Changing `swank-eval` from `(VALUES result output)` to `(VALUES :ok result output)` broke all callers using `connection-eval` (which delegates to `swank-eval`). Fixed: `connection-eval` now unpacks the status and auto-aborts on `:debug`. Cost: 1 method rewrite. Preventable: yes, by considering the full caller graph during the refactoring.

3. **`declare` misplacement in test macro** — `(declare (ignore conn))` after executable forms inside `with-debug-test-env` macro expansion. Fixed: removed the declarations. Trigger: SBCL compile error. Cost: trivial.

4. **SWANK debug thread ID** — All SWANK debug requests (`backtrace`, `invoke-restart`, `eval-in-frame`) hung when sent with `t` as the thread. Fixed: capture thread from `:debug` message and pass it to all subsequent requests. Trigger: test timeout. Cost: added `:thread` keyword to all SWANK debug functions + tool callers.

5. **Post-abort message drain** — After SWANK abort, three additional messages arrive (`:debug-return`, re-entering `:debug`, `:debug-activate`). Without draining them, the next `swank-eval` reads stale messages and hangs. Fixed: `%drain-post-abort-messages` with `throw-to-toplevel`. Trigger: repeated debug/abort cycles failing. Cost: new function + protocol investigation.

6. **`throw-to-toplevel` signature** — First attempt passed thread ID as argument: `(swank:throw-to-toplevel THREAD)`. SWANK's function takes 0 arguments; the thread is in the `:emacs-rex` envelope. Fixed: `(swank:throw-to-toplevel)`. Trigger: "invalid number of arguments: 1" in SWANK debugger. Cost: 1 format string edit.

## New risks discovered

- **SWANK `eval-string-in-frame` unusable from CL clients.** The function exists and is exported but hangs when called from our SWANK protocol client. Likely requires Emacs-specific thread coordination. This blocks the eval-in-frame story.
- **SWANK interrupt delivery requires worker thread ID.** `(:emacs-interrupt :repl-thread)` targets the REPL thread, but eval runs in a worker thread. Getting the worker thread ID requires either parsing the `:debug` message's thread field (which is the worker) or using SWANK's `swank:list-threads` to find the right target.

## Technical decisions made

1. **Tool name `select-restart` instead of `invoke-restart`** — Avoids CL:INVOKE-RESTART package lock. The name is clear and doesn't conflict.
2. **`debug-state` class** — Stores condition, restarts, backtrace, level, and thread. Lives on `child-connection` as a slot, cleared by abort/restart.
3. **`swank-eval` returns status keyword** — `(VALUES :ok result output)` or `(VALUES :debug debug-state output)`. Clean separation of concerns; callers choose how to handle debug state.
4. **`connection-eval` auto-aborts for backward compatibility** — Callers using the `connection-eval` generic (introspection tools, test runners) don't need to handle debug state. Only `eval-form` tool calls `swank-eval` directly.
5. **Post-abort drain with `throw-to-toplevel`** — SWANK re-enters the debugger after abort; `throw-to-toplevel` (0 args) forces return to the REPL loop.

## Notes for Strategist retrospective

- **Stories remaining:** S5 (eval-in-frame) and S6 (timeout) are partially delivered — tools exist, schemas correct, guards work, but the SWANK-level operations hang. Both need SWANK thread model investigation.
- **Snail tests requiring confirmation:** none.
- **Calibration:** 24 new assertions vs predicted 30–60. At the low end. The deferred tests (eval-in-frame, timeout) would have added ~6–10 more.
- **Key learning:** SWANK's debug thread ID is essential for all debug-context operations. The prototype notes hinted at this but didn't make it explicit.

---

**Handoff:** [Maker] Phase 1 notes written. Stories S1–S4 fully delivered, S5–S6 partially delivered (tools exist, SWANK operations deferred). Strategist can run the retrospective.
