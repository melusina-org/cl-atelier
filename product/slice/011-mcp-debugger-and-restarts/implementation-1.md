# Implementation Phase 1: Slice 011 — MCP Debugger and Restarts

**Slice:** `product/slice/011-mcp-debugger-and-restarts/slice.md`
**Phase:** 1 (single phase)
**Scope:** All seven stories — debug state exposure, five debugger tools, eval-form timeout.
**Prerequisites:** Slice 010 complete (623/623 tests).

## Prior Phases

None — this is phase 1.

## Project Knowledge Applied

- **patterns.md — "SWANK functions designed for Emacs"**: every SWANK function (`backtrace`, `frame-locals-and-catch-tags`, `eval-string-in-frame`) verified via exploratory test before production use.
- **patterns.md — "Exploratory test systems"**: extend `testsuite/swank/` rather than ad-hoc scripts.
- **calibration.md**: slow-test-dominated phase → predict 30–60 new assertions.
- **reworks.md**: expect 2–3 SWANK protocol surprises.

## Risk Register

| # | Risk | Category | Mitigation |
|---|------|----------|-----------|
| R1 | Hallucinated requirements | Scope | Every tool traces to an AC in slice.md |
| R2 | SWANK debug protocol surprises | Library API | Extend exploratory tests first |
| R3 | swank-eval refactoring breaks happy path | Regression | Full test suite after refactoring |
| R4 | Timeout race conditions | State/lifecycle | Interrupt on non-running eval is harmless |
| R5 | eval-in-frame unavailable in SWANK | Library API | Verify via exploratory test; defer if broken |
| R6 | Nested debug cleanup in tests | Test dependencies | unwind-protect with multi-level abort |

## OSS Components

None new. All dependencies from slice 010 unchanged.

## Phase Scope

**Stories covered:** S1 (debug state), S2 (invoke-restart), S3 (abort), S4 (backtrace), S5 (eval-in-frame), S6 (timeout), S7 (tests).
**Stories deferred:** None.

## File Organisation

```
src/mcp/
├── package.lisp              [modify — new exports]
├── conditions.lisp           [modify — debugger-active condition]
├── swank-protocol.lisp       [modify — refactor eval, add debug functions]
├── image-connection.lisp     [modify — debug-state slot on child-connection]
├── tools/
│   ├── eval-form.lisp        [modify — debug state return, timeout]
│   ├── invoke-restart.lisp   [new]
│   ├── abort-debug.lisp      [new]
│   ├── backtrace.lisp        [new]
│   └── eval-in-frame.lisp   [new]
testsuite/
├── mcp/
│   ├── child-tests.lisp      [modify — update eval-form-error test, tool count]
│   ├── debugger-tests.lisp   [new]
│   └── entrypoint.lisp       [modify — include debugger tests]
├── swank/
│   └── wire-protocol.lisp    [modify — add debug exploratory tests]
org.melusina.atelier.asd       [modify — new files]
```

## Build System Changes

Add to `:org.melusina.atelier/mcp` tools module:
```
(:file "invoke-restart")
(:file "abort-debug")
(:file "backtrace")
(:file "eval-in-frame")
```

Add to `:org.melusina.atelier/testsuite` MCP module:
```
(:file "debugger-tests")
```

## Package / Module Architecture

New exports in `#:atelier/mcp`:

```
;; Debug state (slice 011)
#:debug-state
#:debug-state-condition
#:debug-state-restarts
#:debug-state-backtrace
#:debug-state-level
#:debug-state-thread
#:connection-debug-state
#:debugger-active
;; SWANK debug protocol (slice 011)
#:swank-invoke-restart
#:swank-backtrace-frames
#:swank-eval-in-frame
#:swank-interrupt
```

## Type / Class Hierarchy

```
debug-state (new)
  condition-text : string
  restarts       : list  ; alist per restart: ((index . N) (name . "...") (description . "..."))
  backtrace      : list  ; alist per frame: ((index . N) (description . "..."))
  level          : integer
  thread         : t     ; SWANK thread id

debugger-active (condition, new)
  inherits: mcp-error
```

## Protocol Definitions

```lisp
;; Refactored: swank-eval now returns (VALUES result output) on success
;; or signals a debug-state-available condition when the debugger is entered.
;; The condition carries the debug-state.

(define-condition debug-state-available ()
  ((debug-state :initarg :debug-state :reader debug-state-available-state)))

;; New SWANK protocol functions:
(defun swank-invoke-restart (connection level index)
  "Send invoke-nth-restart-for-emacs and process the result.")

(defun swank-backtrace-frames (connection start end)
  "Request backtrace frames from the SWANK server.")

(defun swank-eval-in-frame (connection expression frame-index &key package)
  "Evaluate EXPRESSION in the context of FRAME-INDEX.")

(defun swank-interrupt (connection)
  "Send :emacs-interrupt to interrupt a running eval.")
```

## Error / Condition Types

| Name | Superclass | Slots | When signalled |
|------|-----------|-------|----------------|
| `debugger-active` | `mcp-error` | — | `eval-form` called while debugger is active |
| `debug-state-available` | `condition` | `debug-state` | `swank-eval` enters debugger (internal, not exported) |

## Test Plan

| Story | Test name | Category | Skip condition |
|-------|-----------|:--------:|----------------|
| S1 | `validate-eval-form-debug-state` | slow | `#-sbcl` |
| S1 | `validate-eval-form-debug-state-fields` | slow | `#-sbcl` |
| S1 | `validate-eval-form-non-error-unchanged` | slow | `#-sbcl` |
| S1 | `validate-eval-form-rejects-during-debug` | slow | `#-sbcl` |
| S2 | `validate-invoke-restart-abort` | slow | `#-sbcl` |
| S2 | `validate-invoke-restart-no-debugger` | slow | `#-sbcl` |
| S3 | `validate-abort-tool` | slow | `#-sbcl` |
| S3 | `validate-abort-no-debugger` | slow | `#-sbcl` |
| S4 | `validate-backtrace-tool` | slow | `#-sbcl` |
| S4 | `validate-backtrace-no-debugger` | slow | `#-sbcl` |
| S5 | `validate-eval-in-frame-tool` | slow | `#-sbcl` |
| S5 | `validate-eval-in-frame-no-debugger` | slow | `#-sbcl` |
| S6 | `validate-eval-form-timeout` | slow | `#-sbcl` |
| S7 | `validate-debugger-tool-count` | fast | — |

## Implementation Order (Step Table)

| Step | File | Action | Form(s) | Test name | Cat |
|------|------|--------|---------|-----------|:---:|
| 1 | `references/swank-debug-protocol.md` | Write | — | — | — |
| 2 | `testsuite/swank/wire-protocol.lisp` | Modify | `validate-swank-debug-backtrace`, `validate-swank-debug-frame-locals`, `validate-swank-debug-eval-in-frame`, `validate-swank-interrupt` | (exploratory, manual) | slow |
| 3 | `src/mcp/package.lisp` | Modify | new exports | — | — |
| 4 | `src/mcp/conditions.lisp` | Modify | `debugger-active` | — | — |
| 5 | `src/mcp/swank-protocol.lisp` | Modify | `debug-state`, `%swank-receive-loop`, refactored `swank-eval` | existing tests must pass | slow |
| 6 | `src/mcp/swank-protocol.lisp` | Modify | `swank-invoke-restart`, `swank-backtrace-frames`, `swank-eval-in-frame`, `swank-interrupt` | exploratory tests | slow |
| 7 | `src/mcp/image-connection.lisp` | Modify | `debug-state` slot, `connection-debug-state` | — | — |
| 8 | `src/mcp/tools/eval-form.lisp` | Modify | debug state return, timeout, reject during debug | `validate-eval-form-debug-state` | slow |
| 9 | `src/mcp/tools/invoke-restart.lisp` | New | `invoke-restart` tool | `validate-invoke-restart-abort` | slow |
| 10 | `src/mcp/tools/abort-debug.lisp` | New | `abort` tool | `validate-abort-tool` | slow |
| 11 | `src/mcp/tools/backtrace.lisp` | New | `backtrace` tool | `validate-backtrace-tool` | slow |
| 12 | `src/mcp/tools/eval-in-frame.lisp` | New | `eval-in-frame` tool | `validate-eval-in-frame-tool` | slow |
| 13 | `org.melusina.atelier.asd` | Modify | add 4 tool files + 1 test file | — | — |
| 14 | `testsuite/mcp/debugger-tests.lisp` | New | all debugger tests | all S1–S7 tests | slow |
| 15 | `testsuite/mcp/child-tests.lisp` | Modify | update tool count (14→19), update `validate-eval-form-error` | `validate-debugger-tool-count` | fast |
| 16 | `testsuite/mcp/entrypoint.lisp` | Modify | include debugger tests in runner | — | — |
| 17 | Full regression | — | — | 623 + new assertions | slow |

## Invariants

**Carry forward:** INV-1 through INV-25 — all confirmed.

**New (candidates):**
- **INV-26:** `eval-form` returns debug state (not `isError`) when the child enters the debugger. The agent sees restarts and backtrace.
- **INV-27:** `eval-form` rejects calls while the debugger is active. One eval at a time.
- **INV-28:** `swank-eval` refactoring preserves the happy path exactly. Non-error evals unchanged.

## Acceptance Criteria

| # | Criterion | Maps to |
|---|-----------|---------|
| AC1 | `eval-form` on `(error "test")` returns `in_debugger: true` with condition, restarts, backtrace, level | S1 |
| AC2 | `eval-form` on `(+ 1 2)` returns same shape as slice 010 (backward compat) | S1 |
| AC3 | `eval-form` during active debugger returns `isError: true` with "debugger is active" | S1 |
| AC4 | `invoke-restart` with abort clears debug state, returns aborted result | S2 |
| AC5 | `invoke-restart` with no debugger returns `isError: true` | S2 |
| AC6 | `abort` clears innermost debug level | S3 |
| AC7 | `backtrace` returns frame array with index and description | S4 |
| AC8 | `eval-in-frame` reads a local variable | S5 |
| AC9 | `eval-form` with timeout on `(loop)` returns debug state within timeout+2s | S6 |
| AC10 | Tool count increases from 14 to 19 (5 new tools) | S7 |
| AC11 | Full regression 623+ passes in fresh SBCL subprocess | S7 |
| AC12 | No orphan SBCL processes | S7 |

## Phase Closure Conditions

- All acceptance criteria met (AC1–AC12)
- Full `(asdf:test-system "org.melusina.atelier")` green in fresh subprocess
- No orphan SBCL processes
- SWANK exploratory tests extended and passing
- `CLAUDE.md` updated with debugger tool summary
