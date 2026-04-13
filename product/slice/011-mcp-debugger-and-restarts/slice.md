# Slice 011: MCP debugger and restarts

**Status:** Complete
**Type:** New capability
**Goal addressed:** G5 — MCP server exposes REPL evaluation, debugger and image lifecycle, CLOS introspection, and rename refactorings
**Backlog items:** #7 (MCP debugger access, restart selection, condition introspection, sldb-equivalent); partial delivery of deferred AC28 (eval-form timeout) from slice 010.
**Planned start / end:** 2026-04-12 / TBD (Tactician predicts at the planning interview).
**Actual end:** 2026-04-13
**Implementation phases:**
  - Phase 1: `product/slice/011-mcp-debugger-and-restarts/implementation-1.md` — Planned
  - _(Tactician may split into phase 2 during the planning interview.)_

---

## What changes for users

After this slice, an MCP-aware client (Claude Desktop, Cursor, or any MCP host) connected to `atelier-mcp` can:

- **See what went wrong when an eval errors.** When `atelier:eval-form` encounters a condition that enters the SWANK debugger, instead of auto-aborting and returning a generic error string, it returns a structured **debug state**: the condition text, the list of available restarts (each with index, name, and description), and the top backtrace frames (each with index and description). The agent sees what SLIME's `*sldb*` buffer would show.
- **Choose a restart.** The agent calls `atelier:invoke-restart` with a restart index and debug level. If the restart resolves the condition, the original eval completes and the agent receives its result. If the restart causes another condition, a new debug state is returned. The agent can drill down through nested debugger levels exactly as a human does in SLIME.
- **Abort the current error.** `atelier:abort` is a convenience tool that invokes the ABORT restart at the current debug level — the common case when the agent just wants to get back to a working state.
- **Inspect the backtrace.** `atelier:backtrace` returns the full backtrace from the current debug state, with more frames and richer detail (source location, locals availability) than the summary returned by `eval-form`.
- **Evaluate expressions in a frame's context.** `atelier:eval-in-frame` evaluates an expression with the local variable bindings of a specific backtrace frame visible, letting the agent inspect the state that caused the error — the equivalent of SLIME's `e` in sldb.
- **Time-box evaluations.** `atelier:eval-form` now honors a `timeout` parameter (seconds). When the timeout fires, the server sends `(:emacs-interrupt)` to the child, forcing it into the debugger. The debug state is returned to the agent, who can abort or inspect. This delivers the deferred AC28 from slice 010.

**What this slice does not ship:**

- **Stepping.** Single-step debugging (`swank:sldb-step`, `swank:sldb-next`) is not exposed. Stepping is inherently interactive at a tempo that doesn't match agentic tool calls. If a story arrives, it belongs in a future slice.
- **Condition signalling from the agent.** The agent cannot signal conditions in the child — only evaluate forms that may signal.
- **Inspector protocol.** SWANK's inspector protocol (structured value inspection with drill-down) is slice 014 scope. Slice 011's `eval-in-frame` is the simpler "eval a string in frame context" primitive.
- **Thread selection.** The debugger operates on the REPL thread. Multi-thread debugging is a future concern.
- **Dead-child auto-restart (AC29).** Remains deferred — the current error handling is sufficient.

## Specification references

- **SWANK debug protocol** — the `:debug`, `:debug-activate`, `:debug-return` message sequence documented in `product/slice/010-editor-foundation-and-mcp-eval/references/swank-protocol.md` and tested in `testsuite/swank/wire-protocol.lisp`. Slice 011's work is replacing the auto-abort handler in `swank-eval` with a state-capture-and-return handler.
- **Python prototype** — `product/slice/009-mcp-skeleton/references/python-prototype-notes.md` §"Useful patterns to revisit": pattern 2 (debugger entry sentinel), pattern 3 (timeout cleanup via `(:emacs-interrupt)` then `swank:throw-to-toplevel`), pattern 4 (debugger state formatting as "level N: condition\\n\\nRestarts:\\n  0: [...]\\nBacktrace:").
- **MCP specification** — `2024-11-05` as pinned by slice 009. No protocol-level changes. New tools are standard `tools/call` handlers.
- **SWANK functions for frame inspection** — `swank:frame-locals-and-catch-tags`, `swank:frame-source-location`, `swank:eval-string-in-frame`. Tactician verifies these exist in the pinned SWANK version during planning.
- **INV-24** — SWANK debug auto-abort lifecycle. Slice 011 replaces the auto-abort with a capture-and-return, but the lifecycle knowledge (original eval's `:return` never arrives after abort) still governs the `invoke-restart` path when the chosen restart is ABORT.

## Stories

### S1 — Debug state exposure from `eval-form`

**In order to** understand what went wrong when a form errors in the child, **an** agent **can** read the structured debug state returned by `atelier:eval-form` instead of receiving a generic "Evaluation aborted" error.

**Acceptance criteria:**

- Given a form that signals a condition in the child (e.g. `(error "something broke")`), when `atelier:eval-form` is called, then the tool response is a JSON object with `in_debugger: true`, `condition` (string — the condition text from SWANK's `:debug` message), `restarts` (array of objects, each with `index` (integer), `name` (string), and `description` (string)), `backtrace` (array of objects, each with `index` (integer) and `description` (string), limited to top 20 frames), and `level` (integer — the SWANK debug level). The `isError` field is **false** — the agent can act on the debug state without the MCP client treating it as a failure.
- Given a form that signals a `simple-error` with format arguments (e.g. `(error "expected ~A got ~A" :foo :bar)`), when the debug state is returned, then the `condition` field contains the fully formatted message (e.g. `"expected FOO got BAR"`), not the format string.
- Given a form that does not error, when `atelier:eval-form` is called, then the response is unchanged from slice 010: `{value: "...", stdout: "...", duration_ms: N}` with no `in_debugger` field. Backward compatibility with slice 010's non-error path is preserved exactly.
- Given the debug state is returned, when the MCP server continues serving, then it tracks the debug state on the session's child connection. Subsequent `atelier:invoke-restart`, `atelier:abort`, `atelier:backtrace`, and `atelier:eval-in-frame` calls operate on this state.
- Given the debug state is returned, when `atelier:eval-form` is called again (a second eval while the debugger is active), then the tool response is an `isError: true` result with a "debugger is active — invoke a restart or abort before evaluating" message. The server does not allow overlapping evals.

### S2 — `atelier:invoke-restart` tool

**In order to** resolve a condition by selecting a specific restart, **an** agent **can** call `atelier:invoke-restart` with a restart index and debug level and receive either the completed eval result or a new debug state.

**Acceptance criteria:**

- Given the tool `atelier:invoke-restart` is registered, when invoked with `index` (integer) and `level` (integer), then the server sends `(:emacs-rex (swank:invoke-nth-restart-for-emacs LEVEL INDEX) ...)` to the child's SWANK connection.
- Given a restart that resolves the condition and allows the original eval to complete, when invoked, then the tool response contains the original eval's result: `{value: "...", stdout: "...", duration_ms: N}`. The debug state is cleared from the session.
- Given a restart that causes another condition (nested debugger entry), when invoked, then the tool response contains a new debug state with an incremented `level`. The agent can invoke restarts at the new level.
- Given a restart index that is out of range, when invoked, then the response is `isError: true` with a "restart index N out of range (0..M)" message.
- Given no debugger is active (no prior error from `eval-form`), when invoked, then the response is `isError: true` with a "no active debugger" message.
- Given the ABORT restart is chosen (typically index 0), when invoked, then the debug state is cleared and the response is `{aborted: true, condition: "..."}` — not a successful eval result, since the eval was abandoned.

### S3 — `atelier:abort` tool

**In order to** quickly abandon a debug session and return to a working state, **an** agent **can** call `atelier:abort` without needing to know the restart index or debug level.

**Acceptance criteria:**

- Given the tool `atelier:abort` is registered, when invoked with no arguments, then the server sends the ABORT restart at the current debug level. Equivalent to `(invoke-restart :index 0 :level <current-level>)`.
- Given a multi-level debugger (error within error handling), when `atelier:abort` is called, then it aborts the **innermost** (highest-level) debugger. If multiple levels remain, the session is still in the debugger at the previous level. The tool response indicates the remaining debug state or that the session is clear.
- Given no debugger is active, when invoked, then the response is `isError: true` with a "no active debugger" message.

### S4 — `atelier:backtrace` tool

**In order to** see the full call stack at the point of error, **an** agent **can** call `atelier:backtrace` and receive the complete backtrace with source locations.

**Acceptance criteria:**

- Given the tool `atelier:backtrace` is registered, when invoked with optional `start` (integer, default 0) and `end` (integer, default 50), then the server sends `(:emacs-rex (swank:backtrace START END) ...)` to the child and returns a JSON array of frame objects: `{index: N, description: "...", source_file: "/path/or/null", source_line: N_or_null}`.
- Given the initial debug state from `eval-form` includes a truncated backtrace (top 20 frames), when `atelier:backtrace :start 20 :end 50` is called, then the agent receives the next 30 frames. This supports pagination.
- Given no debugger is active, when invoked, then the response is `isError: true` with a "no active debugger" message.

### S5 — `atelier:eval-in-frame` tool

**In order to** inspect the local state at a specific point in the call stack, **an** agent **can** call `atelier:eval-in-frame` with an expression and a frame index to evaluate it with that frame's local variables visible.

**Acceptance criteria:**

- Given the tool `atelier:eval-in-frame` is registered, when invoked with `expression` (string), `frame-index` (integer), and optional `package` (string, default `"CL-USER"`), then the server sends `(:emacs-rex (swank:eval-string-in-frame EXPRESSION FRAME-INDEX PACKAGE) ...)` to the child and returns the result as `{value: "..."}`.
- Given a frame with a local variable `x` bound to `42`, when `(atelier:eval-in-frame :expression "x" :frame-index N)` is called, then the result is `{value: "42"}`.
- Given the expression signals a condition in the frame context, when invoked, then the response is `isError: true` with the condition text. The debugger state is preserved (the agent is still at the same level).
- Given a frame index out of range, when invoked, then the response is `isError: true` with a "frame index out of range" message.
- Given no debugger is active, when invoked, then the response is `isError: true` with a "no active debugger" message.

### S6 — `eval-form` timeout via SWANK interrupt (AC28 from slice 010)

**In order to** prevent a runaway eval from blocking the session indefinitely, **an** agent **can** pass a `timeout` parameter to `atelier:eval-form` and receive the debug state when the timeout fires.

**Acceptance criteria:**

- Given `atelier:eval-form` is called with `timeout: 5` (seconds) and a form that runs longer than 5 seconds (e.g. `(loop)`), when the timeout fires, then the server sends `(:emacs-interrupt :repl-thread)` to the child's SWANK connection, the child enters the debugger with a keyboard-interrupt condition, and the tool returns the debug state: `{in_debugger: true, condition: "...interrupt...", restarts: [...], backtrace: [...], level: 1}`.
- Given the timeout fires and the debug state is returned, when the agent calls `atelier:abort`, then the session returns to a working state. The child is alive and responsive to subsequent `eval-form` calls.
- Given `atelier:eval-form` is called with `timeout: 5` and a form that completes in 1 second, when the eval completes before the timeout, then the timeout is cancelled and the response is the normal eval result. No interrupt is sent.
- Given `atelier:eval-form` is called without a `timeout` parameter, when invoked, then the behavior is unchanged from slice 010 (no timeout, eval runs until completion or error). The default is no timeout.

### S7 — Testsuite coverage for debugger tools

**In order to** verify that debugger interactions work end-to-end, **a** developer **can** run the existing `(asdf:test-system "org.melusina.atelier")` and see all debugger-related assertions pass alongside the existing 623 assertions.

**Acceptance criteria:**

- Given `(asdf:test-system "org.melusina.atelier")` is run in a fresh SBCL subprocess, then all existing 623 assertions pass **and** the new debugger assertions pass. No regression.
- Given the debugger tests run against a real child SBCL image (slow tests), then each test that creates a child cleans up via `unwind-protect` / `connection-shutdown`. No orphan SBCL processes after the test suite completes.
- Given the SWANK exploratory test system (`testsuite/swank/`), when extended with new tests for the debug interaction protocol (frame-locals, eval-in-frame, nested debug levels, interrupt), then these tests encode the exact SWANK behavior and serve as documentation. The exploratory tests are not in the main suite but must pass when run explicitly.
- Given the MCP-level debugger tests, when run, then they cover: (a) eval-form returning debug state, (b) invoke-restart resolving the error, (c) invoke-restart causing nested error, (d) abort clearing the session, (e) backtrace pagination, (f) eval-in-frame reading a local variable, (g) timeout firing and returning debug state, (h) eval-form rejection during active debugger.

## Quality Criteria

- [ ] `(asdf:test-system "org.melusina.atelier")` passes in a fresh SBCL subprocess with zero regressions from the 623-assertion baseline.
- [ ] No tool handler can crash the MCP server: every handler is wrapped in a `handler-case` that converts conditions to JSON-RPC error results (same discipline as slices 009–010).
- [ ] No orphan SBCL processes after the test suite completes — verified by the existing orphan-check test.
- [ ] The `eval-form` tool's non-error path is backward-compatible with slice 010: same JSON shape, same field names. Only the error path changes (debug state instead of `isError` string).
- [ ] No new ASDF system dependencies beyond what slice 010 already declares.
- [ ] The SWANK exploratory test system (`testsuite/swank/`) is extended with tests covering the debug interaction lifecycle (not just auto-abort as in slice 010).
- [ ] Timeout on `eval-form` is honored: a form that would run forever is interrupted within `timeout + 2s` and the debug state is returned. The child survives for subsequent calls.
- [ ] The refactoring of `swank-eval` preserves the existing behavior for non-error evals exactly. No change to the happy path.
- [ ] `canonicalize-form`, `list-packages`, `describe-symbol`, and other parent-image tools continue to work while the child is in the debugger (they don't touch the child connection).
- [ ] INV-11 audit: templates under `resource/template/` grepped for any reference to symbols this slice introduces. None expected.

## Definition of Ready

- [x] Stories traceable to backlog items — slice maps to backlog item #7 and delivers deferred AC28 from slice 010
- [x] Stories sized ≤ 2 days each — seven stories, each scoped to one SWANK protocol change or one MCP tool
- [x] Acceptance criteria written for all stories
- [x] Quality criteria defined
- [x] Spec references identified — SWANK debug protocol, Python prototype patterns 2–4, MCP spec, INV-24

## Definition of Done

- [ ] All seven stories complete with acceptance criteria passing
- [ ] Quality criteria passing
- [ ] Full test suite passes in fresh SBCL subprocess (per INV-4)
- [ ] Manual MCP-client smoke test recorded: launch `atelier-mcp`, invoke `atelier:eval-form` with `(error "test")`, observe debug state with restarts, invoke `atelier:invoke-restart` with abort, confirm session is clear, invoke `atelier:eval-form` with `(+ 1 2)` and observe `3`
- [ ] All implementation phases have completion notes
- [ ] `product/slice/011-mcp-debugger-and-restarts/retrospective.md` created
- [ ] `product/backlog.md` updated (item #7 marked delivered; AC28 delivered; AC29 remains deferred)
- [ ] `product/roadmap.md` updated (011 moved to Completed)
- [ ] `CLAUDE.md` updated with a brief debugger section noting the five new tools and the eval-form timeout capability

---

## Notes for the Tactician

This slice is **one logical unit**: refactor the SWANK eval loop + expose debug state + add tools that operate on that state. My working mental model is **one implementation phase**, because:

- The SWANK protocol refactoring (S1, S6) and the tool implementations (S2–S5) are tightly coupled — `invoke-restart` cannot be tested without the debug state exposure from S1.
- The total surface area is smaller than slice 010: no new ASDF system, no new package, no new child process architecture. It's protocol changes + tool handlers, all in existing files.
- Slice 010's two-phase split was driven by the editor/MCP independence boundary. No such boundary exists here.

But the Tactician may disagree. If the step table exceeds ~30 steps, splitting is justified. A natural split point would be:
- **Phase 1:** SWANK protocol refactoring (`swank-eval` capture-and-return, `swank-invoke-restart`, debug state class) + `eval-form` behavior change + `invoke-restart` + `abort`. This is the load-bearing core.
- **Phase 2:** `backtrace` (with source locations) + `eval-in-frame` + timeout/interrupt. These are tools that consume the Phase 1 infrastructure.

### Questions to resolve during the planning interview

1. **Debug state representation.** `defclass debug-state` with slots for condition, restarts, backtrace, level? Or a plain alist/plist returned from `swank-eval`? My lean: a `defclass` because the session needs to track it and tools need to read its slots. But the class lives in `atelier/mcp`, not `atelier/editor` — it has no editor relevance.

2. **Session-level debug state tracking.** Where does the "currently in debugger" state live? Options: (a) a slot on `child-connection`, (b) a special variable on the server, (c) a slot on a `session` object (which doesn't exist yet). My lean: slot on `child-connection` — the debug state is per-child, and the child connection is per-session. No new `session` class.

3. **`swank-eval` refactoring strategy.** The current `swank-eval` is a single function with a `loop` that auto-aborts on `:debug`. Options: (a) add a keyword argument `:on-debug :capture` vs `:on-debug :abort` to control the behavior, keeping one function; (b) extract the message loop into a separate function and have `swank-eval` and `swank-invoke-restart` both call it; (c) make the `:debug` handler a generic function for extensibility. My lean: (b) — a shared `swank-message-loop` that both `swank-eval` and `swank-invoke-restart` call. The loop returns either a result or a debug state. Tactician decides.

4. **SWANK `backtrace` function.** The prototype uses `swank:backtrace`. Verify the exact function name and argument list in the pinned SWANK version. `(swank:backtrace START END)` is the expected signature, returning a list of `(INDEX DESCRIPTION &optional LOCATION)`. Add to SWANK exploratory tests.

5. **SWANK `frame-locals-and-catch-tags` vs `frame-locals-for-emacs`.** SWANK has multiple frame inspection functions. Verify which one returns useful structured data. Add to SWANK exploratory tests before committing to a wire format.

6. **SWANK `eval-string-in-frame`.** Verify exact signature: `(swank:eval-string-in-frame STRING FRAME-INDEX PACKAGE)`. Expected return: a string result. Add to SWANK exploratory tests.

7. **Interrupt mechanism for timeout.** The Python prototype sends `(:emacs-interrupt :repl-thread)`. Verify this works via a SWANK exploratory test: connect, send a long-running eval, send the interrupt, observe `:debug` entry. If `(:emacs-interrupt)` doesn't work from a CL client (recall INV-24: SWANK functions designed for Emacs may not work for CL clients), alternative is `(bt:interrupt-thread child-thread ...)` from the parent. Tactician pins this during planning.

8. **Timeout implementation.** Options: (a) `bordeaux-threads:make-thread` with a sleep that fires the interrupt, cancelled on normal return; (b) SBCL's `sb-ext:with-timeout` in the message loop (not portable); (c) `select`/`poll` on the SWANK socket with a deadline. My lean: (a) — a watchdog thread is the simplest portable approach.

### Risks to surface early

- **R1 — Hallucinated requirements.** Same discipline as slices 009–010. No frame-inspector protocol, no stepping, no thread selection. If a tool is tempting but has no acceptance criterion, it doesn't ship.
- **R2 — SWANK debug protocol surprises.** INV-24 documents one already (original eval's `:return` never arrives after abort). More surprises are likely: nested debug levels, restart side effects, frame-local formats. Mitigation: extend SWANK exploratory tests before writing production code.
- **R3 — `swank-eval` refactoring breaks the happy path.** The current non-error flow works and is tested. The refactoring must not change it. Mitigation: run the full test suite after refactoring, before adding any debugger tool.
- **R4 — Timeout race conditions.** The watchdog thread sends the interrupt, but the eval might complete between the timeout check and the interrupt delivery. Mitigation: the interrupt arriving on a non-running eval is harmless (no `:debug` entry). The message loop must handle "timeout fired but eval already returned" cleanly.
- **R5 — `eval-in-frame` may not be available.** Some SWANK versions restrict frame inspection. If `swank:eval-string-in-frame` is not available or not functional, the story is deferred to a future slice. Mitigation: exploratory test first.
- **R6 — Nested debug levels in tests.** Tests that intentionally trigger nested debugger levels (error within error handler) must clean up all levels on test exit. Mitigation: `unwind-protect` that sends ABORT for each active level.

### Project knowledge to apply

From `product/knowledge/`:

- **`patterns.md` — "SWANK functions designed for Emacs"**: applies directly. Every SWANK function used in this slice (`backtrace`, `frame-locals-and-catch-tags`, `eval-string-in-frame`, `invoke-nth-restart-for-emacs`) must be verified via exploratory test before production use. No exceptions.
- **`patterns.md` — "Exploratory test systems prevent ad-hoc debugging loops"**: the SWANK exploratory test system already exists. Extend it, don't create throwaway scripts.
- **`patterns.md` — "Skim-then-code does not work"**: re-read INV-24, the SWANK protocol reference, and the Python prototype patterns 2–4 before writing any code.
- **`calibration.md`**: slice 010 Phase 2 had 40 assertions (at the low end) for slow tests. Slice 011 is dominated by slow tests (child connection, SWANK debug interactions). Predict 30–60 new assertions. The low end is plausible because debug tests are slow (child spawn + eval + debug entry + restart + verification).
- **`reworks.md`**: three of five reworks in slice 010 Phase 2 were SWANK protocol surprises. Expect at least 2–3 SWANK surprises in slice 011. Budget accordingly.

---

This slice turns the MCP server from a "evaluate and hope" tool into a "evaluate, see what happened, decide what to do" tool. The debugger is the single most important differentiator between a generic Lisp eval endpoint and a development partner. Getting the debug state exposure right (S1) is the load-bearing story — everything else is a tool that reads or acts on that state.
