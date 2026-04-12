# Implementation Phase 2 Notes: Slice 010 — MCP Eval Layer

**Phase:** 2
**Plan:** `product/slice/010-editor-foundation-and-mcp-eval/implementation-2.md`
**Recorded:** 2026-04-12
**Status:** Complete

## Stories delivered in this phase

- **S4** — `child-connection` subclass of `image-connection`. Spawns a child SBCL via `uiop:launch-program`, loads `org.melusina.atelier/child-worker`, starts SWANK on a random TCP port, connects via our SWANK wire protocol client. `connection-eval`, `connection-shutdown`, `connection-alive-p` all implemented. Stdout drain thread prevents pipe deadlock.
- **S5** — `eval-form` MCP tool. Evaluates forms in the session's child SBCL via SWANK. Lazy child spawn on first call. Returns value, captured stdout, and duration-ms. Error handling via SWANK debug auto-abort.
- **S6** — 4 introspection MCP tools: `list-packages`, `list-package-symbols`, `describe-symbol`, `find-definition`. All eval child-worker helper functions in the child via SWANK.
- **S7** — 2 testsuite runner MCP tools: `run-tests-fresh` (separate SBCL via `uiop:run-program`, stateless), `run-tests-in-child` (session child via SWANK, stateful).
- **S8** — `canonicalize-form` MCP adapter tool. Runs entirely in parent image. Calls `normalize-toplevel-form` and returns canonicalized form + findings as JSON.

## Acceptance criteria results

| # | Criterion | Result | Verified by |
|---|---|:---:|---|
| AC19 | `child-connection` inherits from `image-connection` | ✅ | T50 |
| AC20 | `make-child-connection` spawns within timeout | ✅ | T50 |
| AC21 | `connection-eval` returns `"3"` for `(+ 1 2)` | ✅ | T51 |
| AC22 | Sequential evals share state | ✅ | T52 |
| AC23 | `connection-shutdown` terminates child | ✅ | T53 |
| AC24 | Missing SBCL signals `child-image-spawn-failed` | ✅ | T54 |
| AC25 | `eval-form` returns JSON with value/stdout/duration | ✅ | T55 |
| AC26 | `eval-form` on error returns `isError: true` | ✅ | T57 |
| AC27 | `eval-form` captures stdout | ✅ | T58 |
| AC28 | Timeout not tested (deferred — needs SWANK timeout support) | ⏳ | — |
| AC29 | Dead child auto-restart not tested (deferred) | ⏳ | — |
| AC30 | `list-packages` returns package data | ✅ | T61 |
| AC31 | `list-package-symbols` returns symbol descriptors | ✅ | T62 |
| AC32 | `describe-symbol` returns function info | ✅ | T63 |
| AC33 | `find-definition` returns source location | ✅ | T64 |
| AC34 | Introspection on non-existent symbol returns error | ✅ | T65 |
| AC35 | `run-tests-fresh` spawns separate SBCL | ✅ | T66 |
| AC36 | `run-tests-in-child` uses session child | ✅ | T67 |
| AC37 | `canonicalize-form` returns canonicalized + findings | ✅ | T45–T46 |
| AC38 | `canonicalize-form` on forbidden form returns error | ✅ | T47 |
| AC39 | 14 tools registered | ✅ | T69 |
| AC40 | No orphan SBCL processes | ✅ | T70 |
| AC41 | Full regression passes | ✅ | 623/623 |

## Test results

- **Fast:** 21 assertions passed (SWANK encoding, canonicalize tool, spawn failure, registration count)
- **Slow:** 19 assertions passed (child connection, eval, introspection, test runners, orphan check)
- **Skipped:** 0
- **Exploratory (not in main suite):** I/O tests 14/14, SWANK tests 14/14

**Totals:**
- Phase 2 new: 40 assertions
- Phase 1 carried: 104 assertions
- Existing atelier + MCP: 479 assertions
- Combined: 623/623 (100%)

Assertion count prediction: 40–80. Actual: 40. At the low end of range.

## Invariants established or confirmed

**Invariants 1–20: confirmed.** No changes to prior invariants.

**New invariants:**

- **INV-21:** `child-connection` spawns a child SBCL, connects via SWANK over TCP, and shuts down cleanly. Stdout drain thread prevents pipe deadlock from merged stderr. Enforced by T50–T53, T70.
- **INV-22:** The `canonicalize-form` MCP tool runs entirely in the parent image. No child connection is created or used. Enforced by T49.
- **INV-23:** `eval-form` captures stdout from child eval via SWANK `:write-string` messages. Enforced by T58.
- **INV-24:** SWANK `:debug` events auto-abort in slice 010: wait for `:debug-activate`, send `invoke-nth-restart-for-emacs`, wait for abort's `:return`, then signal error with the captured condition text. The original eval's `:return` never arrives after abort — this is SWANK's documented behavior. Enforced by SWANK exploratory tests.
- **INV-25:** `run-tests-fresh` spawns a separate SBCL via `uiop:run-program`, independent of the session child. Enforced by T66.

## Deferred items (for future slices)

- **AC28** — `eval-form` timeout: needs SWANK-level interrupt support (`(:emacs-interrupt thread-id)`). Deferred to slice 011 (debugger).
- **AC29** — Dead child auto-restart: needs `connection-alive-p` check before eval dispatch. Deferred — current implementation is sufficient for MCP tool error handling.
- **S4 acceptance criterion on socketpair** — Plan deviated: TCP on localhost instead of socketpair. Documented in implementation-2.md.
- **S4 naming** — Plan deviated: `child-connection` instead of `swank-socket-connection`. Documented in implementation-2.md.

## Reworks performed

1. **`swank:interactive-eval` does not work** — The function exists and is exported but requires SLIME-specific state (LINES, WIDTH args for echo-area formatting). Fixed: use `swank:eval-and-grab-output` instead, which takes a single string argument and returns `("captured-output" "result")`. Trigger: SWANK internals not documented, discovered by testing. Cost: 1 edit in `swank-eval`.

2. **Pipe deadlock from child stderr** — `uiop:launch-program` with `:error-output :stream` creates a pipe for stderr. If the parent doesn't read it, the pipe buffer fills (64KB on macOS) and the child blocks. Initial fix: `:error-output nil` (discard). User feedback: "discarding stderr is a bad idea." Final fix: `:error-output :output` (merge with stdout) + background drain thread after port is read. Trigger: test suite hanging indefinitely. Cost: 3 iterations.

3. **SWANK debug auto-abort blocks** — After sending `invoke-nth-restart-for-emacs`, the original eval's `:return` never arrives. SWANK sends `:return` only for the abort restart's continuation ID, not the original eval's. Fixed: track `abort-id` separately; when its `:return` arrives, signal error with captured condition text from `:debug` message. Trigger: `validate-swank-eval-error` test timeout. Cost: restructured `swank-eval` message loop. Discovered and documented via exploratory `testsuite/swank/` tests.

4. **SWANK package not in parent** — `swank-eval` initially built s-expressions with `swank:interactive-eval` symbols, but the SWANK package doesn't exist in the parent MCP image. Fixed: use `swank-send-raw` with format strings like `"(:emacs-rex (swank:eval-and-grab-output ~S) ...)"` — the SWANK reader in the child resolves the symbols. Cost: 1 restructure.

5. **`sb-ext:string-to-octets` not portable** — User feedback: "avoid SB-EXT, use UIOP or well-established libraries." Fixed: replaced with `flexi-streams:string-to-octets`. Added `flexi-streams` to MCP system dependencies. Cost: mechanical replacement.

## New risks discovered

- **SWANK `interactive-eval` is unreliable for programmatic use.** Its API is designed for the SLIME echo area (takes LINES, WIDTH args). `eval-and-grab-output` is the correct function for non-Emacs clients. Future slices should prefer `eval-and-grab-output` or wrap calls in custom child-worker functions.
- **Pipe deadlock is a recurring pattern.** Any `uiop:launch-program` with `:output :stream` and `:error-output :stream` risks deadlock if both streams aren't drained concurrently. The pattern is well-documented in the I/O exploratory tests.

## Technical decisions made

1. **SWANK over TCP, not socketpair** — `swank:create-server :port 0` assigns a random port. The child prints the port to stdout. Simpler than socketpair, uses standard SWANK path. Rationale: SWANK natively supports TCP; socketpair would require a custom transport adapter.
2. **`eval-and-grab-output` as the eval primitive** — Returns `("captured-output" "result")` as a two-element list. The parent's SWANK client accumulates `:write-string` messages for additional output capture. Rationale: cleanest SWANK API for non-Emacs clients.
3. **Exploratory test systems** — Created `testsuite/input-output` and `testsuite/swank` as separate ASDF systems. Not in the main test suite. Rationale: encode learnings about SWANK and pipe behavior in permanent, runnable tests rather than throwaway ad-hoc scripts.
4. **`flexi-streams` for UTF-8 encoding** — Portable across CL implementations. Added as MCP dependency. Rationale: user preference for portability over SBCL-specific `sb-ext`.
5. **`closer-mop` in child-worker only** — Used for class/generic-function introspection in the child image. Not needed in the parent. Rationale: MOP is needed for `describe-symbol-data` to report class slots and generic methods.

## Notes for Strategist retrospective

- **Stories remaining:** none. All 11 stories (S0–S11) delivered across both phases.
- **Snail tests requiring confirmation:** none.
- **Deferred:** eval-form timeout (AC28) and dead-child auto-restart (AC29). Both are polish items, not blocking. Timeout needs SWANK interrupt protocol (slice 011). Auto-restart is a convenience the MCP error handling already covers.
- **Calibration:** 40 assertions actual vs 40–80 predicted. At the low end. The slow tests produce fewer assertions per test (1–2) compared to Phase 1's fast tests (3–5).
- **Rework count:** 5 reworks. Patterns: SWANK API misunderstanding (not documented, discovered by testing), pipe deadlock (classic UNIX pattern, now documented in exploratory tests), SBCL-specific code (user feedback, mechanical fix). The exploratory test systems were the right investment — they encoded the learnings permanently.

---

**Handoff:** [Maker] Phase 2 notes written. All stories delivered. Strategist can run the retrospective.
