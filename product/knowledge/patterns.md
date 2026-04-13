# Recurring Risk Patterns and Failure Modes

Patterns observed across slices that are likely to recur. Each entry names a signal the pattern is present and a mitigation that worked (or should be tried).

---

## Stale-fasl masking of load-order bugs

**Discovered:** slice 007, phase 1
**Pattern:** SBCL's `defvar` in one file + first use in another file loaded *earlier* in the ASDF `:serial` order → at cold compile-file time the symbol is not yet `special`, so `let` bindings that should be dynamic are compiled as lexical. The bug is invisible in the development REPL because the live image's file order rarely matches cold ASDF order; every session succeeds, and the regression test has been running against stale fasls that happened to work.
**Signal:** A test that passes reliably in the REPL but fails on a cold `sbcl --non-interactive` run, with a symptom like a special variable appearing `NIL` inside a `let` that clearly binds it.
**Mitigation:** Every slice-level verification checkpoint runs in a fresh SBCL subprocess (INV-4). Additionally, any time a new `defvar` / `defparameter` is introduced, verify it is defined in the *earliest* ASDF `:serial` position where any file reads or binds the symbol.

## Documentation count drift from live registry

**Discovered:** slice 008, phase 1
**Pattern:** `CLAUDE.md` (and similar count-stating docs) claim "N inspectors / M maintainers" but the live registry holds a different number. Happens when a new registry entry is added without updating the enumeration in the doc. Drift is silent because nothing verifies the two match. Slice 008 found this accidentally: pre-slice CLAUDE.md said "10 maintainers" while `(length (atelier:list-maintainers))` returned 11 — the `fix-line-too-long` stub was registered but not listed.
**Signal:** A count line in a doc that mentions specific registered symbols, with no mechanical link to the registry.
**Mitigation:** Add a regression assertion that compares the numbers stated in `CLAUDE.md` to the live `(length (list-*))` values. Until that assertion exists, treat every slice that adds or removes a registry entry as also requiring a `CLAUDE.md` count update, and grep for the stated number as part of the pre-commit audit.

## Hallucinated requirements from surface intuition

**Discovered:** slice 001, phase 1 (inspector `:languages` slot); re-seen later
**Pattern:** A planner — human or Claude — reads a phrase like "multi-language linter" and generates plumbing for a non-requirement. Slice 001 added a `:languages` slot to `inspector` because Atelier's vision mentions multiple languages, even though language dispatch belongs in the *runner* (one inspector per language, or a language-agnostic check), not on the inspector itself. The slot was carried through design, implementation, and testing before being removed in rework. Effort cost was real but not catastrophic because the slot was orthogonal to the finding/resolution protocol.
**Signal:** A structural feature added to a core type whose only justification is a phrase in the vision statement, with no concrete story that needs it. If you cannot write a test that fails without the feature, the feature is a hallucination.
**Mitigation:** In the planning interview, for every slot/field/parameter added to a core class, the Tactician must point at the specific acceptance criterion or story that requires it. If there is none, drop it. The acceptance criterion for removal is "every slot on the core type is justified by a story in this slice or a prior slice."

## Wrong dispatch mechanism in first implementation, caught by concrete-dispatch acceptance criteria

**Discovered:** slice 002 (finding subclasses); confirmed slice 004 (`define-syntax-inspector`)
**Pattern:** When adding a new level to the inspection pipeline, the first implementation often wires the new inspector type to the *wrong* generic function. Slice 002's first cut had both file inspectors emitting plain `file-finding` instances, bypassing the CLOS dispatch chain. Slice 004's first cut had `define-syntax-inspector` generating `inspect-file` methods (copied from a stub) instead of `inspect-syntax` methods. In both cases the code *compiled and ran*; the bug was silent.
**Signal:** A new inspector level that "works" but whose findings all land in a single parent class, or whose methods are attached to the wrong generic. Usually visible in the test suite as "assertions pass but the finding count or the finding type is not what the slice predicted."
**Mitigation:** Every slice that introduces a new inspector level must have an acceptance criterion that names the dispatch mechanism explicitly — not just "inspector runs" but "`(find-method #'inspect-syntax ...)` returns a method on the new inspector class, and the finding produced is a direct subclass of the specific finding type defined in this slice, not the parent category class." Slice 004's catch proved this criterion wording is worth the boilerplate.

## Bulk text replacement corrupts fixtures that deliberately contain the pattern

**Discovered:** slice 006 (SPDX header replacement)
**Pattern:** A bulk `sed`-like replacement across the entire tree catches every match, including matches inside test fixtures that deliberately contain the pattern being replaced. Slice 006's SPDX header simplification pass rewrote 90 source files from a 4-line verbose MIT block to a single `SPDX-License-Identifier: MIT` line — and also corrupted `testsuite/fixtures/inspector/check-spdx-license-header/missing-spdx.lisp` and `valid-with-spdx.lisp`, which contained the verbose block *as test data*. Fixed by restoring those two files from git after the bulk pass.
**Signal:** A slice that does a tree-wide replacement whose pattern plausibly appears in test fixtures for the very inspector being replaced. Look for directories named after the inspector whose payload is exactly the pattern.
**Mitigation:** Before any tree-wide replacement, grep for the search pattern under `testsuite/fixtures/` and explicitly exclude those paths from the replacement. If exclusion is hard, do the bulk pass on a clean working tree, then run `git diff testsuite/fixtures/` and `git checkout` any unintended hits. A post-bulk regression run in a fresh SBCL subprocess will flag most but not all corruptions.

## Test-registry pollution across test runs

**Discovered:** slice 005 (autofix pipeline)
**Pattern:** Tests that exercise `define-inspector` or `define-automatic-maintainer` register real entries in the global `*inspectors*` / `*maintainers*` hash tables. Those entries persist across test runs inside a single SBCL image, and subsequent integration tests that iterate the registry (e.g., `lint-system :autofix t`) pick up the leftover test fixtures and try to run them as production maintainers. The symptom is test output that changes depending on the order tests are run, or extra resolutions appearing on integration runs.
**Signal:** An integration test that passes in isolation but fails when run after a maintainer-registration test; or test output whose finding count grows across repeated runs in the same image.
**Mitigation:** The production code has a `production-resolution-p` filter that excludes maintainers from packages whose name contains `"TEST"` — this is a soft mitigation, not a fix. The hard fix would be a per-test dynamic binding of `*inspectors*` / `*maintainers*` for any test that registers an entry, so that the global registry is untouched. This is tracked as design debt. Until then, every new registration-touching test must either use a package name containing `"TEST"` or rebind the registry.

## Skim-then-code does not work for documented CL surprises

**Discovered:** slice 009, phase 1
**Pattern:** Nine reworks during Phase 2 execution; five of them were CL-language surprises documented in `MEMORY.md` or in the Common Lisp skill's reference files (jzon's `nil`/`false`/`null` overloading, Confidence's strict `assert-t` versus `assert-t*`, plist reversal via `nreverse` producing `(value key value key)`, `defclass` rejecting initargs on classes without the slot, and `prin1` case on keywords). In every case the knowledge existed; I had skimmed it but not internalised it, and the Maker loop caught each issue within one test cycle. The fixes were small (one edit each) but the cumulative cognitive cost was substantial.
**Signal:** A Maker entering Phase 2 with "I'll refer to MEMORY.md as needed" rather than "I've re-read it." If you cannot name the three CL surprises in this slice's stack that your code is likely to touch, you haven't re-read the knowledge.
**Mitigation:** Before Phase 2 begins, the Maker re-reads (not just skims) `MEMORY.md` and the relevant entries in `~/.claude/skills/common-lisp/references/` for every library the slice depends on. For slice 009 that would have meant re-reading the jzon notes (but the jzon reference didn't exist yet, it was slice-009's own Step 0 output) and the Confidence reference (which documents `assert-t` vs `assert-t*`). Codify as an explicit Maker-protocol step: "Before step 1, read `product/knowledge/MEMORY.md` and every `references/<library>.md` for the slice's dependencies. Not skim — read."

## Append-not-rewrite amendment protocol for mid-execution plan deviations

**Discovered:** slice 009, phase 1 (Plan Amendment 1 — `resources/list`/`resources/templates/list` split)
**Pattern:** During step 2 of a 55-step plan, a web fetch of the MCP spec revealed that concrete and templated resources are listed via two distinct methods, not one. The plan had assumed a single `resources/list` method. The deviation was material (one new class, one new dispatcher method, one new fixture, adjusted acceptance criterion) but the implementation had not started. Two choices: rewrite the plan to pretend the spec said what it actually said, or append an "Amendment 1" section to the plan with trigger + impact + exact list of changes. The second option preserves the plan's narrative as a durable record of the Tactician's pre-execution understanding, and makes the Reviewer's audit (at phase closure) a clean "did the amendment's listed changes happen?" check.
**Signal:** During Phase 2 execution, the Maker discovers that the plan's assumption about an external spec, API, or library is wrong in a way that affects acceptance criteria. The plan was not wrong *in the author's knowledge at the time*; the author simply did not fetch the authoritative source.
**Mitigation:** The Maker stops, writes an "Amendment N" section appended to the bottom of `implementation-N.md`, with (a) the trigger (what was fetched or discovered), (b) the impact (what ACs or step table entries are affected), (c) the exact list of changes (classes/methods/tests/fixtures added or modified), (d) a "what does NOT change" section to bound the amendment's scope, and (e) a note to the Reviewer explaining how to audit the amendment at phase closure. The original plan text stays intact. This is the first time the workflow was used in Atelier; it worked cleanly.

## Plan pass-count predictions that count testcases instead of assertions

**Discovered:** slice 008, phase 1
**Pattern:** A deletion-slice plan predicts the post-slice pass count by counting removed `define-testcase` forms, treating each as one assertion. Reality: `define-testcase` can contain any number of `assert-*` forms, and the actual pass-count delta is the sum of those assertions across all removed testcases. Slice 008's plan predicted 299 → 296 (delta 3); reality was 299 → 295 (delta 4) because `validate-check-line-length-long` held two assertions.
**Signal:** A plan that counts testcases by name and predicts a pass-count delta equal to the count.
**Mitigation:** When predicting pass-count deltas for test removal, grep for `assert-*` forms *inside* the removed testcases and sum those, not the testcases themselves. Better: predict a *range* and verify the actual number in the Reviewer phase completion audit rather than bake a tight number into an acceptance criterion.

## Pipe deadlock when spawning child processes

**Discovered:** slice 010, phase 2
**Pattern:** `uiop:launch-program` with `:output :stream` and `:error-output :stream` creates two pipes. If the parent reads only stdout (e.g., waiting for a port number) and the child writes extensively to stderr (compilation messages, ASDF output), the stderr pipe buffer fills (~64KB on macOS) and the child blocks. The parent is waiting for stdout data that the child can never produce because it's blocked on stderr. Classic pipe deadlock.
**Signal:** A `make-child-connection` or similar process-spawning function that reads from one stream while ignoring the other. Especially likely when the child compiles systems (produces many stderr lines).
**Mitigation:** Either merge stderr into stdout (`:error-output :output`) and drain the combined stream after reading what you need, or spawn a background thread to drain the unused stream. The I/O exploratory tests in `testsuite/input-output/pipe-behavior.lisp` encode the exact behavior.

## SWANK functions designed for Emacs may not work for CL clients

**Discovered:** slice 010, phase 2
**Pattern:** SWANK exports functions like `interactive-eval` that appear general-purpose but depend on Emacs-specific state (echo-area line count, buffer width) or have undocumented requirements (`*buffer-package*` must be bound). Using them from a CL client produces debugger entries or wrong results. The reliable function for programmatic eval is `swank:eval-and-grab-output` (takes a single string, returns `("captured-output" "result")`). The debug protocol also has a non-obvious lifecycle: after invoking an abort restart, the original eval's `:return` never arrives — only the abort restart's `:return` does.
**Signal:** Using a SWANK function without first verifying its behavior via an exploratory test against a real SWANK server.
**Mitigation:** Create exploratory tests (like `testsuite/swank/wire-protocol.lisp`) that verify each SWANK function's exact behavior before using it in production code. Prefer `eval-and-grab-output` over `interactive-eval` for programmatic clients.

## Exploratory test systems prevent ad-hoc debugging loops

**Discovered:** slice 010, phase 2
**Pattern:** When integrating with an external system (SWANK, UNIX pipes), the natural impulse is to write throwaway ad-hoc test scripts in `/tmp/`. Each iteration produces knowledge that is immediately lost. The knowledge gap resurfaces the next time the code is touched. Slice 010 spent ~3 iterations debugging pipe deadlock and ~3 iterations debugging SWANK debug handling via throwaway scripts before creating the exploratory test systems, which immediately exposed both root causes in a single run.
**Signal:** More than one `/tmp/test-*.lisp` file for the same subsystem in the same session.
**Mitigation:** After the first ad-hoc test, create a permanent exploratory test system (`org.melusina.atelier/testsuite/<topic>`). Write assertions about the external system's behavior, not about your code. The tests encode learning, not correctness.

## SWANK debug requests require the debug thread ID

**Discovered:** slice 011, phase 1
**Pattern:** SWANK dispatches `:emacs-rex` requests per-thread. During debugging, the debug thread is blocked in the debugger loop. Requests sent with `t` (the REPL thread) or `:repl-thread` do not reach the debugger context — they hang silently. The correct thread ID is the second element of the `:debug` message. This applies to `backtrace`, `invoke-nth-restart-for-emacs`, and `eval-string-in-frame`.
**Signal:** Any SWANK request that hangs during an active debug session.
**Mitigation:** Always capture the thread ID from the `:debug` message and pass it via the `:emacs-rex` envelope's thread parameter. Never use `t` for debug-context operations.

## SWANK re-enters debugger after abort restart

**Discovered:** slice 011, phase 1
**Pattern:** After sending `invoke-nth-restart-for-emacs` with the ABORT restart (index 0), SWANK sends three additional messages: `(:debug-return THREAD LEVEL)`, `(:debug THREAD LEVEL ...)`, `(:debug-activate THREAD LEVEL)`. The REPL loop catches the abort and re-enters the debugger. Without draining these messages, the next `swank-eval` reads stale data and hangs. The fix is `swank:throw-to-toplevel` (0 arguments, dispatched on the debug thread) which forces return to the REPL loop.
**Signal:** Second `swank-eval` after an abort cycle hangs.
**Mitigation:** After receiving the `:return` for an abort restart, drain pending messages with a short timeout. If `:debug-activate` arrives, send `(swank:throw-to-toplevel)` on the debug thread and drain its response.

## CL package lock on define-tool names

**Discovered:** slice 011, phase 1
**Pattern:** The `define-tool` macro interns `NAME-TOOL` as a class in the current package. If `NAME` shadows a symbol exported from `COMMON-LISP` (like `invoke-restart`, `abort`, `restart`, `continue`), SBCL raises a package lock violation at compile time. The fix is to use a name that doesn't conflict: `select-restart` instead of `invoke-restart`, `abort-debug` instead of `abort`.
**Signal:** `COMPILE-FILE-ERROR` mentioning "Lock on package COMMON-LISP violated when interning NAME-TOOL."
**Mitigation:** Before naming a tool, check if the name shadows a CL export: `(find-symbol "NAME" :cl)`. If it does, choose an alternative.

## Pure tools pattern: child-worker function + thin MCP wrapper

**Discovered:** slice 010, phase 2; confirmed slice 012, phase 1
**Pattern:** The reliable pattern for MCP tools that operate in the child image is: (1) write a function in `atelier/child-worker` that returns an alist, (2) write a `define-tool` wrapper that calls `connection-eval` with a format string invoking the function, (3) `read-from-string` the result. This pattern produces zero SWANK surprises because the only SWANK interaction is `eval-and-grab-output` on a single form. All 5 slice-012 tools used this pattern with zero reworks. Compare with slice 011's 6 reworks from direct SWANK protocol interactions.
**Signal:** A new tool that needs to run code in the child image.
**Mitigation:** Default to the child-worker pattern. Only use direct SWANK protocol for operations that require SWANK-specific state (debugger, interrupt, frame inspection).
