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

## Plan pass-count predictions that count testcases instead of assertions

**Discovered:** slice 008, phase 1
**Pattern:** A deletion-slice plan predicts the post-slice pass count by counting removed `define-testcase` forms, treating each as one assertion. Reality: `define-testcase` can contain any number of `assert-*` forms, and the actual pass-count delta is the sum of those assertions across all removed testcases. Slice 008's plan predicted 299 → 296 (delta 3); reality was 299 → 295 (delta 4) because `validate-check-line-length-long` held two assertions.
**Signal:** A plan that counts testcases by name and predicts a pass-count delta equal to the count.
**Mitigation:** When predicting pass-count deltas for test removal, grep for `assert-*` forms *inside* the removed testcases and sum those, not the testcases themselves. Better: predict a *range* and verify the actual number in the Reviewer phase completion audit rather than bake a tight number into an acceptance criterion.
