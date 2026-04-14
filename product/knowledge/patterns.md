# Recurring Risk Patterns and Failure Modes

Patterns observed across slices that are likely to recur. Each entry names a signal the pattern is present and a mitigation that worked (or should be tried).

---

## Stale-fasl masking of load-order bugs

**Discovered:** slice 007, phase 1
**Pattern:** SBCL's `defvar` in one file + first use in another file loaded *earlier* in the ASDF `:serial` order -> at cold compile-file time the symbol is not yet `special`, so `let` bindings that should be dynamic are compiled as lexical. The bug is invisible in the development REPL because the live image's file order rarely matches cold ASDF order.
**Signal:** A test that passes reliably in the REPL but fails on a cold `sbcl --non-interactive` run.
**Mitigation:** Every slice-level verification checkpoint runs in a fresh SBCL subprocess (INV-4). Verify new `defvar`/`defparameter` is defined in the earliest ASDF `:serial` position.

## Documentation count drift from live registry

**Discovered:** slice 008, phase 1
**Pattern:** `CLAUDE.md` claims "N inspectors / M maintainers" but the live registry holds a different number.
**Signal:** A count line in a doc that mentions specific registered symbols, with no mechanical link to the registry.
**Mitigation:** Treat every slice that adds or removes a registry entry as also requiring a count update.

## Hallucinated requirements from surface intuition

**Discovered:** slice 001, phase 1
**Pattern:** A planner reads a phrase like "multi-language linter" and generates plumbing for a non-requirement. The `:languages` slot on `inspector` was carried through design, implementation, and testing before being removed.
**Signal:** A structural feature whose only justification is a phrase in the vision statement.
**Mitigation:** For every slot/field/parameter on a core class, point at the specific acceptance criterion that requires it.

## Wrong dispatch mechanism in first implementation

**Discovered:** slice 002; confirmed slice 004
**Pattern:** When adding a new level to the inspection pipeline, the first implementation wires the new inspector type to the wrong generic function.
**Signal:** A new inspector level that "works" but whose findings all land in a single parent class.
**Mitigation:** Every slice must have an acceptance criterion naming the dispatch mechanism explicitly.

## Bulk text replacement corrupts fixtures that deliberately contain the pattern

**Discovered:** slice 006
**Pattern:** A bulk replacement across the tree catches matches inside test fixtures that deliberately contain the pattern.
**Signal:** A slice doing tree-wide replacement whose pattern plausibly appears in test fixtures.
**Mitigation:** Grep `testsuite/fixtures/` before any bulk pass; review `git diff testsuite/` after.

## Test-registry pollution across test runs

**Discovered:** slice 005
**Pattern:** Tests that exercise `define-inspector` or `define-automatic-maintainer` register real entries in the global hash tables. Subsequent integration tests pick up leftover test fixtures.
**Signal:** An integration test that passes in isolation but fails when run after a registration test.
**Mitigation:** The production code has a `production-resolution-p` filter that excludes maintainers from packages containing `"TEST"`. The hard fix is per-test dynamic binding of the registries.

## Plan pass-count predictions that count testcases instead of assertions

**Discovered:** slice 008, phase 1
**Pattern:** A plan predicts the pass-count delta by counting removed `define-testcase` forms, treating each as one assertion.
**Signal:** A plan that counts testcases by name and predicts a delta equal to the count.
**Mitigation:** Grep for `assert-*` forms inside removed testcases and sum those.
