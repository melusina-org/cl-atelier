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

## Plan pass-count predictions that count testcases instead of assertions

**Discovered:** slice 008, phase 1
**Pattern:** A deletion-slice plan predicts the post-slice pass count by counting removed `define-testcase` forms, treating each as one assertion. Reality: `define-testcase` can contain any number of `assert-*` forms, and the actual pass-count delta is the sum of those assertions across all removed testcases. Slice 008's plan predicted 299 → 296 (delta 3); reality was 299 → 295 (delta 4) because `validate-check-line-length-long` held two assertions.
**Signal:** A plan that counts testcases by name and predicts a pass-count delta equal to the count.
**Mitigation:** When predicting pass-count deltas for test removal, grep for `assert-*` forms *inside* the removed testcases and sum those, not the testcases themselves. Better: predict a *range* and verify the actual number in the Reviewer phase completion audit rather than bake a tight number into an acceptance criterion.
