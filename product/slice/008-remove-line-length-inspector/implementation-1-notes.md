# Implementation Phase 1 Notes: Slice 008 — Remove line-length inspector

**Phase:** 1 (single-phase slice)
**Plan:** `product/slice/008-remove-line-length-inspector/implementation-1.md`
**Recorded:** 2026-04-09
**Status:** Complete

## Stories delivered in this phase

- **S1 — Remove the inspector and its finding class** — all acceptance criteria passed. `src/inspectors/check-line-length.lisp` deleted, `line-too-long-finding` removed from `src/finding.lisp`, `check-line-length` and `line-too-long-finding` unexported from `src/package.lisp`, ASDF main system updated. `(find-symbol "CHECK-LINE-LENGTH" :atelier)` returns NIL.
- **S2 — Remove the maintainer** — all acceptance criteria passed. `src/maintainers/fix-line-too-long.lisp` deleted, `fix-line-too-long` unexported, ASDF main system updated. `(atelier:list-maintainers)` returns 10 entries, `fix-line-too-long` not among them.
- **S3 — Remove the tests and fixtures** — all acceptance criteria passed. `test/inspectors/check-line-length.lisp` deleted, `(testsuite-check-line-length)` call removed from `test/entrypoint.lisp`, all 20 fixtures under `test/fixtures/autofix/fix-line-too-long/` deleted along with the directory itself, `test/utilities.lisp` docstring updated.
- **S4 — Update documentation and project metadata** — all acceptance criteria passed. `CLAUDE.md` (inspector count and Line inspectors list), `README.md` (inspector bullet and `:disabled-inspectors` example), `product/backlog.md` (item 16 reworded + revision history entry), `product/roadmap.md` (Later row removed + two revision history entries) all updated.

## Acceptance criteria results

| # | Criterion | Result | Verified by |
|---|-----------|:------:|-------------|
| 1 | `(asdf:load-system "org.melusina.atelier")` succeeds in fresh SBCL with zero undefined-symbol warnings | ✓ | Final regression step 23: exit 0, grep for undefined-* warnings empty |
| 2 | `(asdf:test-system "org.melusina.atelier")` runs with zero failures | ✓ | `Failure: 0/295` |
| 3 | Pass count decreases by exactly 3 from 299 baseline → 296 | ✗→✓ | **Actual: 299 → 295 (delta 4).** The plan miscounted assertions: `validate-check-line-length-long` contains two assertions, not one. The criterion's *intent* ("pass count decreases by exactly the number of assertions in the removed testcases, no other tests affected") is met. Strategist should note the correction when updating the calibration record. |
| 4 | `(find-symbol "CHECK-LINE-LENGTH" :atelier)` returns NIL | ✓ | Step 18: `CHECK-LINE-LENGTH=NIL LINE-TOO-LONG-FINDING=NIL FIX-LINE-TOO-LONG=NIL` |
| 5 | Inspector and maintainer list lengths each decreased by 1 | ✓ | Step 18: `INSPECTORS=15 MAINTAINERS=10` |
| 6 | Grep audit across code and user-facing docs returns zero matches | ✓ | Step 24 |
| 7 | `test/fixtures/autofix/fix-line-too-long/` does not exist | ✓ | Step 6 `git rm -rf` |
| 8 | CLAUDE.md line 9 reads "15 inspectors, 10 automatic maintainers"; line 88 clean | ✓ | Step 19 |
| 9 | README.md contains no `check-line-length` | ✓ | Step 20 |

## Test results

- Fast: 295 passed, 0 failed (confidence assertions run in-memory).
- Slow: Full regression in a fresh SBCL subprocess — 295/295, exit 0. Four intermediate regression checkpoints (steps 5, 8, 12, 17) all green at 295.
- Snail: N/A for this slice.

Pre-slice baseline was 299 (slice 007 retrospective). Post-slice: 295. The delta is 4, not 3 as the plan predicted.

## Invariants established or confirmed

- **I7 (new)** — Atelier does not police line length. The pretty-printer's `*print-right-margin*` is the only mechanism that influences line length, and it is advisory. A form that remains long after pretty-printing is accepted as-is. This invariant is established by the deletions themselves; the Reviewer will transcribe it into `product/knowledge/invariants.md` during slice closure.
- **I1–I6** — Carried forward from slice 007, all confirmed. The fresh-SBCL-subprocess verification discipline (I6) was applied at every regression checkpoint.

## Deferred items (for next phase)

None. Single-phase slice, all stories delivered.

## Reworks performed

None. Every step in the plan executed on first attempt with the exception of two `Edit` tool calls on `org.melusina.atelier.asd` whose `old_string` did not match because the plan's indentation did not account for the file using tab + space indentation differently in main vs testsuite modules. Retried with correctly-copied whitespace from a `Read` call and succeeded. No source-level rework.

## New risks discovered

- **Plan assertion-count miscount.** The plan predicted 299 → 296 (delta 3), treating each removed `define-testcase` as one assertion. Reality: `define-testcase` forms can contain multiple assertions, and the three removed testcases collectively held four assertions. Delta was 4, final count 295. Low impact — the test math still works out (no spurious failures or extra losses) — but future deletion slices should count assertions, not testcases, when predicting pass counts. Suggested calibration entry: *"When predicting regression pass-count deltas for test removal, grep for `assert-*` forms inside the removed testcases, don't count the testcases themselves."*
- **CLAUDE.md maintainer count was an under-count before this slice.** CLAUDE.md stated "10 automatic maintainers" but the actual count was 11 (the `fix-line-too-long` stub was in the registry but not listed in the enumeration). This slice makes the stated number correct without requiring an explicit correction to the count line. Suggested pattern entry: *"CLAUDE.md count lines can drift from reality when a registry entry is added without updating the documentation. A future slice could add a test that asserts `(length (list-inspectors))` and `(length (list-maintainers))` match the numbers stated in CLAUDE.md, catching drift mechanically."*

## Technical decisions made

- **Risk 3 resolved in favour of keeping the skip logic.** `test/utilities.lisp`'s `discover-autofix-cycle-fixtures` uses a generic `find-symbol` filter at line 105, not a hard-coded check for `fix-line-too-long`. The skip logic is a legitimate safety net for future in-progress directories, so only the docstring was updated to remove the obsolete reference. The code itself is unchanged.
- **`*default-maximum-line-length*` was not exported.** Step 16 checked the package file and confirmed it was internal, so no export removal was needed.

## Notes for Strategist retrospective

- Stories remaining: none.
- Snail tests requiring confirmation: none.
- Recommended for the retrospective:
  - The pass-count prediction error is a calibration data point: one testcase removal produced a pass-count delta of ~1.3 per testcase, not 1.0. Small sample, but worth recording.
  - The CLAUDE.md maintainer under-count is a discovery-mode finding: documentation drift was masked because the slice's own work corrected it accidentally. The retrospective might propose an assertion-in-CI that keeps CLAUDE.md counts in sync with `(list-*)` lengths.
  - The gofmt-position adoption is a *principle change*, not just a scope reduction. Slice 008's invariant I7 should probably be promoted to a design principle in `product/definition.md` on the retrospective pass, alongside the idempotency principle added in slice 007.
