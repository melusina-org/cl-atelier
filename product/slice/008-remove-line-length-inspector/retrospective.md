# Retrospective: Slice 008 — Remove line-length inspector

**Recorded:** 2026-04-09
**Implementation phases:** 1 total — `implementation-1.md`

## Delivery Summary

- Stories delivered: 4 of 4 (S1 inspector+finding removal, S2 maintainer removal, S3 tests+fixtures removal, S4 documentation updates).
- Acceptance criteria: 8 of 9 pass literally; AC3 ("pass count = 296") fails literally but passes in spirit — actual is 295 because the plan counted testcases, not assertions. No re-work required, only a calibration note.
- Quality criteria: all four passing. Zero failures, zero new skips, clean grep audit, fresh-SBCL subprocess regression green.
- Full test suite: **295/295** in fresh SBCL subprocess, exit 0, no undefined-symbol warnings.

## Phase Notes Summary

| Phase | Key deliverables | Deferred to backlog |
|-------|------------------|---------------------|
| 1 | `check-line-length`, `fix-line-too-long`, `line-too-long-finding`, 3 testcases, 20 fixtures, 3 package exports, 3 ASDF components, and 5 documentation mentions all removed in one 24-step pass. Invariant I7 established. | None. Single-phase slice. |

## Goal Progress

- **G2 (Linter capability)** — no quantitative change, but a principled contraction. The linter shrinks from 16 inspectors to 15, but the remaining 15 are *all defensible*: each either does something the pretty-printer cannot do, or wraps an external tool, or enforces a structural invariant. The removed inspector was in the residual category "a diagnostic without a credible automatic fix," and the slice closes that category.
- **No goal moves to Reached.** This is a scope-cleanup slice, not a capability delivery.

## What we learned

**Deletion slices are a legitimate delivery shape, and this codebase tolerates them cleanly.** The 24-step plan executed with zero rework at the source level. Two `Edit`-tool whitespace mismatches on the ASDF file were the only friction, and both resolved on the first retry after a quick `Read`. The fresh-SBCL-subprocess discipline carried over from slice 007 ran at four intermediate checkpoints plus the final run; every checkpoint was green. The slice 007 *"all prior slice regressions had been running against stale fasls"* lesson is now baked into the working rhythm — not just the plan.

**The gofmt position is an honest simplification, and the research made that visible.** The research pass (`product/reference/line-length-research.md`) surveyed ESLint, Ruff, Black, Prettier, gofmt, rustfmt, and clang-format. The decisive finding was Black's explicit escape hatch — *"auto-formatted code will exceed your allotted limit"* in rare cases — which told us that even the most principled structural formatter cannot guarantee line-length satisfaction. That made the choice between "ship a best-effort formatter pass" (shape B) and "refuse the problem entirely" (shape A) much easier: shape A is *not* a cop-out, it is the position Go took, with documented rationale, for a decade. Atelier is smaller than Go and has the Oppen pretty-printer as its *actual* canonical-text authority, so the position fits.

**Documentation drift was masked until the deletion accidentally fixed it.** `CLAUDE.md` stated "10 automatic maintainers" throughout slice 007, while the live registry held 11 (the stub `fix-line-too-long` was registered but not listed in the enumeration). Nobody noticed because the stub returned NIL from its body and had no fixtures. The slice accidentally brought the stated number into alignment with reality. This is a calibration and pattern finding, not a capability finding: a drift-detection assertion in the test suite would have caught it years ago. This should become a pattern entry.

**The plan's pass-count prediction was off by 1.** The plan predicted 299 → 296 (delta 3), counting the three removed testcases. Reality: 299 → 295 (delta 4), because `validate-check-line-length-long` contained two `assert-*` forms (one `find-inspector` check, one `typep` check). Low impact — the slice verification still worked — but a clean calibration signal: *when predicting pass-count deltas for test removal, grep for `assert-*` inside the removed testcases, don't count testcases.*

**Invariant I7 is a principle change, not just a scope reduction.** "Atelier does not police line length" is structurally parallel to slice 007's idempotency principle, which was promoted from invariant to design principle #7 in `definition.md`. I7 deserves the same promotion, and the retrospective is recording the recommendation so the Reviewer can take it to the Steward at the next definition revision. The wording is roughly: *"Line length is advisory, not mandatory. The pretty-printer's `*print-right-margin*` is the only mechanism that influences line length, and a form that remains long after pretty-printing is accepted as-is. Atelier does not report long lines, does not fix long lines, and does not accept contributions that reintroduce a line-length inspector."*

## User feedback since delivery

*(to be filled 2–4 weeks after release — leave blank initially)*

## Backlog and roadmap changes

- **Completed (this slice):** removal of `check-line-length`, `fix-line-too-long`, `line-too-long-finding`, their tests, fixtures, and documentation.
- **Added:** nothing.
- **Reprioritized:**
  - Backlog item 16 reworded (line length removed from scope).
  - Roadmap Later-row for `fix-line-too-long` maintainer removed.
  - **Recommendation to Steward (not made in this slice):** promote invariant I7 to design principle #8 in `definition.md` on the next definition revision, parallel to how idempotency was promoted in slice 007.
  - **Recommendation for a future slice (added to backlog as a Considering item during knowledge consolidation):** a drift-detection assertion that keeps `CLAUDE.md`'s inspector/maintainer count lines in sync with `(length (list-inspectors))` / `(length (list-maintainers))` at test time.

## Verdict

**Delivered as scoped.** Zero failures, zero rework at the source level, principled contraction of the linter surface. The slice is an example of the delivery shape "maintenance / scope reduction" working cleanly through the five-role protocol — the planning interview was minimal (no architecture, no OSS, no new tests), the execution was mechanical, and the verification discipline held. Close the slice.
