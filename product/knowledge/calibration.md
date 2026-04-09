# Estimation Calibration

Running record of where plans over- or under-estimated effort, pass counts, or other quantities. Used to make future plans more accurate.

---

## Slice 001 — Finding/resolution schema

**Planned phases:** 1 — **Actual phases:** 1
**Leading indicator:** planned "public API symbols ≥ 25", actual **95**. A 3.8× overshoot.
**Interpretation:** The target was set conservatively because nobody had tried to enumerate the surface area before. The overshoot is not a planning failure — it is a discovery that the schema naturally exports many small helpers and class names — but it is a signal that leading-indicator thresholds for schema/foundation slices are effectively impossible to set before the surface is drawn. For foundation slices, prefer "the public API is complete enough for a 3rd-party companion system to define an inspector and maintainer" (a functional criterion) over a count.

## Slice 002 — ASDF integration and file-level inspectors

**Planned phases:** 1 — **Actual phases:** 1
**Leading indicator:** planned "concrete inspectors ≥ 3", actual **2**. The retrospective explicitly records the plan target as a *plan arithmetic error* — only two inspectors were actually in scope (`check-file-encoding`, `check-spdx-license-header`) and the ≥3 threshold was set without counting the stories. No stories were dropped; the threshold was wrong from the start.
**Lesson:** Plan thresholds for "count of X delivered" should be derived from the story list, not set aspirationally. Count stories first, threshold second.

## Slice 005 — Autofix pipeline with pretty-printer

**Planned phases:** 1 — **Actual phases:** 1
**Leading indicator:** planned "automatic maintainers ≥ 6", actual **7** (+1 emergent `fix-labels-to-flet`).
**Effort surprises:** The emergent seventh inspector/maintainer pair came from the slice's own source code — the `lint-system` autofix pipeline contained a spurious LABELS form that became the motivating test case for a new inspector. A slice caught a problem in its own implementation and shipped the fix. This is rare and pleasant, not plannable.
**Calibration value:** +1 emergent deliverable is *not* a planning accuracy failure; it is evidence that the autofix protocol is extensible enough that new maintainers can be added mid-slice without friction. But for calibration purposes: don't count emergent deliverables against plan accuracy. Count them separately as a "slice-extensibility signal."

## Slice 007 — Autofix cycle redesign and new CST inspectors

**Planned phases:** 1 — **Actual phases:** 1
**Leading indicator:** planned "10 migrated fixtures + 3 new CST inspectors", actual **5 migrated + 3 new = 8 delivered**. Plan over by 5 on the migration count.
**Root cause:** The plan counted all 10 registered maintainers as if each had a discoverable fixture. Reality: only 6 did (`fix-labels-to-flet`, `fix-header-line`, `fix-footer-line`, and `fix-project-identification` used ad-hoc testcases, not fixture files). During execution, `fix-mixed-indentation`'s fixture was also removed, leaving 5. The slice *counted the wrong unit*: "maintainers" not "maintainers with discoverable fixtures."
**Calibration value:** This is the third data point in the "counting the wrong unit" pattern (slices 002, 007, 008). See the hypothesis below.

## Slice 008 — Remove line-length inspector

**Planned phases:** 1 — **Actual phases:** 1
**Effort surprises:**
- Mechanical deletion went exactly as planned. Two `Edit`-tool whitespace-mismatch retries on `org.melusina.atelier.asd` were the only friction, both resolved immediately after a `Read` call.
- Risk 3 (the `testsuite/utilities.lisp` docstring) resolved on first inspection — the skip logic was already generic, only the docstring needed updating.

**Quantitative miss:**
- **Pass-count prediction:** planned 299 → 296 (delta 3), actual 299 → 295 (delta 4). The plan counted testcases (3 removed) as if each held one assertion. Reality: `validate-check-line-length-long` held two `assert-*` forms, for a total of 4 assertions across the 3 testcases. See `patterns.md` entry on pass-count predictions.

**Category patterns (four data points; hypothesis promoted from tentative to supported):**

*Pure-deletion slices* (008): plan matches reality closely on step count, file count, and effort. One assertion-count off-by-one was the only variance.

*Foundation / schema slices* (001): leading-indicator thresholds cannot be set meaningfully in advance. Prefer functional criteria ("3rd-party companion can define an inspector") over numeric thresholds.

*Migration slices* (007): plan assumes uniformity across a registry that is not actually uniform. The planner counted "registered maintainers" as if they were a homogeneous set.

*Arithmetic-error slices* (002): the plan target contradicts the story list. The threshold is simply wrong from the start because it was set aspirationally.

**Hypothesis (supported by 4 data points):** **Plan-accuracy failures cluster around counting the wrong unit.**

| Slice | Wrong unit | Right unit |
|---|---|---|
| 002 | Inspectors (plan said ≥3) | Stories-shipping-inspectors (2 in the story list) |
| 007 | Registered maintainers (plan said 10) | Maintainers-with-discoverable-fixtures (6 in reality) |
| 008 | Testcases (plan said 3) | Assertions inside the testcases (4 in reality) |
| 001 | A-priori API surface count | *There is no right unit for foundation slices — use functional criteria* |

**When reviewing a plan, ask:** *What unit am I counting, and is that the unit the reality will be measured in?* If you can be wrong by a factor of 2 because of a definitional misalignment, the criterion is brittle.

**Preferred style for numeric acceptance criteria:** prefer *invariants* over *tight numbers*. "Full regression passes with zero failures and zero new skips; pass count does not decrease by more than N" is a stronger criterion than "pass count = 296," because the former survives counting-unit errors and the latter does not.
