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

## Slice 009 — MCP server skeleton

**Planned phases:** 1 — **Actual phases:** 1
**Leading indicator:** planned "50–100 new assertions" (expressed as a range, following slice 008's lesson), actual **184 assertions**. Plan low by a factor of ~2 on the upper end.
**Effort surprises:**
- 9 reworks in Phase 2, 5 of them documented CL surprises (jzon null/false overloading, `assert-t` vs `assert-t*` strictness, plist reversal via `nreverse`, `defclass` initarg rejection on notification classes, `prin1` keyword case). Each caught in one test cycle — low individual cost, significant cumulative cost.
- Plan Amendment 1 during step 2 (writing `references/mcp-protocol.md`): the MCP spec revealed that `resources/list` and `resources/templates/list` are distinct methods, requiring one additional class, one additional dispatcher method, one additional fixture, and an adjusted acceptance criterion. Handled via the append-not-rewrite amendment protocol.
- `define-tool` macro decomposition into 17 helper functions each ≤25 lines was forced by a standing "no function over 25 lines" guideline. In hindsight this made the macro dramatically more testable and more robust to rework — every rework touched exactly one helper.

**Quantitative miss (assertion count):**
- Predicted range: 50–100.
- Actual: 184 (+84% above upper bound).
- Root cause: the plan counted testcases (~40) and multiplied by 1.25–2.5 assertions each. Reality: thorough testcases bundled 3–6 assertions each (per-method dispatcher checks, per-field shape assertions, cross-cutting registry counts, compile-time rejection checks). The multiplier was under-predicted by ~2×.

**Category pattern update** (slice 009 adds a fifth data point to the "counting the wrong unit" hypothesis):

| Slice | Wrong unit | Right unit |
|---|---|---|
| 002 | Inspectors (plan said ≥3) | Stories-shipping-inspectors (2 in the story list) |
| 007 | Registered maintainers (plan said 10) | Maintainers-with-discoverable-fixtures (6 in reality) |
| 008 | Testcases (plan said 3) | Assertions inside the testcases (4 in reality) |
| 009 | Assertions × 1.25–2.5 factor | Assertions × 3–6 factor for test-heavy slices |
| 001 | A-priori API surface count | *There is no right unit for foundation slices — use functional criteria* |

**Revised guidance for test-heavy slices** (slices where the primary deliverable is new test infrastructure *and* new source code tested by it): predict assertion counts as `testcases × 3–6`, not `testcases × 1–2`. A thoroughly-tested new subsystem tends toward the upper end; a slice that only adds a handful of testcases for an existing subsystem tends toward the lower end.

**Non-surprise:** the base atelier testsuite held at 295/295 throughout slice 009 development. The consolidated `asdf:test-system "org.melusina.atelier/testsuite"` invocation now runs 479 assertions (295 base + 184 MCP) in one call and completes in a few seconds in a fresh SBCL subprocess. The base suite's regression discipline carried over unchanged.

## Slice 010 — Editor foundation and MCP eval

**Planned phases:** 2 — **Actual phases:** 2
**Phase 1 prediction:** 50–150 assertions — **Actual:** 104. Within range.
**Phase 2 prediction:** 40–80 assertions — **Actual:** 40. At the low end.
**Total new assertions:** 144 (104 + 40). Combined suite: 623/623.
**Effort surprises:**
- Phase 1: 5 reworks. The write-path rework (Reviewer feedback changing the architecture from CST reconstruction to verbatim source copy) was the largest single rework in the slice. Cost: 89 lines removed, 32 added. High-value — the resulting design is simpler and correct.
- Phase 2: 5 reworks. Three of five were SWANK-protocol surprises discovered during integration. Creating exploratory test systems (`testsuite/input-output`, `testsuite/swank`) was the turning point — all remaining issues were diagnosed in one test run each after the exploratory systems existed.

**Category pattern update:**

| Slice | Wrong unit | Right unit |
|---|---|---|
| 010 P2 | "40–80 assertions" (correct range) | Actual 40 — slow tests produce 1–2 assertions each vs Phase 1's fast tests at 3–5 each |

**Observation:** Slow tests (child spawn, SWANK eval) produce fewer assertions per testcase than fast tests (pure functions, in-memory parsing). When a phase is dominated by slow tests, the assertion count trends toward the low end of the range. The range prediction was correct; the distribution within the range is predictable from the test-category mix.

**Timing:** Phase 2 child-dependent tests take ~80s total (one child spawn ≈ 20s, eval/introspection ≈ 5s, run-tests-fresh ≈ 30s, run-tests-in-child ≈ 20s, shutdown+orphan ≈ 5s). This is the first slice where test runtime is a material factor.

## Slice 011 — MCP debugger and restarts

**Planned phases:** 1 — **Actual phases:** 1
**Assertion prediction:** 30–60 — **Actual:** 24. Below the lower bound.
**Effort surprises:**
- 6 reworks, all SWANK protocol surprises. Same pattern as slice 010 Phase 2 (5 SWANK reworks). The exploratory test system caught all issues but the diagnosis cost was cumulative.
- eval-in-frame and timeout deferred — SWANK functions designed for Emacs don't work from CL clients. This is the third instance of this pattern (slice 010: `interactive-eval`, slice 011: `eval-string-in-frame` and `:emacs-interrupt :repl-thread`).

**Root cause of low assertion count:** 2 stories deferred (eval-in-frame, timeout) which would have contributed ~6–10 assertions. The remaining 4 stories produced 21 slow-test assertions — at 5.25 assertions per story, consistent with the "slow tests produce fewer assertions" observation from slice 010.

**Category pattern update:**
- SWANK-integration stories consistently require 2–3 reworks per story from protocol surprises. Budget 1.5× the naive estimate.
- Stories that depend on SWANK functions designed for Emacs (containing "for-emacs" in the name, or documented as interacting with Emacs UI state) have a ~50% chance of being unusable from CL clients. Verify via exploratory test before committing the story.
