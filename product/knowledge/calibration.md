# Estimation Calibration

Running record of where plans over- or under-estimated effort, pass counts, or other quantities. Used to make future plans more accurate.

---

## Slice 008 — Remove line-length inspector

**Planned phases:** 1 — **Actual phases:** 1
**Effort surprises:**
- Mechanical deletion went exactly as planned. Two `Edit`-tool whitespace-mismatch retries on `org.melusina.atelier.asd` were the only friction, both resolved immediately after a `Read` call.
- Risk 3 (the `testsuite/utilities.lisp` docstring) resolved on first inspection — the skip logic was already generic, only the docstring needed updating.

**Quantitative miss:**
- **Pass-count prediction:** planned 299 → 296 (delta 3), actual 299 → 295 (delta 4). The plan counted testcases (3 removed) as if each held one assertion. Reality: `validate-check-line-length-long` held two `assert-*` forms, for a total of 4 assertions across the 3 testcases. See `patterns.md` entry on pass-count predictions.

**Category patterns (emerging — single data point, tentative):**
- Pure-deletion slices (this one): plan matches reality closely on step count, file count, and effort. One assertion-count off-by-one is the only variance.
- Slice 007 (migration slice): plan predicted 10 fixtures, actual 6. The delta in 007 came from the plan assuming a uniformity that did not exist in the registry. 008's delta came from counting the wrong granularity.
- **Hypothesis (needs more data):** *Plan errors cluster around counting the wrong unit.* 007 counted "registered maintainers" when it should have counted "maintainers with discoverable fixtures." 008 counted "testcases" when it should have counted "assertions." When reviewing a plan, ask: *what unit am I counting, and is that the unit the reality will be measured in?*
