# Estimation Calibration

Running record of where plans over- or under-estimated effort, pass counts, or other quantities. Used to make future plans more accurate.

---

## Slice 001 — Finding/resolution schema

**Planned phases:** 1 — **Actual phases:** 1
**Leading indicator:** planned "public API symbols >= 25", actual **95**. A 3.8x overshoot.
**Interpretation:** For foundation slices, prefer functional criteria ("3rd-party companion can define an inspector") over a count.

## Slice 002 — ASDF integration and file-level inspectors

**Planned phases:** 1 — **Actual phases:** 1
**Leading indicator:** planned "concrete inspectors >= 3", actual **2**. Plan arithmetic error — only two inspectors were in scope.
**Lesson:** Plan thresholds should be derived from the story list, not set aspirationally.

## Slice 005 — Autofix pipeline with pretty-printer

**Planned phases:** 1 — **Actual phases:** 1
**Leading indicator:** planned "automatic maintainers >= 6", actual **7** (+1 emergent `fix-labels-to-flet`).
**Calibration value:** Don't count emergent deliverables against plan accuracy.

## Slice 007 — Autofix cycle redesign and new CST inspectors

**Planned phases:** 1 — **Actual phases:** 1
**Leading indicator:** planned "10 migrated fixtures + 3 new CST inspectors", actual **5 migrated + 3 new = 8 delivered**. Plan counted "maintainers" not "maintainers with discoverable fixtures."

## Slice 008 — Remove line-length inspector

**Planned phases:** 1 — **Actual phases:** 1
**Quantitative miss:** planned 299 -> 296 (delta 3), actual 299 -> 295 (delta 4). Plan counted testcases; reality measured assertions.

**Hypothesis (supported by 4 data points):** **Plan-accuracy failures cluster around counting the wrong unit.**

| Slice | Wrong unit | Right unit |
|---|---|---|
| 002 | Inspectors (plan said >=3) | Stories-shipping-inspectors (2 in the story list) |
| 007 | Registered maintainers (plan said 10) | Maintainers-with-discoverable-fixtures (6 in reality) |
| 008 | Testcases (plan said 3) | Assertions inside the testcases (4 in reality) |
| 001 | A-priori API surface count | *There is no right unit for foundation slices — use functional criteria* |

**Preferred style for numeric acceptance criteria:** prefer *invariants* over *tight numbers*. "Full regression passes with zero failures and zero new skips; pass count does not decrease by more than N" is stronger than "pass count = 296."

## Slice 009: Project Structure and Hooks

**Planned phases:** 1 — **Actual phases:** 1
**Effort surprises:**
- None — the slice was sized correctly as M (medium) and delivered in one phase.
**Category patterns:**
- File-level inspectors on .asd files are simpler than CST inspectors: no Eclector parsing, straightforward READ + search-based position tracking.
- Test isolation for filesystem-mutating tests (pre-commit hooks) requires dedicated temp directories, not the shared system temp dir.

## Slice 011: Linter API Cleanup

**Planned phases:** 1 — **Actual phases:** 1
**Effort surprises:**
- Slice scoped as S (1–2 days); delivered in a single session.
- The primitive extraction was almost mechanical (literal `flet` → `defun` lift) because the original `lint-system` had already separated the stages as local helpers. The real design work was naming the public primitives and deciding where dynamic-configuration binding belonged.
**Category patterns:**
- API-cleanup slices benefit from an upfront design discussion ("what is the surface shape?") before any code is touched — we converged in a short back-and-forth on `:action :inspect|:preview|:fix` and `:scope :system|:project` and the subsequent refactor did not revisit the surface.
- When removing a symbol (vs. deprecating), grepping the production tree (`src/`, `test/`, `libexec/`, `README.md`, `CLAUDE.md`, `resource/`) plus separately acknowledging intentional "name-of-absent-symbol" test assertions is the right audit before declaring the symbol gone.
