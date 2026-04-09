# Slice 007: Autofix Cycle Redesign and New CST Inspectors

**Status:** Planned
**Type:** Bet
**Goal:** G2 — Linter covers file, line, region, CST level with ASDF
**OKR contribution:** G2 — sharpen the diagnostic-cycle test protocol and expand the inspector catalogue
**Hypothesis assumption:** The `(inspector, finding, maintainer, resolution)` quadruple is the real unit under test in our autofix fixtures. Making that explicit in the fixture format — with structural (CST / AST) comparison and a self-idempotency assertion — will surface any latent bugs in existing maintainers without requiring changes to the write-back engine.
**Hypothesis prediction:** Renaming `testsuite/fixtures/maintainer/` to `testsuite/fixtures/autofix/` and migrating the 10 existing maintainer fixtures to the new four-part format (input / expected finding slots / expected fixed code) will preserve all current test behaviour and pass a self-idempotency assertion on every existing fixture. Three new CST inspectors (IF→WHEN/UNLESS, single-form PROGN, WHEN-NOT→UNLESS) will extend the style-check catalogue without touching the autofix pipeline.
**Hypothesis disposition:** ✅ Validated — the underlying pipeline is already proven through 10 maintainers and 13 inspectors (slices 001–006).
**Leading indicator:** 10 existing autofix-cycle fixtures migrated to the new format; self-idempotency assertion holds for all 10; at least 3 new CST inspectors with auto-discovered fixtures passing.
**Kill criterion:** If migrating the existing fixtures reveals that more than one existing maintainer is non-idempotent, or if the new fixture format requires special-casing for more than one maintainer, the design needs rethinking before further inspector work.
**Planned start / end:** 2026-04-09 / 2026-04-12
**Actual end:**
**Implementation phases:**
  - Phase 1: product/slice/007-maintainer-and-inspector-expansion/implementation-1.md — Planned

---

## Customer Journey

For the developer using `(atelier:lint-system "my-system" :autofix t)`, the user-visible surface gains three new style checks: a warning to rewrite `(if TEST THEN nil)` as `(when TEST THEN)`, a warning against `(progn FORM)` with a single body form, and a warning to rewrite `(when (not TEST) ...)` as `(unless TEST ...)`.

For the maintainer of Atelier, the internal surface gains a sharper test protocol. The `testsuite/fixtures/autofix/` directory replaces `testsuite/fixtures/maintainer/`, and every fixture declares its full diagnostic cycle explicitly: which inspector produces which finding class, which maintainer consumes it, and which resolution class the maintainer emits. The test harness asserts that the maintainer yields a self-idempotent result (N=1 convergence) — running the autofix cycle twice on the same input produces the same AST as running it once.

## Personas served
- End user: Common Lisp developer — gets three new idiomatic-style checks.
- Atelier maintainer: the autofix cycle is now a first-class testable unit; regressions in idempotency surface immediately.

## Stories

### S1: Autofix-cycle fixture redesign
**As an** Atelier maintainer, **I want** the `(inspector, finding, maintainer, resolution)` quadruple to be explicit in every fixture, **so that** test failures point at the exact component that broke.

**Acceptance criteria:**
- Given a fixture in `testsuite/fixtures/autofix/`, when it is read, then its YAML front matter declares `inspector`, `finding`, `maintainer`, `resolution`, and `description` fields.
- Given a fixture, when it is read, then the body contains exactly three `---`-separated documents: input source, expected finding slot values (plist), expected fixed code.
- Given the directory `testsuite/fixtures/maintainer/`, when the slice completes, then that directory no longer exists and `testsuite/fixtures/autofix/` holds all fixtures.
- Given the function `validate-one-maintainer-fixture`, when the slice completes, then it has been renamed `validate-one-autofix-cycle-fixture` and its docstring describes what the autofix cycle is.
- Given the 10 existing maintainer fixtures, when the slice completes, then all 10 have been migrated to the new format and pass their primary assertions.

### S2: Structural comparison for syntax inspectors
**As an** Atelier maintainer, **I want** the autofix cycle test to compare at the CST / AST level (not the text level) for syntax inspectors, **so that** the separation of concerns between maintainers and the pretty-printer is preserved.

**Acceptance criteria:**
- Given a fixture whose inspector is a SYNTAX-INSPECTOR, when the test runs, then equality of the result form is asserted against `read-from-string` of the expected fixed code.
- Given a fixture whose inspector is a LINE-INSPECTOR, when the test runs, then equality is asserted at the level that matches the finding class (documented per fixture).
- Given an expected finding slot plist in a fixture, when the test runs, then each slot named in the plist is checked against the produced finding; slots not named are not checked.
- Given an expected resolution class in the front-matter, when the test runs, then the produced resolution is asserted to be an instance of that class.

### S3: Self-idempotency assertion (N=1 convergence)
**As an** Atelier maintainer, **I want** the autofix cycle test to assert that applying the cycle twice yields the same AST as applying it once, **so that** non-idempotent maintainers cannot ship.

**Acceptance criteria:**
- Given any migrated autofix-cycle fixture, when the test runs, then after the primary assertion the test applies the same (inspector, maintainer) pair to the result and asserts AST equality with the first result.
- Given one of the 10 existing maintainers, when the self-idempotency assertion runs, then the assertion holds (zero regressions).
- Given the risk that a maintainer may legitimately need more than one pass to converge, when such a case surfaces in the future, then the assumption `N=1` is revisited; for this slice, `N=1` is the contract and any violation is a bug.

### S4: Pretty-printer cross-population of fixed-code documents
**As an** Atelier maintainer, **I want** every autofix-cycle fixture **whose inspector operates at the :SYNTAX level** to have its "expected fixed code" document asserted as a fixed point of `read ⟫ pretty-print-form`, **so that** the pretty-printer is the single authority on canonical text for Lisp forms — regardless of whether the maintainer emits a text-resolution or a syntax-resolution.

**Acceptance criteria:**
- Given a migrated autofix-cycle fixture whose inspector is a SYNTAX-INSPECTOR, when the pretty-printer test suite runs, then the expected fixed code string is asserted to be a fixed point of `read` composed with `pretty-print-form`.
- Given a migrated autofix-cycle fixture whose inspector is a LINE-INSPECTOR, when the pretty-printer test suite runs, then the fixture is excluded from the cross-population — its expected document is a text fragment with semantically meaningful whitespace, not a canonical Lisp form.
- Given the existing syntax-level maintainer fixtures, when this assertion runs after migration, then it holds for all of them.

**Scope note:** `fix-mixed-indentation` does not get an autofix-cycle fixture at all — its expected output (e.g. `  (defvar *x* 1)` with semantically meaningful leading whitespace) does not fit the fixture format cleanly. Its behaviour is verified by the existing ad-hoc `define-testcase` at `testsuite/maintainers/fix-mixed-indentation.lisp`. Discoverable fixtures are a convenience, not a goal in themselves.

### S5: CST inspector — check-single-branch-if (IF→WHEN/UNLESS)
**As a** CL developer, **I want** the linter to flag `(if TEST THEN nil)` as "use WHEN" and `(if TEST nil ELSE)` as "use UNLESS", **so that** my code follows idiomatic Common Lisp style.
**Acceptance criteria:**
- Given `(if (foo) (bar) nil)`, when inspection runs, then a finding with observation referencing WHEN is reported.
- Given `(if (foo) nil (bar))`, when inspection runs, then a finding with observation referencing UNLESS is reported.
- Given `(if (foo) (bar) (baz))` (both branches non-nil), when inspection runs, then no finding is reported.
- Given `(if (foo) (bar))` (no else branch, implicit nil), when inspection runs, then a finding is reported suggesting WHEN.

### S6: CST inspector — check-single-form-progn
**As a** CL developer, **I want** the linter to flag `(progn FORM)` containing a single body form, **so that** unnecessary PROGN wrappers are removed.
**Acceptance criteria:**
- Given `(progn (foo))`, when inspection runs, then a finding is reported.
- Given `(progn (foo) (bar))`, when inspection runs, then no finding is reported.
- Given `(progn)` (empty progn), when inspection runs, then no finding is reported.

### S7: CST inspector — check-when-not (WHEN-NOT→UNLESS)
**As a** CL developer, **I want** the linter to flag `(when (not TEST) BODY)` and suggest `(unless TEST BODY)`, **so that** my code follows idiomatic Common Lisp style.
**Acceptance criteria:**
- Given `(when (not x) (foo))`, when inspection runs, then a finding suggesting UNLESS is reported.
- Given `(when x (foo))`, when inspection runs, then no finding is reported.

## Out of Scope

- **fix-line-too-long implementation.** Removed from this slice. The maintainer skeleton and 20 fixtures remain in place but the skeleton continues to return NIL. A future slice will implement it when the design of text-vs-CST comparison for the string-breaking strategy is settled.
- **Pipeline idempotency.** Self-idempotency (running the same `(inspector, maintainer)` pair twice on the same input) is the scope of this slice. Pipeline idempotency (running the whole `lint-system :autofix t` over a whole file twice) is a distinct, harder property and is tracked as a long-term goal in the roadmap.

## Quality Attribute Acceptance Criteria
- [ ] Self-idempotency holds for all 10 existing migrated fixtures
- [ ] Every existing fixture's expected fixed code is a `read ⟫ pretty-print` fixed point
- [ ] No regressions: `(asdf:test-system "org.melusina.atelier")` passes in full

## Capability Maturity Transitions
- Autofix / Write-back: Foundation → Foundation (test protocol sharpened; no engine changes)
- CST-level Inspection: Foundation → Foundation (three new inspectors)

## Definition of Ready
- [x] Hypothesis disposition ✅
- [x] Stories sized: each story completable in 1–2 days by one developer
- [x] Acceptance criteria written for all stories
- [x] QA acceptance criterion defined
- [x] Leading indicator baseline established: 10 existing maintainer fixtures; 13 inspectors
- [x] Dependencies clear: infrastructure from slices 001–006 is in place
- [x] Research reference on linter convergence (Ruff, ESLint) committed to `references/linter-convergence.md`

## Definition of Done
- [ ] All stories complete with acceptance criteria passing
- [ ] Quality attribute criteria passing
- [ ] `testsuite/fixtures/autofix/` replaces `testsuite/fixtures/maintainer/`
- [ ] 10 existing fixtures migrated; 3 new CST inspectors with fixtures added
- [ ] All implementation phases have completion notes
- [ ] `product/slice/007-maintainer-and-inspector-expansion/retrospective.md` created
- [ ] `product/maturity-tracker.md` updated
