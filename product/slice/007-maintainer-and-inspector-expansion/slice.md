# Slice 007: Maintainer and Inspector Expansion

**Status:** Planned
**Type:** Bet
**Goal:** G2 — Linter covers file, line, region, CST level with ASDF
**OKR contribution:** G2 — expand automatic fix coverage and inspector repertoire
**Hypothesis assumption:** The existing finding/resolution/write-back pipeline can absorb a complex maintainer (line-too-long) and new CST inspectors without architectural changes.
**Hypothesis prediction:** Implementing fix-line-too-long against 20 fixtures and adding style inspectors (IF to WHEN/UNLESS, etc.) will validate that the pipeline handles both structural reformatting and new diagnostic categories without regressions.
**Hypothesis disposition:** ✅ Validated — pipeline proven through 10 maintainers and 13 inspectors in slices 001--006.
**Leading indicator:** 20/20 fix-line-too-long fixtures passing; at least 3 new CST inspectors; fixed-point assertion in maintainer test harness.
**Kill criterion:** If fewer than 15/20 fixtures pass or the fixed-point assertion reveals regressions in existing maintainers, the write-back engine needs redesign.
**Planned start / end:** 2026-04-08 / 2026-04-12
**Actual end:**
**Implementation phases:**
  - Phase 1: product/slice/007-maintainer-and-inspector-expansion/implementation-1.md — Planned

---

## Customer Journey

A developer running `(atelier:lint-system "my-system" :autofix t)` now gets automatic reformatting of lines exceeding the configured length limit (100 columns by default). Long function calls, definition forms, loop clauses, and string literals are broken into idiomatic multi-line layout. New CST inspectors flag additional Common Lisp style issues: single-branch IF that should be WHEN/UNLESS, PROGN with a single body form, etc.

## Personas served
- End user: Common Lisp developer — gets actionable fixes for line length violations and additional style guidance with zero manual effort.

## Stories

### S1: fix-line-too-long — structural reformatting strategy
**As a** CL developer, **I want** long lines automatically reformatted into idiomatic multi-line layout, **so that** I do not have to manually reflow code.
**Acceptance criteria:**
- Given a long function call (keyword args), when autofix runs, then args are wrapped one per line aligned after the function name.
- Given a defun/defmethod with many parameters, when autofix runs, then parameters are wrapped one per line aligned after the first parameter.
- Given a long IF/WHEN/COND test, when autofix runs, then sub-conditions are wrapped with standard indentation.
- Given a long LOOP form, when autofix runs, then each clause appears on its own line with LOOP keyword alignment.
- Given a long LET binding, when autofix runs, then the binding value is wrapped across lines.

### S2: fix-line-too-long — string-breaking strategy
**As a** CL developer, **I want** long string literals automatically broken via `#.(concatenate 'string ...)`, **so that** line length is respected without changing runtime semantics.
**Acceptance criteria:**
- Given a defun with a long docstring, when autofix runs, then the docstring is replaced with `#.(concatenate 'string ...)` breaking at word boundaries.
- Given a FORMAT call with a long control string, when autofix runs, then the control string is broken via compile-time concatenation.
- Given a DEFVAR/WARN/MAKE-INSTANCE with a long string, when autofix runs, then the string is broken similarly.
- Given a deeply indented form, when autofix runs, then the break width accounts for the current indentation level.

### S3: fix-line-too-long — all 20 fixtures pass
**As a** maintainer, **I want** all 20 test fixtures in `testsuite/fixtures/maintainer/fix-line-too-long/` to pass, **so that** coverage is comprehensive from day one.
**Acceptance criteria:**
- Given the existing fixture auto-discovery mechanism, when the test suite runs, then all 20 fixtures are discovered and validated.
- Given each fixture, when the maintainer is applied, then the output matches the expected string exactly.

### S4: Pretty-printer fixed-point assertion
**As a** maintainer, **I want** an assertion that applying any maintainer twice yields the same output as applying it once, **so that** the autofix pipeline is idempotent.
**Acceptance criteria:**
- Given `validate-one-maintainer-fixture`, when the fixture is validated, then the test additionally applies the maintainer a second time to the result and asserts string equality with the first application.
- Given any of the existing maintainer fixtures (fix-earmuffs, fix-constant-naming, fix-bare-lambda, fix-loop-keywords, fix-labels-to-flet, fix-trailing-whitespace, fix-mixed-indentation, fix-header-line, fix-footer-line, fix-project-identification, fix-line-too-long), when the fixed-point assertion runs, then no regressions are found.

### S5: CST inspector — check-single-branch-if (IF to WHEN/UNLESS)
**As a** CL developer, **I want** the linter to flag `(if TEST THEN nil)` as "use WHEN" and `(if TEST nil ELSE)` as "use UNLESS", **so that** my code follows idiomatic Common Lisp style.
**Acceptance criteria:**
- Given `(if (foo) (bar) nil)`, when inspection runs, then a finding with observation "Use WHEN instead of IF with NIL else branch" is reported.
- Given `(if (foo) nil (bar))`, when inspection runs, then a finding with observation "Use UNLESS instead of IF with NIL then branch" is reported.
- Given `(if (foo) (bar) (baz))` (both branches non-nil), when inspection runs, then no finding is reported.
- Given `(if (foo) (bar))` (no else branch, implicit nil), when inspection runs, then a finding is reported suggesting WHEN.

### S6: CST inspector — check-single-form-progn
**As a** CL developer, **I want** the linter to flag `(progn FORM)` containing a single body form, **so that** unnecessary PROGN wrappers are removed.
**Acceptance criteria:**
- Given `(progn (foo))`, when inspection runs, then a finding is reported.
- Given `(progn (foo) (bar))`, when inspection runs, then no finding is reported.
- Given `(progn)` (empty progn), when inspection runs, then no finding is reported (separate concern).

### S7: CST inspector — check-when-single-test (nested AND in WHEN)
**As a** CL developer, **I want** the linter to flag `(when (and A B) BODY)` where the AND has only two operands as acceptable but `(when TEST BODY1 BODY2 ... BODYn)` where n=1 as a style note reminding that WHEN has an implicit PROGN, **so that** developers are aware of the implicit PROGN semantics.
**Acceptance criteria:**
- Given `(when test form)`, when inspection runs, then no finding is reported (single-body WHEN is fine).
- Given `(when (not test) form)`, when inspection runs, then a finding suggests UNLESS.

## Quality Attribute Acceptance Criteria
- [ ] All 20 fix-line-too-long fixtures pass with string comparison
- [ ] Fixed-point (idempotency) holds for all maintainer fixtures — no exceptions
- [ ] No regressions: full test suite (`asdf:test-system "org.melusina.atelier"`) passes

## Capability Maturity Transitions
- Autofix / Write-back: Foundation (maintained — complex maintainer validates pipeline)
- CST-level Inspection: Foundation (maintained — new inspector patterns)

## Definition of Ready
- [x] Hypothesis disposition ✅
- [x] Stories sized: S1--S2 are 1--2 days each; S3--S4 are under 1 day each; S5--S7 are under 1 day each
- [x] Acceptance criteria written for all stories
- [x] QA acceptance criterion defined
- [x] Leading indicator baseline established: 10 maintainers / 13 inspectors (end of slice 006)
- [x] Dependencies clear: all infrastructure in place from slices 001--006

## Definition of Done
- [ ] All stories complete with acceptance criteria passing
- [ ] Quality attribute criteria passing
- [ ] Leading indicator being measured (fixture count, inspector count)
- [ ] All implementation phases have completion notes
- [ ] `product/slice/007-maintainer-and-inspector-expansion/retrospective.md` created
- [ ] `product/maturity-tracker.md` updated
