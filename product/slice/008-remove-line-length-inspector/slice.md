# Slice 008: Remove line-length inspector and fix-line-too-long maintainer

**Status:** Complete
**Type:** Maintenance
**Goal addressed:** None — scope cleanup
**Backlog items:** fix-line-too-long (previously "Later")
**Planned start / end:** 2026-04-09 / 2026-04-09
**Actual end:** 2026-04-09
**Implementation phases:**
  - Phase 1: `product/slice/008-remove-line-length-inspector/implementation-1.md` — Planned

---

## What changes for users

After this slice, Atelier no longer reports or attempts to fix long lines.

- `check-line-length` stops running. `line-too-long-finding` no longer appears in any report.
- `fix-line-too-long` is removed from the maintainer registry.
- The 20 carried-over fixtures under `testsuite/fixtures/autofix/fix-line-too-long/` are deleted.
- Users who care about line length rely on the pretty-printer (`pretty-print-form`) to re-flow code, and on their own judgement for anything the pretty-printer leaves long. This matches the gofmt school: the linter does not police line length; the formatter attempts to do the right thing; anything still too long is a human call.

This is a deliberate scope *reduction* based on the research in `product/reference/line-length-research.md`. The short version of the research: ESLint and Ruff do not auto-fix their line-length rules; Black and Prettier wrap structurally but explicitly allow lines to overflow when they cannot split safely; Go refuses to have the rule at all. Atelier chooses the Go position, on the reasoning that the pretty-printer is already the single authority on canonical Lisp text (slice 007 invariant) and a separate line-length reporter adds noise without adding correctness.

## Specification references

- `product/reference/line-length-research.md` — the research that led to the removal decision.
- Slice 007 retrospective, section "What we learned" — the "pretty-printer is the single authority on canonical Lisp text for syntax-level maintainers" principle that the gofmt position extends.

## Stories

### S1 — Remove the inspector and its finding class

**In order to** stop reporting line-length diagnostics, **a** developer **can** run `(atelier:lint-system "org.melusina.atelier")` and see no `line-too-long-finding` in the output regardless of line length.

**Acceptance criteria:**
- Given `src/inspectors/check-line-length.lisp` is deleted, when the system loads, then the load succeeds with no unresolved references.
- Given `line-too-long-finding` is removed from `src/finding.lisp`, when the system loads, then the load succeeds with no unresolved references.
- Given `check-line-length`, `line-too-long-finding`, and `*default-maximum-line-length*` are removed from `src/package.lisp` exports, when external consumers attempt to reference them, then they get a package-symbol-not-found error (expected — this is a breaking removal).
- Given `(atelier:find-inspector 'atelier:check-line-length)` was previously callable, when called after this slice, then the symbol `check-line-length` is not interned in the `atelier` package.
- Given the ASDF system `org.melusina.atelier` is loaded, when the component list is walked, then `src/inspectors/check-line-length` is absent.

### S2 — Remove the maintainer

**In order to** keep the maintainer registry honest, **a** developer **can** call `(atelier:list-maintainers)` and see no `fix-line-too-long` entry.

**Acceptance criteria:**
- Given `src/maintainers/fix-line-too-long.lisp` is deleted, when the system loads, then the load succeeds with no unresolved references.
- Given `fix-line-too-long` is removed from `src/package.lisp` exports, when the system loads, then the load succeeds.
- Given the ASDF system is loaded, when the component list is walked, then `src/maintainers/fix-line-too-long` is absent.
- Given `(atelier:list-maintainers)` is called, when the result is inspected, then `fix-line-too-long` is not in the returned list.

### S3 — Remove the tests and fixtures

**In order to** keep the test suite free of skipped or dead fixtures, **a** developer **can** run `(atelier/testsuite:run-all-tests)` and see no test references to line-length.

**Acceptance criteria:**
- Given `testsuite/inspectors/check-line-length.lisp` is deleted, when the testsuite system loads, then the load succeeds.
- Given `(testsuite-check-line-length)` is removed from `testsuite/entrypoint.lisp`, when the testsuite runs, then no testcase named `validate-check-line-length-*` executes.
- Given `testsuite/fixtures/autofix/fix-line-too-long/` is deleted (all 20 `.text` fixtures and the directory itself), when the auto-discovering fixture loader runs, then no attempt is made to load any `fix-line-too-long` fixture.
- Given the fixture-skip comment block in `testsuite/utilities.lisp:119` exists to carry these fixtures along, when the fixtures are gone, then the skip logic is also removed (or simplified if it is still needed for other directories — verify during planning).
- Given the full regression `(atelier/testsuite:run-all-tests)` is run, when it completes, then the pass count equals the pre-slice pass count minus exactly the three removed testcases (`validate-check-line-length-short`, `validate-check-line-length-long`, `validate-check-line-length-skips-definitions`), with zero failures and zero new skips.

### S4 — Update documentation and project metadata

**In order to** keep the project's self-description truthful, **a** reader of CLAUDE.md, README.md, the backlog, and the roadmap **can** see the current inspector/maintainer count and no mention of line-length as a supported diagnostic.

**Acceptance criteria:**
- Given `CLAUDE.md` states "16 inspectors" and "10 automatic maintainers" and lists them, when the slice closes, then it states "15 inspectors" and "9 automatic maintainers" with `check-line-length` and `fix-line-too-long` removed from the respective listings.
- Given `README.md` mentions line-length checking (if it does — verify during planning), when the slice closes, then the mention is removed.
- Given `product/backlog.md` lists item 16 as "Linter: line-level inspectors for CL (trailing whitespace, line length, mixed indentation)", when the slice closes, then item 16 no longer mentions line length.
- Given `product/roadmap.md` mentions `fix-line-too-long`, when the slice closes, then the mention is removed or marked as dropped.
- Given `product/reference/line-length-research.md` exists, when the slice closes, then it is **retained unchanged** as the record of why the removal happened.

## Quality Criteria

- [ ] Full regression (`(atelier/testsuite:run-all-tests)`) passes with zero failures. The pass count decreases by exactly 3 (the three removed line-length testcases).
- [ ] A clean `(ql:quickload "org.melusina.atelier")` in a fresh SBCL image produces no warnings about undefined functions, classes, or exported symbols.
- [ ] `(atelier:list-inspectors)` and `(atelier:list-maintainers)` both return lists whose length has decreased by exactly one, with no reference to the removed symbols.
- [ ] `grep -r 'line-too-long\|check-line-length\|fix-line-too-long\|line-too-long-finding' src/ testsuite/` returns zero matches after the slice (excluding `product/reference/line-length-research.md`, which is retained).
- [ ] No other maintainer or inspector changes behaviour. This is a pure deletion slice.

## Definition of Ready

- [x] Stories traceable to backlog items — the "fix-line-too-long (Later)" entry is the direct predecessor
- [x] Stories sized ≤ 2 days each — four mechanical removal stories
- [x] Acceptance criteria written
- [x] Quality criterion defined
- [x] Spec references identified — research reference retained at `product/reference/line-length-research.md`

## Definition of Done

- [ ] All four stories complete with acceptance criteria passing
- [ ] Quality criteria passing
- [ ] Full test suite passes with the expected pass-count decrement of exactly 3
- [ ] Clean-image load produces no warnings
- [ ] All implementation phases have completion notes
- [ ] `product/slice/008-remove-line-length-inspector/retrospective.md` created
- [ ] `product/backlog.md` updated (line-length removed from item 16; `fix-line-too-long` "Later" entry removed)
- [ ] `product/roadmap.md` updated
- [ ] `CLAUDE.md` inspector/maintainer counts and listings updated
- [ ] `README.md` updated if necessary

---

## Notes for the Tactician

This is a mechanical-deletion slice. The planning interview should be short: confirm the four stories, decide on the order (S1 → S2 → S3 → S4 is the natural dependency order, since S3 and S4 assume S1 and S2 are done), and confirm there is nothing new to learn architecturally. The only real risk is that `line-too-long-finding` is referenced somewhere outside the files surveyed above; the Strategist's grep found 40 files total, of which the source and test files are cleanly enumerated in the stories. The Tactician should re-run the grep as the first planning step to confirm no new references have appeared.

One open question for the Tactician: the `testsuite/utilities.lisp:119` fixture-skip block was written in slice 007 specifically to carry the `fix-line-too-long/` directory along "without being exercised." With the directory gone, the skip logic is dead code. Verify whether it exists only for this directory (in which case delete it) or also handles other directories (in which case leave it).

No OSS components, no architectural decisions, no new tests to write — just deletions and documentation updates.
