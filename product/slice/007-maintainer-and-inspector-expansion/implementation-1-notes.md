# Implementation Phase 1 Notes: Slice 007 — Autofix Cycle Redesign and New CST Inspectors

**Phase:** 1
**Plan:** [product/slice/007-maintainer-and-inspector-expansion/implementation-1.md](implementation-1.md)
**Recorded:** 2026-04-09
**Status:** Complete

## Stories delivered in this phase

- **S1 — Autofix-cycle fixture redesign** — directory renamed (`test/fixtures/maintainer/` → `test/fixtures/autofix/`) via `git mv` preserving history; new 4-part fixture format (YAML front-matter with mandatory `inspector`/`finding`/`maintainer`/`resolution` symbol fields, three body documents: input / finding-slot plist / expected fixed code); `validate-one-autofix-cycle-fixture` replaces `validate-one-maintainer-fixture`.
- **S2 — Structural comparison for syntax inspectors** — `autofix-cycle-comparison-level` returns `:form` for `:syntax` inspectors and `:string` for `:line`; AST equality via `read-from-string` for the former.
- **S3 — Self-idempotency at N=1** — every autofix-cycle fixture asserts that running the `(inspector, maintainer)` pair on the result of the first pass yields the same result as the first pass.
- **S4 — Pretty-printer cross-population** — delivered with scope correction: the assertion is applied to autofix-cycle fixtures **whose inspector operates at the :SYNTAX level**, not to every fixture. See "Plan deviations" below.
- **S5 — check-single-branch-if** — inspector + 4 fixtures (when, unless, implicit-nil, clean) + test case. Flags `(if T E nil)`, `(if T nil E)`, `(if T E)`; ignores `(if T E1 E2)` with both branches non-nil.
- **S6 — check-single-form-progn** — inspector + 3 fixtures (baseline, clean, good) + test case. Flags `(progn F)` with exactly one body form; ignores `(progn)` and `(progn A B ...)`.
- **S7 — check-when-not** — inspector + 2 fixtures (baseline, clean) + test case. Flags `(when (not X) BODY)` suggesting `(unless X BODY)`.

## Acceptance criteria results

| # | Criterion | Result | Verified by |
|---|-----------|:------:|-------------|
| AC1 | `test/fixtures/autofix/` replaces `test/fixtures/maintainer/` | ✓ | `git mv` preserved history; `git status` shows `R` entries |
| AC2 | Existing maintainer fixtures migrated to 4-part format | ✓ | 5 of 6 migrated (see deviation 1); the 6th (fix-mixed-indentation) intentionally removed and covered by ad-hoc test |
| AC3 | Primary assertion passes for all migrated fixtures | ✓ | `(validate-autofix-cycle-fixtures)` → 20/20 assertions pass in clean subprocess |
| AC4 | Self-idempotency N=1 holds | ✓ | Embedded in the same 20 assertions |
| AC5 | Pretty-printer cross-population — syntax fixtures | ✓ | `(validate-autofix-cycle-fixtures-as-pretty-printer-fixed-points)` → 4/4 pass (fix-earmuffs, fix-constant-naming, fix-bare-lambda, fix-bare-loop-keywords) |
| AC6 | `check-single-branch-if` detects IF→WHEN and IF→UNLESS | ✓ | `(testsuite-check-single-branch-if)` → 5/5 |
| AC7 | `check-single-form-progn` detects single-body PROGN | ✓ | `(testsuite-check-single-form-progn)` → 4/4 |
| AC8 | `check-when-not` detects WHEN (NOT …) | ✓ | `(testsuite-check-when-not)` → 3/3 |
| AC9 | `validate-one-maintainer-fixture` no longer in source | ✓ | `Grep` returns zero hits across `src/`, `test/` |
| AC10 | No regressions | ✓ | `atelier/test:run-all-tests` → **357/357 passing** in clean SBCL 2.6.3 subprocess |

## Test results

- **Fast:** 357 passed, 0 failed (clean SBCL subprocess via `run-all-tests`)
- **Slow:** 0 — none in scope
- **Snail:** 0 — none in scope
- **Skipped:** 20 × fix-line-too-long text documents — carried along by `git mv` as planned, silently skipped by the autofix-cycle fixture loader because their front-matter lacks the new mandatory fields. Documented as intentional.

## Invariants established or confirmed

New invariants I46–I52 from the plan, all established:

- **I46** — Autofix-cycle fixtures live under `test/fixtures/autofix/<maintainer-name>/`. Each fixture declares `inspector`, `finding`, `maintainer`, `resolution` in its YAML front-matter as mandatory symbol fields interned in `:atelier`.
- **I47** — Autofix-cycle fixtures have exactly three body documents: input source, expected finding slots plist, expected fixed code.
- **I48** — SYNTAX-INSPECTOR fixtures compare via `(equal expected-form result-form)`; LINE-INSPECTOR fixtures compare via `string=` on raw text.
- **I49** — Every autofix-cycle fixture asserts self-idempotency at N=1.
- **I50** — *Scope adjusted during execution.* Every autofix-cycle fixture **whose inspector operates at the :SYNTAX level** has its expected fixed code asserted as a `read ⟫ pretty-print-form` fixed point. Line-inspector fixtures are excluded. Maintainers that do not fit the fixture format (e.g. `fix-mixed-indentation`) use ad-hoc `define-testcase` tests.
- **I51** — Finding slot checks use plists with slot reader names or synthetic accessors (`:cst-node-raw`, `:observation-matches`, `:source-text-substring`).
- **I52** — `validate-one-autofix-cycle-fixture` is the sole autofix-cycle test function; legacy `validate-one-maintainer-fixture` is removed.

I1–I17, I31–I38 from prior slices: confirmed, no stretches or violations.

## Plan deviations

All deviations were surfaced to the product owner before acting; none silent.

1. **Fixture count: plan said 10 existing fixtures, reality is 6.** The 4 missing maintainers (`fix-labels-to-flet`, `fix-header-line`, `fix-footer-line`, `fix-project-identification`) never had discoverable fixtures — they are covered by dedicated ad-hoc testcases in `test/maintainers/`. Not a blocker, reduces migration workload.

2. **Pretty-printer cross-population scope correction.** The plan and slice.md originally said "every fixture's expected fixed code is a `read ⟫ pretty-print` fixed point." Execution revealed that this is not achievable for (a) line-level fixtures whose expected text contains semantically meaningful whitespace, and (b) text-resolution maintainers that emit non-canonical text. Surfaced to user; Option A accepted: **scope the cross-population to fixtures whose inspector operates at the :SYNTAX level**, and rewrite fix-bare-loop-keywords's expected fixed code to the pretty-printer's canonical multi-line form (AST-equal to the single-line text the text-resolution produces, so the primary assertion still holds). slice.md S4 wording and implementation-1.md invariant I50 updated in place.

3. **fix-mixed-indentation fixture removed entirely.** Per user directive: "discoverable fixtures are a convenience, not a goal." The fixture's expected output `  (defvar *x* 1)` had semantically meaningful leading whitespace that could never be a `read ⟫ pretty-print` fixed point, and the existing `test/maintainers/fix-mixed-indentation.lisp` already provides full coverage via a synthetic finding. The fixture was removed via `git rm -f`; its directory is gone from both the source tree and the autofix-cycle discovery.

4. **fix-bare-loop-keywords expected fixed code rewritten to multi-line.** The original fixture's expected was a single line `(loop :for item :in items :when (...) :collect (...))`. The pretty-printer's canonical LOOP form wraps clauses across lines. Rewrote the expected to the multi-line form; the maintainer is a text-resolution that actually produces single-line output, but since the primary assertion is at the FORM level (AST equality via `read-from-string`), both read to the same AST and the assertion still passes. The pretty-printer fixed-point assertion now also passes for this fixture.

## New risks discovered

1. **Text-resolution drift from pretty-printer canonical form.** Text-resolution maintainers at the syntax level can produce output that is AST-equal to the pretty-printer canonical form but not string-equal. The pretty-printer cross-population catches this for fixture-covered cases, but there is no guarantee for maintainers without fixtures. **Suggested mitigation:** a future slice could add a global invariant test that runs every registered syntax-level maintainer on a corpus of synthetic inputs and asserts that the output is a `read ⟫ pretty-print` fixed point.

2. **Live-image ASDF reload pitfall.** Force-loading `org.melusina.atelier/test` from a live SBCL image fails with a package-variance error because the image has accumulated dozens of exported test symbols from incremental `define-testcase` calls, and the new DEFPACKAGE would "un-export" them. Workaround during execution: run the full regression in a fresh SBCL subprocess via `sbcl --no-userinit --script`. **Suggested mitigation:** document as a memory entry alongside the existing live-image pitfalls, and consider using `sb-ext:*on-package-variance*` configuration or a helper function that unexports stale symbols before reload.

## Technical decisions made

- **`autofix-cycle-fixture-error` condition** for fixture load errors, with clean human-readable report. Allows the loader to skip malformed fixtures (including the 20 carried-over fix-line-too-long files) with a clear reason instead of crashing.
- **Synthetic plist accessors** (`:cst-node-raw`, `:observation-matches`, `:source-text-substring`) instead of exposing `slot-value` at the fixture level. Keeps fixture authors on a small, stable vocabulary of assertion keys and allows substring-match assertions without regex.
- **`autofix-cycle-comparison-level`** as a named function dispatching on `inspector-level`. Future additions (e.g. a `:region` level) have a clear insertion point.
- **`run-autofix-cycle`** extracted as a standalone helper so the primary pass and the self-idempotency second pass share exactly one code path. Keeps the idempotency assertion airtight.
- **Directory rename via `git mv`** rather than copy + delete. Preserves file history so `git log -- test/fixtures/autofix/fix-earmuffs/baseline.text` still shows the original slice 005/006 commits.
- **CLAUDE.md updated** to reflect the new fixture layout, the autofix-cycle test protocol, the N=1 self-idempotency contract, and the rationale for when to use ad-hoc testcases instead of discoverable fixtures.

## Notes for product-ownership retrospective

- **Stories remaining undelivered in this slice:** none — all seven delivered.
- **Snail tests requiring manual confirmation before slice closes:** none.
- **Recommended for retrospective:**
  - S4 scope correction is a good case study in hypothesis testing: the original "every fixture is a fixed point" framing was an over-reach, and execution surfaced the real shape of the property (syntax-inspector-scoped). The hypothesis was still supported — maintainers are self-idempotent — but its scope was narrower than initially claimed.
  - The decision to remove fix-mixed-indentation from the fixture set (rather than force-fit it) validated the principle "discoverable fixtures are a convenience, not a goal." Worth making this an explicit design maxim in definition.md.
  - Pipeline idempotency (the deferred goal) now has a concrete starting point: we have per-maintainer idempotency as a checked property, and the pretty-printer fixed-point as an additional constraint on syntax-level maintainers. The next step toward pipeline idempotency is a cross-maintainer check.
  - The two unrelated `defun-key-params.text` and `long-loop.text` fixture diffs in the working tree at the start of this session were carried through by `git mv` but not touched by execution. They remain staged (via the rename detection) with working-tree modifications and need the product owner's decision before committing the phase's work.

## Files changed

**New files (source):**
- `src/inspectors/check-single-branch-if.lisp`
- `src/inspectors/check-single-form-progn.lisp`
- `src/inspectors/check-when-not.lisp`

**New files (test):**
- `test/inspectors/check-single-branch-if.lisp`
- `test/inspectors/check-single-form-progn.lisp`
- `test/inspectors/check-when-not.lisp`

**New files (fixtures):**
- `test/fixtures/inspector/check-single-branch-if/{when,unless,implicit-nil,clean}.lisp`
- `test/fixtures/inspector/check-single-form-progn/{baseline,clean,good}.lisp`
- `test/fixtures/inspector/check-when-not/{baseline,clean}.lisp`

**Modified (source):**
- `src/finding.lisp` — 3 new finding classes via `define-findings`
- `src/package.lisp` — 3 new finding exports, 3 new inspector exports

**Modified (test):**
- `test/package.lisp` — import `assert-equal` from confidence
- `test/utilities.lisp` — `autofix-cycle-fixture-error` condition, `read-autofix-cycle-fixture`, `discover-autofix-cycle-fixtures`, `autofix-cycle-fixture` accessor, `:autofix` kind on `fixture`; legacy `maintainer-fixture`, `discover-maintainer-fixtures`, `read-maintainer-fixture` and `:maintainer` kind removed
- `test/autofix.lisp` — `autofix-cycle-finding-accessor`, `autofix-cycle-finding-slot-check`, `run-autofix-cycle`, `autofix-cycle-comparison-level`, `autofix-cycle-results-equal-p`, `validate-one-autofix-cycle-fixture`, `validate-autofix-cycle-fixtures`, `validate-autofix-cycle-fixtures-as-pretty-printer-fixed-points`; legacy `validate-one-maintainer-fixture` and `validate-maintainer-fixtures` removed; `testsuite-autofix` entry updated

**Modified (fixtures — migrated from 2-part to 4-part format):**
- `test/fixtures/autofix/fix-earmuffs/baseline.text`
- `test/fixtures/autofix/fix-constant-naming/baseline.text`
- `test/fixtures/autofix/fix-bare-lambda/baseline.text`
- `test/fixtures/autofix/fix-bare-loop-keywords/baseline.text` (expected rewritten to pretty-printer canonical multi-line)
- `test/fixtures/autofix/fix-trailing-whitespace/baseline.text`

**Removed (per user directive):**
- `test/fixtures/autofix/fix-mixed-indentation/` (entire directory)

**Moved (as-is, silently skipped by new loader):**
- All 20 `test/fixtures/autofix/fix-line-too-long/*.text` files

**Modified (build):**
- `org.melusina.atelier.asd` — registered 3 new inspector source files and 3 new test files in both primary and testsuite modules

**Modified (docs):**
- `CLAUDE.md` — inspector count (13→16), fixture directory layout, autofix-cycle test protocol, self-idempotency invariant, pretty-printer cross-population rule, ad-hoc-testcase guidance
- `product/slice/007-maintainer-and-inspector-expansion/slice.md` — S4 scope correction during execution (surfaced to user, approved)
- `product/slice/007-maintainer-and-inspector-expansion/implementation-1.md` — I50 scope correction during execution
