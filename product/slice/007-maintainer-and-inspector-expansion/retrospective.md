# Retrospective: Slice 007 — Autofix Cycle Redesign and New CST Inspectors

**Recorded:** 2026-04-09
**Delivered by:** Michaël Le Barbier (with Claude as engineering pair)
**Implementation phases:** 1 total — `implementation-1.md`

## Hypothesis Outcome

- **Assumption tested:** The `(inspector, finding, maintainer, resolution)` quadruple is the real unit under test in our autofix fixtures. Making it explicit in the fixture format — with structural (CST / AST) comparison and a self-idempotency assertion — will surface any latent bugs in existing maintainers without requiring changes to the write-back engine.
- **Leading indicator:** planned "10 migrated fixtures + 3 new CST inspectors + self-idempotency at N=1 holding for all 10." Actual: **5 migrated fixtures + 3 new CST inspectors + self-idempotency at N=1 holding for all 5.** The 10-count was a plan error — only 6 maintainer fixtures ever existed, and fix-mixed-indentation was removed from the fixture set per directive, leaving 5.
- **Verdict: ✅ Supported.** The test protocol works exactly as hypothesised. Zero non-idempotent maintainers. Zero write-back engine changes. The autofix-cycle format cleanly expresses the full diagnostic cycle and the fixture test harness runs inspect → finding-class check → slot check → maintain → resolution-class check → apply → primary assertion → idempotency pass → pretty-printer fixed-point cross-population, all without special-casing.
- **Kill criterion:** "More than one non-idempotent maintainer, or more than one fixture requiring format special-case" — **not triggered**. Zero non-idempotent maintainers and zero format special-cases on the maintainer side.
- **Stories delivered:** 7 of 7.
- **Quality criteria:** all three passing — self-idempotency on all migrated fixtures, pretty-printer fixed-point on all syntax-inspector fixtures (4/4), full regression green (299/299).

## Phase Delivery Summary

| Phase | Plan | Key deliverables | Deferred items |
|-------|------|-----------------|----------------|
| 1 | `implementation-1.md` | `test/fixtures/autofix/` directory replaces `maintainer/` via `git mv`; new 4-part fixture format with mandatory `inspector`/`finding`/`maintainer`/`resolution` front-matter; `validate-one-autofix-cycle-fixture` replaces `validate-one-maintainer-fixture`; N=1 self-idempotency assertion; pretty-printer cross-population for syntax-inspector fixtures; 5 existing fixtures migrated; 3 new CST inspectors (check-single-branch-if, check-single-form-progn, check-when-not) with 9 auto-discovered fixtures; latent bug fix (`*current-line-vector*` defvar moved from write-back.lisp to runner.lisp); post-implementation cleanup of 14 redundant test files, pruning of 10 others, and removal of 4 category-1 duplicate inspector fixtures. | fix-labels-to-flet autofix-cycle fixture (still covered by ad-hoc end-to-end testcase — could be added in a future slice for consistency). Unification of remaining inspector fixtures (diagnostic-only + clean cases) into the autofix/ directory — deferred as a future cleanup; none of the 14 remaining inspector fixtures are blocking. |

## What we learned

**The quadruple IS the right unit under test.** Every maintainer fixture now carries the `inspector → finding → maintainer → resolution` declaration explicitly in its front-matter, which immediately made failure messages more precise: a test failure now points at which link in the chain broke. The slot-check plist with synthetic accessors (`:cst-node-raw`, `:observation-matches`, `:source-text-substring`) proved flexible enough to cover both syntax-finding and line-finding assertions without new assertion primitives.

**The pretty-printer fixed-point assertion is more useful than anticipated, but narrower in scope than the plan claimed.** The initial plan language said "every fixture's expected fixed code is a `read ⟫ pretty-print` fixed point." Execution surfaced the real shape of the property: it only applies to fixtures whose inspector operates at the `:syntax` level. Line-level fixtures have semantically meaningful whitespace in their expected text (fix-mixed-indentation's `  (defvar *x* 1)`) that could never round-trip through the pretty-printer. Text-resolution maintainers at the syntax level (fix-bare-loop-keywords) needed their expected documents rewritten to the pretty-printer's canonical form — this is still AST-equal to what the maintainer produces, so the primary assertion passes, and the stronger fixed-point property now holds. **The pretty-printer is now enforced as the single authority on canonical Lisp text for syntax-level maintainers.**

**The slice surfaced a latent bug unrelated to its hypothesis, in an area the hypothesis did not predict.** After the redundant-test cleanup, the full regression ran in a clean SBCL subprocess and failed with a type error in `make-syntax-finding-from-form` — `*current-line-vector*` was NIL despite the `inspect-file` pathname method's `(let ((*current-line-vector* ...)))`. Root cause: the variable was `defvar`'d in `write-back.lisp` but first used in `runner.lisp`, which is loaded earlier in the ASDF `:serial` order. At compile-file time for runner.lisp, the symbol was not yet known as `special`, so SBCL compiled the `let` bindings as **lexical**, not dynamic. The bug had been present since slices 003–004 but was masked during interactive development because the live image always had write-back.lisp loaded before runner.lisp was recompiled. **All prior slice regressions had been running against stale fasls that happened to succeed.** The fix was one line: move the defvar to runner.lisp.

**The discoverable-fixture protocol is a convenience, not a goal in itself.** fix-mixed-indentation's expected output contained semantically meaningful leading whitespace that could not be a `read ⟫ pretty-print` fixed point. Rather than force-fit the fixture format, the decision was to remove that one fixture from the discoverable set and cover the maintainer via an ad-hoc `define-testcase` in `test/maintainers/`. This principle — "fixtures are a convenience, not a goal" — is now explicit in CLAUDE.md and in the slice's invariants.

**Self-idempotency at N=1 is the right contract for per-maintainer tests.** Pipeline idempotency (whole-file `lint-system :autofix t` reaching a fixed point after one pass) is a strictly stronger property that requires understanding cross-maintainer interactions. A research reference on how Ruff and ESLint handle the problem (`references/linter-convergence.md`) confirmed that both linters use hard iteration caps — Ruff's unnamed `MAX_ITERATIONS` with "Failed to converge" reporting, ESLint's `MAX_AUTOFIX_PASSES = 10` with "Circular fixes detected" warnings — rather than trying to break cycles. Atelier's position is: per-maintainer N=1 self-idempotency is enforced today as an invariant; pipeline idempotency is on the roadmap as a long-term goal.

## Plan-reality discrepancies

1. **Fixture count: planned 10, actual 6.** The plan assumed all 10 registered maintainers had discoverable fixtures. Reality: only 6 did — fix-labels-to-flet, fix-header-line, fix-footer-line, and fix-project-identification never had fixtures in the old format and continue to use ad-hoc `define-testcase` tests. This does not invalidate the hypothesis; it just shrinks the migration surface.
2. **S4 scope was narrowed mid-execution.** The pretty-printer cross-population was scoped from "every fixture" to "syntax-inspector fixtures only" after execution revealed the line-inspector expected documents cannot be canonical forms and the text-resolution fixtures needed their expected documents rewritten to canonical pretty-printer form. The scope correction was surfaced to the product owner and approved (Option A).
3. **fix-mixed-indentation fixture removed entirely.** Per directive after the scope correction: the fixture's expected output (whitespace-significant text) did not fit the format, and the existing ad-hoc testcase already provided full coverage. The principle "discoverable fixtures are a convenience, not a goal" was added to CLAUDE.md.
4. **Post-implementation cleanup was larger than planned.** After the three new CST inspectors were delivered, a broader audit of `test/inspectors/` and `test/maintainers/` identified 14 fully redundant test files (fixture-wrappers or trivial registration checks) and 10 partially redundant files (registration checks mixed with meaningful ad-hoc content). All were cleaned up. A further 4 category-1 duplicate inspector fixtures were removed after a question-and-answer round on whether the two fixture directories should be unified. Test assertion count: 357 (phase-1 stale-cache number) → 303 (fresh run after post-cleanup, before category-1 deletions) → **299 (final)**.

## Research

N/A — verdict is ✅ Supported, no unexpected results to investigate.

## Impact on next prioritization

- **Pipeline idempotency (Later)** has a concrete starting point: per-maintainer N=1 self-idempotency is now enforced as an invariant. The next step toward pipeline idempotency is a cross-maintainer check — running `lint-system :autofix t` twice on a representative corpus and asserting no changes on the second pass.
- **fix-line-too-long (Later)** is unchanged — still blocked on the text-vs-CST comparison design question. The 20 carried-over text fixtures are now in `test/fixtures/autofix/fix-line-too-long/` and silently skipped by the loader until they are migrated to the new format.
- **Line-level and CST-level CL inspectors (Next)** — the autofix-cycle format is proven and ready. Any new CST inspector can immediately emit auto-discovered fixtures in the new format. A mild recommendation for the next planner: consider unifying the remaining `test/fixtures/inspector/` fixtures into the autofix directory, either by loosening the autofix-cycle format to support diagnostic-only and clean cases, or by accepting the two directories as a known intentional split. Decision deferred.
- **MCP server skeleton (Next)** — no dependency on slice 007 outputs.
- **VEC confidence:** raise confidence on all G2 inspector/maintainer initiatives from the success of this slice; no change to other goals.

## Maturity tracker changes

- **CST-level Inspection:** Foundation → Foundation. *Three new CST inspectors added; level remains Foundation because the capability is not yet Capable (Capable would require >15 CST inspectors and full pretty-printer interop for fixtures). Last changed: 2026-04-09.*
- **Autofix / Write-back:** Foundation → Foundation. *Test protocol sharpened with N=1 self-idempotency invariant and pretty-printer fixed-point cross-population; no engine changes. Level remains Foundation. Last changed: 2026-04-09.*

No level transitions this slice; both touched capabilities remain at Foundation with refined test invariants and three additional CST inspectors.
