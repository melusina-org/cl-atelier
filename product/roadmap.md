# Roadmap: Atelier
**Last updated:** 2026-04-09 (slice 008 planning)

## Now (in progress)

_Nothing in progress._

## Completed

| Slice | Type | Goal addressed | Verdict |
|-------|------|----------------|:-------:|
| [001-finding-resolution-schema](slice/001-finding-resolution-schema/slice.md) | Bet | G7 | ✅ |
| [002-asdf-integration-and-file-inspectors](slice/002-asdf-integration-and-file-inspectors/slice.md) | Bet | G2 | ✅ |
| [003-line-level-inspectors](slice/003-line-level-inspectors/slice.md) | Bet | G2 | ✅ |
| [004-cst-inspectors](slice/004-cst-inspectors/slice.md) | Bet | G2 | ✅ |
| [005-autofix-pipeline](slice/005-autofix-pipeline/slice.md) | Bet | G2, G4 | ✅ |
| [006-linter-refinements](slice/006-linter-refinements/slice.md) | Bet | G2 | ✅ |
| [007-maintainer-and-inspector-expansion](slice/007-maintainer-and-inspector-expansion/slice.md) | Bet | G2 | ✅ |

## Next (intended for the following 1–3 slices)

| Item | Type | Goal addressed | Notes |
|------|------|----------------|-------|
| Line-level and CST-level CL inspectors (backlog #16, #17) | New capability | G2 | Validates full write-back pipeline. Pretty-printer decision needed before #17. |
| MCP server skeleton + REPL evaluation tool (backlog #5, #6) | New capability | G5 | Parallel track once schema is stable. Requires MCP design session to complete first. |

## Later (probable but not yet scheduled)

| Item | Goal addressed | Notes |
|------|----------------|-------|
| MCP: debugger restarts, image restart, system reload (#7, #8, #9) | G5 | Requires live debugger state accessible from MCP. |
| MCP: rename refactoring tools (#10, #11, #12) | G5 | Depends on composite-resolution pipeline being solid. |
| MCP: remove unexported symbols, CLOS introspection (#13, #14, #15) | G5 | |
| LLM-driven maintainer for any finding class (#21) | G3 | First-class MAINTAINER kind; structured prompt derived from observation + rationale + source context. |
| Line-level and CST-level CL inspectors (#16, #17) | G2 | Validates full write-back pipeline. Pretty-printer decision (#23) needed before #17. |
| ShellCheck, Elisp, and Terraform wrapper inspectors (#18, #19, #20) | G2 | External tools integrated via REGION-FINDING / LINE-FINDING. |
| Common Lisp code formatter (#22) | G4 | Depends on pretty-printer decision (#23). |
| CLI thin wrapper (#25) | G2, G4 | After linter and formatter are solid. |
| Documentation generation (#26) | G6 | |
| Pipeline idempotency (`lint-system :autofix t` whole-file fixed point) | G2, G3 | Stronger property than per-maintainer self-idempotency. Required for safe pre-commit hooks and CI gates. Depends on understanding cross-maintainer interactions; may require inspector ordering guarantees or explicit max-pass limits. See `slice/007-.../references/linter-convergence.md` for how Ruff and ESLint handle this. |

## Considering (may or may not happen — feedback welcome)

| Item | Open question |
|------|---------------|
| Code coverage (#28) | sb-cover is SBCL-only. Is portability required? |
| Profiling (#29) | Same portability question. |
| ASDF:COMPONENT for CFFI bridges (#30) | Scope: one source file, one .so, reload into image. Primary motivation is linting C/C++ through shared pipeline and driving compile+reload from ASDF. |
| ASDF:COMPONENT for Terraform (#31) | terraform-file, terraform-module (C4 Component), terraform-stack (C4 Container with verifiable interface contract). |
| Editor plugins (SLIME/SLY/Eglot) | Aspirational. No concrete plan. Would Atelier expose an additional protocol, or rely purely on MCP? |
| Quicklisp distribution, OCICL publication, MELPA packages | Aspirational. When does this become necessary — at v1.0, or earlier? |

## Out of scope (not planned — listed to prevent repeated requests)

| Item | Reason |
|------|--------|
| Package management / dependency resolution | Belongs to Quicklisp/OCICL/ASDF. |
| Build system replacement | Atelier consumes ASDF; it does not replace it. |
| Complex C/C++ build graphs | CFFI bridge pattern only (one file, one .so). |
| Language support beyond CL, Elisp, Shell, HCL | Requires explicit scope expansion. |
| Editor plugins (now) | Atelier exposes MCP; editor integration is the editor's concern for now. |

## Revision History

| Date | Change |
|------|--------|
| 2026-04-05 | Initial roadmap from vision session |
| 2026-04-05 | Added: LLM-driven maintainer to Later; Elisp language support throughout; editor plugins, CFFI and Terraform ASDF components, MELPA/OCICL publication to Considering; new MCP tools to Later; REGION-FINDING and external tool wrappers to Later |
| 2026-04-06 | Slice 001 (finding/resolution schema + registry) moved from Next to Now |
| 2026-04-06 | Slice 001 completed — verdict ✅ Supported. Moved to Completed |
| 2026-04-06 | Slice 002 (ASDF integration + file inspectors) moved from Next to Now |
| 2026-04-06 | Slice 002 completed — verdict ✅ Supported. Moved to Completed |
| 2026-04-06 | Slice 003 (line-level inspectors) added to Now |
| 2026-04-07 | Slice 003 completed — verdict ✅ Supported. Moved to Completed |
| 2026-04-07 | Slice 004 (CST-level inspectors) added to Now |
| 2026-04-07 | Slice 004 completed — verdict ✅ Supported. Moved to Completed |
| 2026-04-07 | Slice 005 (autofix pipeline + pretty-printer) added to Now. Resolves backlog #23 (pretty-printer decision: CL built-in pprint, not third-party) |
| 2026-04-07 | Slice 005 completed — verdict ✅ Validated. Moved to Completed. 7 automatic maintainers delivered (target was 6). |
| 2026-04-07 | Slice 006 (linter refinements) added and completed. Legacy system removed. SPDX headers, fixture reorg, autofix signalling, project configuration. 13 inspectors, 10 maintainers. |
| 2026-04-08 | Slice 007 (maintainer and inspector expansion) added to Now. fix-line-too-long maintainer, fixed-point assertion, IF-to-WHEN/UNLESS + PROGN + WHEN-NOT inspectors. |
| 2026-04-09 | Slice 007 rescoped: fix-line-too-long removed (design open, returned to Later). Slice now focuses on autofix-cycle fixture redesign, self-idempotency assertion (N=1), and three new CST inspectors. Pipeline idempotency added to Later as a long-term goal. Idempotency added as design principle #7 in definition.md. |
| 2026-04-09 | Slice 007 completed — verdict ✅ Supported. Moved to Completed. Delivered: `testsuite/fixtures/autofix/` directory with new 4-part fixture format; 5 migrated maintainer fixtures; 3 new diagnostic-only CST inspectors (check-single-branch-if, check-single-form-progn, check-when-not); N=1 self-idempotency invariant; pretty-printer fixed-point cross-population for syntax-inspector fixtures. Unanticipated: surfaced and fixed a latent `*current-line-vector*` defvar load-order bug that had been masked by stale fasls since slice 003–004. 299/299 tests passing in clean SBCL subprocess. 16 inspectors, 10 maintainers. |
| 2026-04-09 | Slice 008 planned: remove `check-line-length` inspector, `fix-line-too-long` maintainer, `line-too-long-finding` class, three testcases, and twenty carried-over fixtures. Rationale: adopt the gofmt position — the pretty-printer is Atelier's single authority on canonical Lisp text; a separate line-length reporter adds noise without correctness. Research at `product/reference/line-length-research.md`. |
