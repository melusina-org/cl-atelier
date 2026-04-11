# Roadmap: Atelier
**Last updated:** 2026-04-11 (slice 009 closure)

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
| [008-remove-line-length-inspector](slice/008-remove-line-length-inspector/slice.md) | Maintenance | — | ✅ |
| [009-mcp-skeleton](slice/009-mcp-skeleton/slice.md) | New capability | G5 | ✅ |

## Next (intended for the following 1–3 slices)

| Item | Type | Goal addressed | Notes |
|------|------|----------------|-------|
| 010 — MCP child image, eval, package/symbol introspection (backlog #6) | New capability | G5 | Spawns SBCL child via `socketpair(2)` to in-image SWANK. Adds `closer-mop` dependency. Lint+autofix runs before every eval. Ships `lisp://packages/...` resources. |
| 011 — MCP debugger and restarts (backlog #7) | New capability | G5 | Live sldb-equivalent over the slice-010 transport. |
| 012 — MCP ASDF + Quicklisp + Confidence test runner (backlog #8) | New capability | G5 | `asdf-operate`, `quickload`, `where-is-system`, `system-apropos`, plus Confidence integration via the `define-testcase` symbol-property convention. |

## Later (probable but not yet scheduled)

| Item | Goal addressed | Notes |
|------|----------------|-------|
| 013 — MCP documentation: describe/apropos/hyperspec (with X3J13 issues), source-location, compile-with-notes, disassemble, macroexpand (#9) | G5 | CLHS available locally via MacPorts; X3J13 issue documents exposed as `lisp://hyperspec/issues/<id>` resources. |
| 014 — MCP xref + inspector + trace + `who-tests`/`run-impacted` (#10) | G5 | `who-tests` filters xref callers to those carrying the Confidence testcase property — differentiator over generic Lisp MCP servers. |
| 015 — MCP refactorings: rename-symbol, rename-package, rename-system, unintern, unexport, remove-method, **lint-passthrough** (#11) | G5 | Lint-passthrough is the bridge between MCP server and Atelier linter. |
| 016 — MCP domain diagnostics: CFFI, bordeaux-threads, SBCL profiling (#12) | G5 | SBCL profiling is `#+sbcl`. |
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
| Web transcript viewer for MCP sessions (#32) | What concrete story needs HTML beyond what an MCP client renders from Markdown? If a story arrives, build as a separate `org.melusina.atelier/mcp/transcript-viewer` system; do not pull Hunchentoot into core `/mcp`. |
| SQLite project index of Lisp systems and findings (#33) | What story requires persistence that ASDF source-registry and Quicklisp local-projects do not provide? Revisit after slice 014 (xref) when persistence patterns become clearer. |
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
| Secret management / SSH key handling in MCP server (R6) | The MCP server inherits user privileges; adding a credential store buys nothing and adds attack surface. Per tech stack: identity over secrets. Decided 2026-04-10. |

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
| 2026-04-09 | Slice 008 completed — verdict ✅. Moved to Completed. 15 inspectors, 10 maintainers. 295/295 tests passing in fresh SBCL subprocess (baseline 299; four assertions removed across three testcases). Invariant I7 established ("Atelier does not police line length") — recommended for promotion to design principle #8 in `definition.md` at the next Steward revision. |
| 2026-04-10 | MCP design session. Slice 009 (MCP skeleton) added to Now. Slice 010–016 sketched in Next/Later as the MCP track. Backlog items #6–#12 rewritten to map onto the slice plan; items #13–#15 marked subsumed. Rejected: secret management in MCP server (R6). Under consideration: Hunchentoot-based web transcript viewer (#32), SQLite project index (#33). Architectural decisions: standalone `atelier-mcp` binary, `socketpair(2)` transport for slice 010, SWANK as in-image backend, sexp-canonical transcript with derived JSON and Markdown views, no Hunchentoot, no secret store, no SQLite. |
| 2026-04-11 | Slice 009 completed — verdict ✅ Supported. Moved to Completed. 6 tools, 3 concrete resources, 5 templates delivered via unified `define-tool` macro (17 helpers, each ≤25 lines). 184/184 MCP assertions + 295/295 base atelier assertions = 479/479 green in one fresh SBCL subprocess run. Plan amendment 1 during execution (`resources/list`/`resources/templates/list` split per spec) handled cleanly via append-not-rewrite protocol. Five new invariants (INV-12 to INV-16) proposed for Reviewer promotion. INV-4 upgraded from discipline to enforced suite property via subprocess-load test. |
