# Roadmap: Atelier
**Last updated:** 2026-04-14

The roadmap is a projection of the backlog's top items into time horizons. It does not introduce new items — every item in Now or Next must appear in the backlog.

## Now (in progress)

*(No slice currently in progress.)*

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

## Next (intended for the following 1-3 slices)

| Item | Type | Goal addressed | Notes |
|------|------|----------------|-------|
| LLM-driven maintainer for any finding class (#9) | New capability | G3 | First-class MAINTAINER kind; structured prompt derived from observation + rationale + source context. |
| Line-level and CST-level CL inspectors (#5, #6) | Completion | G2 | Validates full write-back pipeline. |

## Later (probable but not yet scheduled)

| Item | Goal addressed | Notes |
|------|----------------|-------|
| ShellCheck, Elisp, and Terraform wrapper inspectors (#7, #8, #9) | G2 | External tools integrated via REGION-FINDING / LINE-FINDING. |
| Common Lisp code formatter (#10) | G4 | |
| CLI thin wrapper (#11) | G2, G4 | After linter and formatter are solid. |
| Documentation generation (#12) | G6 | |
| Pipeline idempotency (`lint-system :autofix t` whole-file fixed point) | G2, G3 | Stronger property than per-maintainer self-idempotency. Required for safe pre-commit hooks and CI gates. |

## Considering (may or may not happen — feedback welcome)

| Item | Open question |
|------|---------------|
| Code coverage (#13) | sb-cover is SBCL-only. Is portability required? |
| Profiling (#14) | Same portability question. |
| ASDF:COMPONENT for CFFI bridges (#15) | Scope: one source file, one .so, reload into image. |
| ASDF:COMPONENT for Terraform (#16) | terraform-file, terraform-module (C4 Component), terraform-stack (C4 Container with verifiable interface contract). |
| Editor plugins (SLIME/SLY/Eglot) | Aspirational. No concrete plan. |
| Quicklisp distribution, OCICL publication, MELPA packages | Aspirational. When does this become necessary — at v1.0, or earlier? |

## Out of scope (not planned — listed to prevent repeated requests)

| Item | Reason |
|------|--------|
| MCP server | Extracted to org.melusina.mcp. |
| Package management / dependency resolution | Belongs to Quicklisp/OCICL/ASDF. |
| Build system replacement | Atelier consumes ASDF; it does not replace it. |
| Complex C/C++ build graphs | CFFI bridge pattern only (one file, one .so). |
| Language support beyond CL, Elisp, Shell, HCL | Requires explicit scope expansion. |
| Editor plugins (now) | Aspirational — no concrete plan. |

## Revision History

| Date | Change |
|------|--------|
| 2026-04-05 | Initial roadmap from vision session |
| 2026-04-05 | Added: LLM-driven maintainer to Later; Elisp language support throughout; editor plugins, CFFI and Terraform ASDF components, MELPA/OCICL publication to Considering; REGION-FINDING and external tool wrappers to Later |
| 2026-04-06 | Slices 001-008 completed over sessions through 2026-04-09 |
| 2026-04-14 | MCP server extracted to org.melusina.mcp. Slices 009-015 removed. MCP moved to out-of-scope. Backlog renumbered. |
