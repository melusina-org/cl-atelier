# Roadmap: Atelier
**Last updated:** 2026-04-06

## Now (in progress)

| Slice | Type | Goal addressed | Description |
|-------|------|----------------|-------------|
| [003-line-level-inspectors](slice/003-line-level-inspectors/slice.md) | Bet | G2 | Trailing whitespace, line length, mixed indentation inspectors. Supersede legacy codestyle-0002. |

## Completed

| Slice | Type | Goal addressed | Verdict |
|-------|------|----------------|:-------:|
| [001-finding-resolution-schema](slice/001-finding-resolution-schema/slice.md) | Bet | G7 | ✅ |
| [002-asdf-integration-and-file-inspectors](slice/002-asdf-integration-and-file-inspectors/slice.md) | Bet | G2 | ✅ |

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
