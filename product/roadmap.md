# Roadmap: Atelier
**Last updated:** 2026-04-16

The roadmap is a projection of the backlog's top items into time horizons. It does not introduce new items — every item in Now or Next must appear in the backlog.

## Now (in progress)

*(No slice currently in progress.)*

## Next (intended for the following 1–3 slices)

| Item | Type | Goal addressed | Notes |
|------|------|----------------|-------|
| Pretty-printer / SLIME-SLY compatibility (#1) | Improvement | G2, G4 | Align pprint dispatch with Emacs indentation; `.dir-locals.el`. Unblocks code formatter. |
| Trivia pattern matching for inspectors/maintainers (#2) | Improvement | G2 | Spike then migration. More declarative inspector/maintainer code. |
| Code formatter for CL (#3) | New capability | G4 | Depends on #1. Idempotent, REPL+CLI. |
| CLI thin wrapper (#4) | New capability | G2, G4 | Depends on #3. `atelier lint`, `atelier fix`, `atelier format`. |

## Later (probable but not yet scheduled)

| Item | Goal addressed | Notes |
|------|----------------|-------|
| Projectional editor capabilities (#5) | G4 | Design spike needed. Form-level CRUD, file ops, future shell/terraform. |
| LLM-driven maintainer (#6) | G3 | AGENT-RESOLUTION with structured prompt. |
| ShellCheck wrapper inspector (#7) | G2 | External tool integration via REGION-FINDING / LINE-FINDING. |
| Elisp byte-compiler + package-lint wrappers (#8) | G2 | External tool integration. |
| tflint / terraform validate wrappers (#9) | G2 | External tool integration. |
| Project templates: extend for Elisp, HCL (#10) | G2 | Shell templates already exist. |
| Configuration tie-breaking (#11) | G2 | Superseding conflict resolution in linter-configuration. |
| Documentation generation (#12) | G6 | HTML + machine-readable index. |

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
| [009-project-structure-and-hooks](slice/009-project-structure-and-hooks/slice.md) | New capability | G2 | ✅ |

## Considering (may or may not happen — feedback welcome)

| Item | Open question |
|------|---------------|
| Code coverage (#13) | sb-cover is SBCL-only. Is portability required? |
| Profiling (#14) | Same portability question. |
| ASDF:COMPONENT for CFFI bridges (#15) | Scope: one source file, one .so, reload into image. |
| ASDF:COMPONENT for Terraform (#16) | terraform-file, terraform-module (C4 Component), terraform-stack (C4 Container). |

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
| 2026-04-14 | Slice 009 (project structure + hooks) added to Now. |
| 2026-04-14 | Slice 009 completed, moved to Completed. |
| 2026-04-16 | Backlog cleanup. Next: pretty-printer/SLIME-SLY (#1), Trivia pattern matching (#2), code formatter (#3), CLI wrapper (#4). Later: projectional editor, LLM maintainer, external tool wrappers, templates, config tie-breaking, doc generation. |
