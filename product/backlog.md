# Backlog: Atelier
**Last updated:** 2026-04-05

## Active (next 1–3 slices)

| # | Item | Type | Source | Effort | Notes |
|---|------|------|--------|:------:|-------|
| 1 | Define and stabilise the finding/resolution schema: FINDING, FILE-FINDING, LINE-FINDING, REGION-FINDING, SYNTAX-FINDING, RESOLUTION, TEXT-RESOLUTION, SYNTAX-RESOLUTION, AGENT-RESOLUTION, COMPOSITE-RESOLUTION | New capability | Vision session 2026-04-05 | M | Prerequisite for all linter and maintainer work. Named-instance pattern implementation to be supplied by maintainer. |
| 2 | Define and stabilise the inspector/maintainer registry: define-inspector, define-maintainer, :supersedes, list-inspectors, list-maintainers, prepare-resolution generic | New capability | Vision session 2026-04-05 | M | Depends on #1. This is the 3rd-party extensibility contract — must be stable before any concrete inspector is written. |
| 3 | ASDF integration: atelier:project-configuration and atelier:linter-configuration component types; atelier:linter-op operation | New capability | Maintainer requirement 2026-04-05 | M | Depends on #1 and #2. project-configuration read by templates; linter-configuration read by linter-op. |
| 4 | First concrete inspector set: file-header / SPDX license identifier (CL, Shell, HCL, Elisp) | Completion | README + vision session | S | Depends on #2. File-level only — validates registry and write-back at simplest level. |
| 5 | MCP server skeleton: protocol, connection lifecycle, tool registration | New capability | Vision session 2026-04-05 | M | Can proceed in parallel with #3 and #4 once #1/#2 are done. MCP design session must complete before this slice starts. |

## Queued (clearly belongs, not yet scheduled)

| # | Item | Type | Source | Effort | Notes |
|---|------|------|--------|:------:|-------|
| 6 | MCP tool: REPL evaluation with structured result | New capability | Vision session 2026-04-05 | M | Depends on #5. |
| 7 | MCP tool: debugger restart selection | New capability | Vision session 2026-04-05 | M | Depends on #6. Requires live debugger state accessible from MCP connection. |
| 8 | MCP tool: Lisp image restart | New capability | Maintainer requirement 2026-04-05 | S | Depends on #5. |
| 9 | MCP tool: Lisp system reload | New capability | Maintainer requirement 2026-04-05 | S | Depends on #5. |
| 10 | MCP tool: rename symbol across a system | New capability | Vision session 2026-04-05 | L | Depends on #5. Requires syntax-finding + composite-resolution pipeline to be solid. |
| 11 | MCP tool: rename package | New capability | Vision session 2026-04-05 | L | Depends on #10. |
| 12 | MCP tool: rename system | New capability | Vision session 2026-04-05 | M | Depends on #11. |
| 13 | MCP tool: remove exported symbols not present in :export clause | New capability | Maintainer requirement 2026-04-05 | M | Depends on #5. Requires package definition inspection. |
| 14 | MCP tool: CLOS FIND-CLASS introspection | New capability | Maintainer requirement 2026-04-05 | S | Depends on #5. |
| 15 | MCP tool: CLOS FIND-METHOD and REMOVE-METHOD | New capability | Maintainer requirement 2026-04-05 | S | Depends on #14. |
| 16 | Linter: line-level inspectors for CL (trailing whitespace, line length, mixed indentation) | Completion | Vision session 2026-04-05 | S | Validates line-finding + text-resolution + write-back pipeline. |
| 17 | Linter: CST-level inspectors for CL (earmuffs convention, function body length) | New capability | Vision session 2026-04-05 | M | Validates syntax-finding + Eclector integration + syntax-resolution write-back. Blocked on #22. |
| 18 | Linter: ShellCheck wrapper inspector (LINE-FINDING / REGION-FINDING) | New capability | Vision session 2026-04-05 | S | Wraps ShellCheck output into shared finding pipeline. |
| 19 | Linter: Elisp byte-compiler and package-lint wrapper inspectors | New capability | Vision session 2026-04-05 | S | |
| 20 | Linter: tflint and terraform validate wrapper inspectors | New capability | Vision session 2026-04-05 | S | |
| 21 | LLM-driven maintainer: AGENT-RESOLUTION with structured prompt for any finding class | New capability | Vision session 2026-04-05 | M | Depends on #2. A first-class maintainer kind; participates in superseding like any other. |
| 22 | Code formatter for Common Lisp (idempotent, REPL and CLI) | New capability | Vision session 2026-04-05 | L | Blocked on #23 (pretty-printer decision). |
| 24 | Configuration file / project policy tie-breaking for superseding conflicts | New capability | Vision session 2026-04-05 | M | Depends on #3. Required for multi-maintainer projects. |
| 25 | CLI thin wrapper: atelier lint, atelier fix, atelier format | New capability | Maintainer requirement 2026-04-05 | S | Depends on linter and formatter being solid. |
| 26 | Documentation generation from source (HTML + machine-readable index) | New capability | Vision session 2026-04-05 | L | |
| 27 | Project templates: extend for Elisp, Shell, and HCL companion files | Improvement | README + vision session | S | CL template exists; Elisp, Shell, HCL templates to be added. |

## Under consideration (needs more information before deciding)

| # | Item | Source | Open question |
|---|------|--------|---------------|
| 23 | Pretty-printer for syntax write-back | Vision session 2026-04-05 | Eclector + trivial-formatter vs. custom emitter. Must preserve comments and reader macros not touched by transforms. Decision needed before #17 and #22. |
| 28 | Code coverage instrumentation (atelier:coverage-op) | Vision session 2026-04-05 | Which CL coverage library? sb-cover is SBCL-only. Is portability required? |
| 29 | Profiling instrumentation (atelier:profile-op) | Vision session 2026-04-05 | Same portability question as #28. |
| 30 | ASDF:COMPONENT for CFFI bridges (atelier:c-source-file, atelier:cpp-source-file) | Vision session 2026-04-05 | Aspirational. Scope: CFFI bridge pattern only (one source file, one .so, reload into image). Not a general C build system. |
| 31 | ASDF:COMPONENT for Terraform (atelier:terraform-file, atelier:terraform-module, atelier:terraform-stack) | Vision session 2026-04-05 | Aspirational. Stack = C4 Container with verifiable interface contract. Module = C4 Component. |

## Rejected (considered and decided against)

| # | Item | Reason |
|---|------|--------|
| R1 | Editor plugins (SLIME/SLY/Eglot) now | Aspirational future direction. No concrete plan. Atelier exposes MCP in the interim. |
| R2 | Package management / dependency resolution | Belongs to Quicklisp/OCICL/ASDF. Publication (Quicklisp, OCICL, MELPA) is aspirational future work. |
| R3 | Build system replacement | Atelier integrates with ASDF as a consumer, not a replacement. |
| R4 | Language support beyond CL, Elisp, Shell, HCL | Requires deliberate scope expansion decision. |
| R5 | Complex C/C++ build graphs | Out of scope. CFFI bridge pattern (one file, one .so) is aspirational; multi-target C projects are not. |

## Revision History

| Date | Change |
|------|--------|
| 2026-04-05 | Initial backlog from vision session and README analysis |
| 2026-04-05 | Added: REGION-FINDING to #1; ASDF project-configuration and linter-configuration to #3; Elisp to #4 and #27; MCP tools #8-#15 (image restart, system reload, unexported symbol removal, CLOS introspection); ShellCheck/Elisp/Terraform wrapper inspectors #18-#20; LLM-driven maintainer #21; CFFI and Terraform ASDF components #30-#31 |
