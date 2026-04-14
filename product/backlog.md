# Backlog: Atelier
**Last updated:** 2026-04-14

The roadmap is a projection of the backlog's top items into time horizons. It does not introduce new items — every item in Now or Next must appear in the backlog.

| Rank | Item | Type | Source | Effort | Ready | Notes |
|-----:|------|------|--------|:------:|:-----:|-------|
| 1 | Finding/resolution schema (FINDING, FILE-FINDING, LINE-FINDING, REGION-FINDING, SYNTAX-FINDING, resolutions) | New capability | Vision 2026-04-05 | M | ✓ | **Delivered (slice 001)** |
| 2 | Inspector/maintainer registry (define-inspector, define-maintainer, supersedes, prepare-resolution) | New capability | Vision 2026-04-05 | M | ✓ | **Delivered (slice 002)** |
| 3 | ASDF integration (project-configuration, linter-configuration, linter-op) | New capability | Maintainer 2026-04-05 | M | ✓ | **Delivered (slice 002)** |
| 4 | First inspectors: file-header / SPDX (CL, Shell, HCL, Elisp) | Completion | README + vision | S | ✓ | **Delivered (slice 002)** |
| 5 | Linter: line-level CL inspectors (trailing whitespace, mixed indentation) | Completion | Vision 2026-04-05 | S | ✓ | |
| 6 | Linter: CST-level CL inspectors (earmuffs, function body length) | New capability | Vision 2026-04-05 | M | — | Blocked on #10. |
| 7 | Linter: ShellCheck wrapper inspector | New capability | Vision 2026-04-05 | S | ✓ | |
| 8 | Linter: Elisp byte-compiler and package-lint wrappers | New capability | Vision 2026-04-05 | S | ✓ | |
| 9 | LLM-driven maintainer (AGENT-RESOLUTION, structured prompt) | New capability | Vision 2026-04-05 | M | ✓ | |
| 10 | Code formatter for CL (idempotent, REPL and CLI) | New capability | Vision 2026-04-05 | L | — | Blocked on pretty-printer decision. |
| 11 | CLI thin wrapper (atelier lint, atelier fix, atelier format) | New capability | Maintainer 2026-04-05 | S | — | Depends on linter + formatter. |
| 12 | Documentation generation (HTML + machine-readable index) | New capability | Vision 2026-04-05 | L | — | |
| 13 | Project templates: extend for Elisp, Shell, HCL | Improvement | README + vision | S | ✓ | |
| 14 | Linter: tflint and terraform validate wrappers | New capability | Vision 2026-04-05 | S | ✓ | |
| 15 | Configuration file tie-breaking for superseding conflicts | New capability | Vision 2026-04-05 | M | — | Depends on #3. |

## Under consideration

| Item | Source | Open question |
|------|--------|---------------|
| Pretty-printer for syntax write-back | Vision 2026-04-05 | Eclector + trivial-formatter vs. custom emitter. Must preserve comments and reader macros. Decision needed before #6 and #10. |
| Code coverage instrumentation | Vision 2026-04-05 | sb-cover is SBCL-only. Is portability required? |
| Profiling instrumentation | Vision 2026-04-05 | Same portability question. |
| ASDF:COMPONENT for CFFI bridges | Vision 2026-04-05 | Scope: one .c file, one .so, reload. Not general C build. |
| ASDF:COMPONENT for Terraform | Vision 2026-04-05 | Stack = C4 Container, Module = C4 Component. |

## Rejected

| Item | Reason |
|------|--------|
| MCP server | Extracted to org.melusina.mcp as independent project. |
| Editor plugins (SLIME/SLY/Eglot) now | Aspirational. |
| Package management | Belongs to Quicklisp/OCICL/ASDF. |
| Build system replacement | Atelier consumes ASDF, not replaces. |
| Language support beyond CL, Elisp, Shell, HCL | Requires deliberate scope expansion. |
| Complex C/C++ build graphs | Out of scope. CFFI bridge pattern only. |

## Revision History

| Date | Change |
|------|--------|
| 2026-04-05 | Initial backlog from vision session and README analysis |
| 2026-04-05 | Added REGION-FINDING, ASDF components, external tool wrappers |
| 2026-04-09 | Slice 008: removed line-length inspector |
| 2026-04-14 | MCP server extracted to org.melusina.mcp. MCP backlog items (#5-#10 old numbering) removed. Renumbered remaining items. |
