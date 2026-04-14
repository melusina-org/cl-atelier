# Backlog: Atelier
**Last updated:** 2026-04-14

The roadmap is a projection of the backlog's top items into time horizons. It does not introduce new items — every item in Now or Next must appear in the backlog.

| Rank | Item | Type | Source | Effort | Ready | Notes |
|-----:|------|------|--------|:------:|:-----:|-------|
| 1 | Finding/resolution schema (FINDING, FILE-FINDING, LINE-FINDING, REGION-FINDING, SYNTAX-FINDING, resolutions) | New capability | Vision 2026-04-05 | M | ✓ | **Delivered (slice 001)** |
| 2 | Inspector/maintainer registry (define-inspector, define-maintainer, supersedes, prepare-resolution) | New capability | Vision 2026-04-05 | M | ✓ | **Delivered (slice 002)** |
| 3 | ASDF integration (project-configuration, linter-configuration, linter-op) | New capability | Maintainer 2026-04-05 | M | ✓ | **Delivered (slice 002)** |
| 4 | First inspectors: file-header / SPDX (CL, Shell, HCL, Elisp) | Completion | README + vision | S | ✓ | **Delivered (slice 002)** |
| 5 | MCP server skeleton (protocol, tools, resources, transcript) | New capability | Vision 2026-04-05 | M | ✓ | **Delivered (slice 009)** |
| 6 | MCP child image lifecycle, eval, package introspection | New capability | Vision 2026-04-05 | L | ✓ | **Delivered (slice 010)** |
| 7 | MCP debugger access, restart selection | New capability | Vision 2026-04-05 | M | ✓ | **Delivered (slice 011)** |
| 8 | MCP ASDF/Quicklisp/Confidence integration | New capability | Maintainer 2026-04-05 | M | ✓ | **Delivered (slice 012)** |
| 9 | MCP documentation tools (apropos, hyperspec, macroexpand, disassemble, compile) | New capability | Maintainer 2026-04-05 | M | ✓ | **Delivered (slice 013)** |
| 10 | MCP xref, CLOS inspector, trace, who-tests, run-impacted | New capability | Vision 2026-04-05 | M | ✓ | **Delivered (slice 014)** |
| 11 | MCP reliability: SWANK health, output capture, test speed, kernel for reload | Improvement | Session 2026-04-14 | M | ✓ | Slice 015 planned. Broken-pipe recovery, separate stdout/stderr/trace capture, shared test child, MCP kernel system. |
| 12 | MCP refactorings: rename-symbol, rename-package, lint-passthrough | New capability | Vision 2026-04-05 | L | — | Depends on #11. |
| 13 | MCP domain diagnostics: CFFI, bordeaux-threads, SBCL profiling | New capability | Vision 2026-04-05 | M | — | Depends on #11. |
| 14 | Linter: line-level CL inspectors (trailing whitespace, mixed indentation) | Completion | Vision 2026-04-05 | S | ✓ | |
| 15 | Linter: CST-level CL inspectors (earmuffs, function body length) | New capability | Vision 2026-04-05 | M | — | Blocked on #19. |
| 16 | Linter: ShellCheck wrapper inspector | New capability | Vision 2026-04-05 | S | ✓ | |
| 17 | Linter: Elisp byte-compiler and package-lint wrappers | New capability | Vision 2026-04-05 | S | ✓ | |
| 18 | Linter: tflint and terraform validate wrappers | New capability | Vision 2026-04-05 | S | ✓ | |
| 19 | Code formatter for CL (idempotent, REPL and CLI) | New capability | Vision 2026-04-05 | L | — | Blocked on pretty-printer decision. |
| 20 | LLM-driven maintainer (AGENT-RESOLUTION, structured prompt) | New capability | Vision 2026-04-05 | M | ✓ | |
| 21 | Configuration file tie-breaking for superseding conflicts | New capability | Vision 2026-04-05 | M | — | Depends on #3. |
| 22 | CLI thin wrapper (atelier lint, atelier fix, atelier format) | New capability | Maintainer 2026-04-05 | S | — | Depends on linter + formatter. |
| 23 | Documentation generation (HTML + machine-readable index) | New capability | Vision 2026-04-05 | L | — | |
| 24 | Project templates: extend for Elisp, Shell, HCL | Improvement | README + vision | S | ✓ | |

## Under consideration

| Item | Source | Open question |
|------|--------|---------------|
| Pretty-printer for syntax write-back | Vision 2026-04-05 | Eclector + trivial-formatter vs. custom emitter. Must preserve comments and reader macros. Decision needed before #15 and #19. |
| Code coverage instrumentation | Vision 2026-04-05 | sb-cover is SBCL-only. Is portability required? |
| Profiling instrumentation | Vision 2026-04-05 | Same portability question. |
| ASDF:COMPONENT for CFFI bridges | Vision 2026-04-05 | Scope: one .c file, one .so, reload. Not general C build. |
| ASDF:COMPONENT for Terraform | Vision 2026-04-05 | Stack = C4 Container, Module = C4 Component. |
| Web transcript viewer | Design 2026-04-10 | What story needs HTML beyond native MCP client Markdown? |
| SQLite project index | Design 2026-04-10 | What story requires persistence beyond ASDF source-registry? |

## Rejected

| Item | Reason |
|------|--------|
| Editor plugins (SLIME/SLY/Eglot) now | Aspirational. MCP surface in interim. |
| Package management | Belongs to Quicklisp/OCICL/ASDF. |
| Build system replacement | Atelier consumes ASDF, not replaces. |
| Language support beyond CL, Elisp, Shell, HCL | Requires deliberate scope expansion. |
| Complex C/C++ build graphs | Out of scope. CFFI bridge pattern only. |
| Secret management in MCP server | Server inherits user privileges. Identity over secrets. |

## Revision History

| Date | Change |
|------|--------|
| 2026-04-05 | Initial backlog from vision session and README analysis |
| 2026-04-05 | Added REGION-FINDING, ASDF components, MCP tools #6-#15, external tool wrappers |
| 2026-04-09 | Slice 008: removed line-length inspector |
| 2026-04-10 | MCP design session. Slices 009-016 planned. R6 rejected. |
| 2026-04-13 | Slices 011-013 delivered |
| 2026-04-14 | Slice 014 delivered. Migrated to flat-ranked layout. Added #11 (MCP reliability). Renumbered: old #11 (refactorings) → #12, old #12 (diagnostics) → #13. Removed subsumed items #13-#15 (folded into delivered slices). |
