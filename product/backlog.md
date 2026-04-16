# Backlog: Atelier
**Last updated:** 2026-04-16

The roadmap is a projection of the backlog's top items into time horizons. It does not introduce new items — every item in Now or Next must appear in the backlog.

| Rank | Item | Type | Source | Effort | Ready | Notes |
|-----:|------|------|--------|:------:|:-----:|-------|
| 1 | Pretty-printer / SLIME-SLY compatibility | Improvement | Discovery 2026-04-16 | S | ✓ | Align pprint dispatch with Emacs indentation; document/provide `.dir-locals.el` |
| 2 | Trivia pattern matching for inspectors/maintainers | Improvement | Discovery 2026-04-16 | S+M | — | Spike first (S), then migration (M). Needs custom Trivia expander for Eclector CST. |
| 3 | Code formatter for CL (idempotent, REPL+CLI) | New capability | Vision 2026-04-05 | L | — | Depends on #1 |
| 4 | CLI thin wrapper (atelier lint/fix/format) | New capability | Maintainer 2026-04-05 | S | — | Depends on #3 |
| 5 | Projectional editor capabilities | New capability | Discovery 2026-04-16 | M–L | — | Design spike needed. Form-level CRUD, file ops, future shell/terraform. |
| 6 | LLM-driven maintainer (AGENT-RESOLUTION) | New capability | Vision 2026-04-05 | M | ✓ | |
| 7 | ShellCheck wrapper inspector | New capability | Vision 2026-04-05 | S | ✓ | |
| 8 | Elisp byte-compiler + package-lint wrappers | New capability | Vision 2026-04-05 | S | ✓ | |
| 9 | tflint / terraform validate wrappers | New capability | Vision 2026-04-05 | S | ✓ | |
| 10 | Project templates: extend for Elisp, HCL | Improvement | README 2026-04-05 | S | ✓ | Shell templates already exist |
| 11 | Configuration tie-breaking for superseding conflicts | New capability | Vision 2026-04-05 | M | — | |
| 12 | Documentation generation (HTML + index) | New capability | Vision 2026-04-05 | L | — | |

## Delivered

| Item | Delivered in |
|------|-------------|
| Finding/resolution schema (FINDING, FILE-FINDING, LINE-FINDING, REGION-FINDING, SYNTAX-FINDING, resolutions) | Slice 001 |
| Inspector/maintainer registry (define-inspector, define-maintainer, supersedes, prepare-resolution) | Slice 002 |
| ASDF integration (project-configuration, linter-configuration, linter-op) | Slice 002 |
| First inspectors: file-header / SPDX (CL, Shell, HCL) | Slice 002 |
| Line-level CL inspectors (trailing whitespace, mixed indentation) | Slice 003 |
| CST-level CL inspectors (earmuffs, constant-naming, bare-lambda, loop-keywords, labels-for-flet, single-branch-if, single-form-progn, when-not) | Slices 004–007 |
| Pre-commit hook installer | Slice 009 |
| Project structure inspectors (system naming, file naming, test mirror) | Slice 009 |

## Under consideration

| Item | Source | Open question |
|------|--------|---------------|
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
| Function body length inspector | No user demand; dropped during 2026-04-16 backlog cleanup. |

## Revision History

| Date | Change |
|------|--------|
| 2026-04-05 | Initial backlog from vision session and README analysis |
| 2026-04-05 | Added REGION-FINDING, ASDF components, external tool wrappers |
| 2026-04-09 | Slice 008: removed line-length inspector |
| 2026-04-14 | MCP server extracted to org.melusina.mcp. MCP backlog items removed. Renumbered. |
| 2026-04-14 | Added: pre-commit hook installer, project structure inspectors. |
| 2026-04-16 | Major cleanup: moved 8 delivered items to Delivered section. Added pretty-printer/SLIME-SLY (#1), Trivia pattern matching (#2), projectional editor (#5). Moved code formatter (#3) and CLI wrapper (#4) up. Dropped function body length inspector. Removed Trivia spike from Under Consideration (now backlog #2). |
