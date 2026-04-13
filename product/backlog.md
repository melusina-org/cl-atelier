# Backlog: Atelier
**Last updated:** 2026-04-10

## Active (next 1–3 slices)

| # | Item | Type | Source | Effort | Notes |
|---|------|------|--------|:------:|-------|
| 1 | Define and stabilise the finding/resolution schema: FINDING, FILE-FINDING, LINE-FINDING, REGION-FINDING, SYNTAX-FINDING, RESOLUTION, TEXT-RESOLUTION, SYNTAX-RESOLUTION, AGENT-RESOLUTION, COMPOSITE-RESOLUTION | New capability | Vision session 2026-04-05 | M | Prerequisite for all linter and maintainer work. Named-instance pattern implementation to be supplied by maintainer. |
| 2 | Define and stabilise the inspector/maintainer registry: define-inspector, define-maintainer, :supersedes, list-inspectors, list-maintainers, prepare-resolution generic | New capability | Vision session 2026-04-05 | M | Depends on #1. This is the 3rd-party extensibility contract — must be stable before any concrete inspector is written. |
| 3 | ASDF integration: atelier:project-configuration and atelier:linter-configuration component types; atelier:linter-op operation | New capability | Maintainer requirement 2026-04-05 | M | Depends on #1 and #2. project-configuration read by templates; linter-configuration read by linter-op. |
| 4 | First concrete inspector set: file-header / SPDX license identifier (CL, Shell, HCL, Elisp) | Completion | README + vision session | S | Depends on #2. File-level only — validates registry and write-back at simplest level. |
| 5 | MCP server skeleton: protocol, connection lifecycle, tool registration, transcript subsystem, four trivial tools, seven resources | New capability | Vision session 2026-04-05; design session 2026-04-10 | M | **Slice 009 — Planned 2026-04-10.** Standalone binary `atelier-mcp` over stdio. Sexp-canonical transcript with JSON and Markdown views. Abstract `image-connection` class for slice 010. No child image, no eval, no SWANK in this slice. |

## Queued (clearly belongs, not yet scheduled)

| # | Item | Type | Source | Effort | Notes |
|---|------|------|--------|:------:|-------|
| 6 | MCP slice 010 — child image lifecycle (start/stop/terminate), eval, structured stdout/stderr, package and symbol introspection (`closer-mop` dependency) | New capability | Vision session 2026-04-05; design session 2026-04-10 | L | Depends on #5. Spawns SBCL child via `socketpair(2)` to in-image SWANK. Ships `lisp://packages/...` resources. SBCL_HOME set inside child if absent. **Lint+autofix-before-eval is baked into the eval tool, not a separate tool.** |
| 7 | MCP slice 011 — debugger access, restart selection, condition introspection, sldb-equivalent | New capability | Vision session 2026-04-05 | M | Depends on #6. Requires live debugger state accessible from MCP connection over the slice-010 transport. |
| 8 | MCP slice 012 — ASDF operations (`asdf-operate`), Quicklisp (`quickload`, `where-is-system`, `system-apropos`), Confidence test runner (uses Confidence's symbol-property convention for testcase discovery) | New capability | Maintainer requirement 2026-04-05; design session 2026-04-10 | M | Depends on #6. Confidence integration leverages the existing `define-testcase` symbol property — no upstream extension needed. Includes `confidence.run-impacted` (depends on #14 xref). |
| 9 | MCP slice 013 — documentation: describe, documentation, apropos, hyperspec dictionary **and X3J13 issue discussions**, source-location, compile-with-notes, disassemble, macroexpand | New capability | Maintainer requirement 2026-04-05; design session 2026-04-10 | M | Depends on #6. CLHS available locally via MacPorts; no network fetch. X3J13 issues exposed as `lisp://hyperspec/issues/<id>` resources. |
| 10 | MCP slice 014 — xref tools (`who-calls`, `who-references`, `who-binds`, `who-specializes`), CLOS inspector, trace/untrace, **`who-tests` / `run-impacted`** | New capability | Vision session 2026-04-05; design session 2026-04-10 | M | Depends on #9. `who-tests` filters xref callers to those carrying the Confidence testcase property and ranks them. Differentiator over generic Lisp MCP servers. |
| 11 | MCP slice 015 — refactorings: rename-symbol, rename-package, rename-system, unintern-symbol, unexport-symbol, find-and-remove-method, lint-passthrough (examine forms before sending to image) | New capability | Vision session 2026-04-05; design session 2026-04-10 | L | Depends on #10. Requires syntax-finding + composite-resolution pipeline. The lint-passthrough tool is the bridge between the MCP server and the existing Atelier linter. |
| 12 | MCP slice 016 — domain diagnostics: CFFI foreign-library state and path debugging, bordeaux-threads thread listing and backtraces, SBCL profiling | New capability | Vision session 2026-04-05; design session 2026-04-10 | M | Depends on #9. SBCL profiling is `#+sbcl`. |
| 13 | MCP rename system tool | New capability | Vision session 2026-04-05 | M | Subsumed by #11. Kept for cross-reference. |
| 14 | MCP remove unexported symbols | New capability | Maintainer requirement 2026-04-05 | M | Subsumed by #11. |
| 15 | MCP CLOS introspection | New capability | Maintainer requirement 2026-04-05 | S | Subsumed by #10 (xref + inspector). |
| 16 | Linter: line-level inspectors for CL (trailing whitespace, mixed indentation) | Completion | Vision session 2026-04-05 | S | Validates line-finding + text-resolution + write-back pipeline. Line-length check removed in slice 008 — see `product/reference/line-length-research.md`. |
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
| 32 | Web transcript viewer (Hunchentoot + cl-who + Bootstrap) for MCP session transcripts | Design session 2026-04-10 | What concrete story needs HTML rendering beyond what an MCP client renders natively from Markdown? If a story arrives, build as a separate `org.melusina.atelier/mcp/transcript-viewer` system; do not pull Hunchentoot into core `/mcp`. |
| 33 | SQLite project index of Lisp systems and findings | Design session 2026-04-10 | What story requires persistence that ASDF source-registry and Quicklisp local-projects do not already provide? Possible: cross-session memory of findings, cross-image symbol tracking. Revisit after slice 014 (xref) when persistence patterns become clearer. |

## Rejected (considered and decided against)

| # | Item | Reason |
|---|------|--------|
| R1 | Editor plugins (SLIME/SLY/Eglot) now | Aspirational future direction. No concrete plan. Atelier exposes MCP in the interim. |
| R2 | Package management / dependency resolution | Belongs to Quicklisp/OCICL/ASDF. Publication (Quicklisp, OCICL, MELPA) is aspirational future work. |
| R3 | Build system replacement | Atelier integrates with ASDF as a consumer, not a replacement. |
| R4 | Language support beyond CL, Elisp, Shell, HCL | Requires deliberate scope expansion decision. |
| R5 | Complex C/C++ build graphs | Out of scope. CFFI bridge pattern (one file, one .so) is aspirational; multi-target C projects are not. |
| R6 | Secret management / SSH key handling / credential storage in MCP server | The MCP server inherits the privileges of the user who launched it. Adding a credential surface buys nothing and adds an attack surface. Per tech stack: identity over secrets. If a tool needs a credential, it reads it from the environment at the moment of use. Decided 2026-04-10 design session. |

## Revision History

| Date | Change |
|------|--------|
| 2026-04-05 | Initial backlog from vision session and README analysis |
| 2026-04-05 | Added: REGION-FINDING to #1; ASDF project-configuration and linter-configuration to #3; Elisp to #4 and #27; MCP tools #8-#15 (image restart, system reload, unexported symbol removal, CLOS introspection); ShellCheck/Elisp/Terraform wrapper inspectors #18-#20; LLM-driven maintainer #21; CFFI and Terraform ASDF components #30-#31 |
| 2026-04-09 | Slice 008: removed line-length check and `fix-line-too-long` maintainer. Item #16 reworded to drop "line length". Rationale: Atelier adopts the gofmt position — the pretty-printer is the single authority on canonical Lisp text, and a separate line-length reporter adds noise without correctness. See `product/reference/line-length-research.md`. |
| 2026-04-10 | MCP design session. Slice 009 (MCP skeleton) planned. Backlog items #6–#12 rewritten to map onto the planned slice 010–016 track: #6=child image+eval+package introspection, #7=debugger, #8=ASDF/QL/Confidence, #9=documentation+CLHS issues, #10=xref+`who-tests`, #11=refactorings+lint passthrough, #12=CFFI/threads/profiling. Items #13–#15 marked as subsumed but kept for cross-reference. Added rejected item R6 (secret management). Added under-consideration #32 (web transcript viewer) and #33 (SQLite project index). Rationale captured in `product/slice/009-mcp-skeleton/slice.md`. |
| 2026-04-13 | Slice 011 delivered: backlog #7 (debugger access). Core debugger tools shipped; eval-in-frame and timeout deferred (SWANK protocol issues). Slice 012 delivered: backlog #8 (ASDF/QL/Confidence). All 5 tools shipped. |
