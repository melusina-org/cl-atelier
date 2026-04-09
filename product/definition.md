# System Definition: Atelier
**Repository:** https://github.com/melusina-org/cl-atelier
**Last updated:** 2026-04-05

## Purpose

Atelier is a craftsperson's workshop for Common Lisp developers: a
single, coherent system providing project scaffolding, a multi-language
linter, a code formatter, a live MCP server, and documentation
generation. Its defining characteristic is that the tools which report
problems — the linter, the formatter in check mode, and the
documentation generator — all speak the same language: they produce
FINDING instances of the same class hierarchy, consumed by the same
MAINTAINER registry and the same resolution pipeline. A developer,
a CI script, or an AI coding agent needs to understand one protocol,
not five.

## Scope

### In scope

- **Project templates** — opinionated scaffolding for new Common Lisp
  systems and companion languages (Shell, Terraform/HCL, Elisp),
  including license headers using SPDX identifiers.

- **Multi-language linter** — operates at file, line, region, and
  concrete-syntax-tree levels for Common Lisp, Emacs Lisp, Shell, and
  Terraform/HCL. External tools (ShellCheck, Elisp linters, tflint)
  are wrapped as INSPECTOR subclasses that produce LINE-FINDING or
  REGION-FINDING instances, integrating their output into the shared
  pipeline without requiring consumers to know the tool's native format.

- **Automatic and agent-assisted linter fixes** — diagnostics carry
  metadata sufficient for automatic correction or for a coding agent to
  apply targeted fixes. LLM-driven MAINTAINER instances are a supported
  and planned kind: a maintainer that emits an AGENT-RESOLUTION with a
  structured prompt is a first-class citizen of the registry.

- **Common Lisp code formatter** — opinionated, reproducible; usable
  from REPL and CLI. In check mode, reports findings through the shared
  pipeline.

- **MCP server for Common Lisp** — exposes REPL-like evaluation,
  debugger restarts, Lisp image restart, system reload, and fundamental
  refactoring (rename symbol, rename package, rename system, remove
  unexported symbols, CLOS class and method inspection and manipulation)
  through the Model Context Protocol.

- **ASDF integration:**
  - atelier:linter-op — invoke the linter via
    (asdf:operate 'atelier:linter-op :my-system).
  - atelier:project-configuration — a specific ASDF:COMPONENT subclass
    that holds Atelier project-level configuration (copyright holder,
    license, homepage, template parameters) and is read by templates
    and other tools.
  - atelier:linter-configuration — a specific ASDF:COMPONENT subclass
    that holds linter policy (severity overrides, disabled inspectors,
    path scoping) and is read by linter-op.

- **CI pipeline access** — CLI is a first-class interface, a thin
  wrapper over the same Lisp functions; every tool is scriptable in a
  headless environment.

- **Documentation generation** — produces human-readable and
  machine-readable API documentation from source; reports missing or
  broken documentation as findings.

- **Code quality instrumentation** — code coverage and/or profiling
  support (aspirational — not yet implemented).

- **AI-first diagnostic schema** — a shared, versioned class hierarchy
  (FINDING, RESOLUTION) emitted by all reporting tools, so that
  agentic consumers need no tool-specific adapters.

### Out of scope

- **Editor plugins (not yet planned)** — SLIME, SLY, and Eglot
  integration are aspirational and in scope as a future direction;
  there is no concrete plan today. Atelier exposes an MCP surface in
  the interim.

- **Package management** — dependency resolution belongs to
  Quicklisp / OCICL / ASDF. (Aspirational: producing a Quicklisp
  distribution, publishing OCICL packages, and publishing MELPA
  packages for any Elisp components are future goals, not current
  scope.)

- **Build system** — build orchestration belongs to ASDF / Make.
  Atelier integrates with ASDF as a consumer, not a replacement.
  Exception: atelier:c-source-file (see aspirational below) drives
  a C compiler for the narrow CFFI bridge pattern only.

- **Complex C/C++ build graphs** — out of scope. The CFFI use case
  (one source file, one shared system, reload into image) is
  aspirational; driving a multi-target C project is not.

- **Language support beyond CL, Elisp, Shell, and Terraform/HCL**
  without deliberate scope expansion.

- **Runtime monitoring or production observability.**

### Aspirational (in scope as future direction, not yet planned)

- **Editor plugins** — SLIME, SLY, and Eglot integration. Atelier's
  finding/resolution protocol and MCP surface are designed to make
  this tractable when the time comes.

- **ASDF:COMPONENT for CFFI bridges** — atelier:c-source-file and
  atelier:cpp-source-file component types covering the CFFI bridge
  pattern: one source file, one shared system, compile via
  atelier:compile-c-op, reload into the running Lisp image via
  atelier:load-c-op. Linting via clang-tidy/cppcheck through the
  shared FINDING pipeline.

- **ASDF:COMPONENT for Terraform** — three component types reflecting
  the C4 architecture model:
  - atelier:terraform-file — individual .tf source file.
  - atelier:terraform-module — a directory of .tf files forming a
    reusable unit (C4 Component). Linted via tflint/terraform validate.
  - atelier:terraform-stack — a module with a verifiable interface
    contract (declared inputs/outputs), composable with other stacks
    (C4 Container). Inspectors can verify contract completeness, output
    documentation, and naming conventions.

- **Package publication** — Quicklisp distribution, OCICL repository
  publication, and MELPA package publication for any Elisp components.

## Target User

**Primary user:** A Common Lisp developer — solo maintainer or small
team — who starts new projects frequently, cares about consistent
quality (formatting, licensing, documentation), and works in
mixed-language repositories (Lisp + Elisp + Shell + Terraform).

**Assumed context:** Familiar with Common Lisp, ASDF, and Quicklisp.
Likely uses SLIME or SLY. May use an AI coding assistant and wants
their toolchain to produce output the assistant can act on directly.
Comfortable running tools from both the REPL and a CI pipeline.

**Not designed for:** Developers who want only a standalone linter or
only a formatter — those needs are served by sblint, trivial-formatter,
and lisp-format. Atelier's value is the integrated workshop; users who
need one isolated tool should use the single-purpose alternative.

## Design Principles

1. **One coherent workshop.** The tools that report problems — linter,
   formatter in check mode, documentation generator, coverage — all
   produce FINDING instances of the same class hierarchy and feed the
   same MAINTAINER registry. A CI script, an AI agent, or a future
   editor plugin handles one protocol, not one per tool.
   *Example: a formatter finding about misindentation on line 12 and a
   linter finding about a naming violation on the same line are both
   LINE-FINDING instances; a consumer deduplicating by location needs
   no format translation.*

2. **AI-first outputs, human-readable too.** Every finding carries
   observation (what was measured) and rationale (why it matters),
   giving an agent enough context to act without consulting external
   documentation. The same output must be readable by a human in a
   terminal. The schema is not a separate mode; it is the default.
   *Example: an agent receiving a SYNTAX-FINDING about a missing
   earmuffs convention reads the observation, the rationale, and the
   CST node — enough to apply a rename without clarification.*

3. **REPL-native, CI-capable.** Every capability is accessible from
   the Common Lisp REPL. CLI wrappers are thin layers over the same
   Lisp functions. CI access is first-class, not an afterthought.
   *Example: (atelier:lint :my-system) and atelier lint my-system
   call the same function and produce the same findings.*

4. **Multi-language, Common Lisp-primary.** Common Lisp is the first
   class citizen. Elisp, Shell, and Terraform/HCL are supported because
   they co-occur in Lisp project repositories. External tools
   (ShellCheck, tflint, Elisp byte-compiler) are wrapped as inspectors,
   not reimplemented.
   *Example: ShellCheck findings arrive as LINE-FINDING or
   REGION-FINDING instances indistinguishable in structure from
   native Atelier findings; the consumer does not know which tool
   produced them.*

5. **Correctness before convenience.** A diagnostic that is always
   accurate but requires one extra flag is better than one that is
   fast but sometimes wrong. No false positives by default.
   *Example: a rule that cannot be safely applied to all macro-expanded
   forms is disabled by default rather than emitting noisy warnings.*

6. **SPDX for licenses.** License identification uses SPDX identifiers
   throughout templates and file headers. No custom license name strings.

7. **Idempotency of autofix.** Every MAINTAINER is self-idempotent:
   applying an `(inspector, maintainer)` pair twice to the same input
   yields the same AST as applying it once. Non-idempotent maintainers
   are bugs, not features. This is the current guarantee, enforced in
   the test harness for every fixture. **Pipeline idempotency** — the
   stronger property that running the whole `lint-system :autofix t`
   pipeline over a whole file twice yields the same file as running
   it once — is a long-term goal: it is the contract a pre-commit hook
   or CI step needs in order to avoid gratuitous re-diffs. Reaching it
   requires understanding cross-maintainer interactions and is tracked
   on the roadmap.
   *Example: a fix-earmuffs maintainer that rewrites `foo` to `*foo*`
   must not, on a second pass, rewrite `*foo*` to `**foo**`. A test
   fixture asserts the fixed point after one application.*

## Diagnostic Schema

This section is the authoritative design record for the
finding/resolution protocol. It governs the linter, formatter, MCP
server, documentation generator, and any 3rd-party extension.

### Finding class hierarchy

```
finding
├── file-finding
├── line-finding
├── region-finding        (span of several lines; eases integration of
│                          external tools like ShellCheck or tflint
│                          that report multi-line diagnostics)
└── syntax-finding        (line-finding + Eclector CST node reference)
```

```lisp
(defclass finding ()
  ((inspector    :type symbol)
   (severity     :type (member :error :warning :info :style))
   (observation  :type string)
   ;; Per-instance. Objective statement of what was measured.
   ;; e.g. "Function body is 87 lines long."
   (rationale    :type string)))
   ;; Per-class. Why this measurement warrants attention.
   ;; e.g. "Long function bodies reduce readability and testing."

(defclass file-finding (finding)
  ((file         :type pathname)))

(defclass line-finding (file-finding)
  ((line         :type (integer 1))
   (column       :type (integer 0))
   (end-line     :type (integer 1))
   (end-column   :type (integer 0))
   (source-text  :type string)))

(defclass region-finding (file-finding)
  ((start-line   :type (integer 1))
   (end-line     :type (integer 1))
   (source-text  :type string)))
   ;; Used by external tool wrappers (ShellCheck, tflint, Elisp linters)
   ;; that report diagnostics spanning multiple lines without precise
   ;; column information.

(defclass syntax-finding (line-finding)
  ((cst-node     :type eclector.cst:cst)
   (cst-root     :type eclector.cst:cst)))
```

### Resolution class hierarchy

```
resolution
├── text-resolution        (single location, text edit)
├── syntax-resolution      (single CST node, transform function)
├── agent-resolution       (prompt string for LLM or coding agent)
└── composite-resolution   (ordered list of SYNTAX-RESOLUTION;
                            innermost CST nodes first)
```

```lisp
(defclass resolution ()
  ((maintainer   :type symbol)
   (finding      :type finding)
   (kind         :type (member :automatic :agent))
   (description  :type string)))

(defclass text-resolution (resolution)
  ((location     :type location)
   (replacement  :type string)))

(defclass syntax-resolution (resolution)
  ((transform    :type function)))   ; cst-node -> new-form (NIL = delete)

(defclass agent-resolution (resolution)
  ((prompt       :type string)))
  ;; Used by LLM-driven maintainers. The prompt is a targeted,
  ;; structured instruction derived from the finding's observation,
  ;; rationale, and source context.

(defclass composite-resolution (resolution)
  ((transforms   :type list)))
  ;; Ordered list of SYNTAX-RESOLUTION instances.
  ;; Contract: innermost CST nodes first. The defining maintainer is
  ;; responsible for correct ordering.
```

### Write-back phases

Resolutions are applied to a file in three sequential phases. Each
phase's output is the next phase's input.

1. **File phase** — file-resolution instances applied to raw file
   content (whole-file insertions, deletions, header rewrites).
2. **Line phase** — text-resolution instances applied in reverse line
   order, preserving offsets.
3. **Syntax phase** — Eclector reads the result of phase 2. All
   syntax-resolution and composite-resolution transforms are applied to
   the in-memory CST; composite-resolution transforms are applied in
   declared order (innermost first) before top-level syntax-resolution
   instances. The file is pretty-printed once and written back.

### External tool inspectors

External linters and formatters are wrapped as INSPECTOR subclasses.
Their native output is translated into LINE-FINDING or REGION-FINDING
instances by the wrapper. Planned wrappers:

- atelier/shell:shellcheck-inspector     -> LINE-FINDING / REGION-FINDING
- atelier/elisp:byte-compiler-inspector  -> LINE-FINDING
- atelier/elisp:package-lint-inspector   -> LINE-FINDING
- atelier/terraform:tflint-inspector     -> LINE-FINDING / REGION-FINDING
- atelier/terraform:terraform-validate-inspector -> REGION-FINDING

### LLM-driven maintainers

A MAINTAINER subclass whose prepare-resolution method constructs an
AGENT-RESOLUTION with a structured prompt is a first-class citizen of
the registry. It participates in the superseding mechanism like any
other maintainer. A 3rd-party project can define an LLM-driven
maintainer that supersedes Atelier's automatic maintainer for a given
finding class, providing richer or project-specific remediation.

### Maintainer registry and superseding

Every INSPECTOR and MAINTAINER is identified by an exported symbol
(named instance pattern). Registration is a side-effect of loading the
defining system.

The registry is a hash table keyed on finding class ->
(list of maintainer-name ...).

Superseding is declared explicitly:

```lisp
(define-maintainer my-project/atelier:fix-license-header
  :reacts-to   atelier/linter:license-header-finding
  :supersedes  (atelier/linter:fix-license-header)
  :kind        :automatic
  :documentation "...")
```

Atelier runs only the maximal elements of the partial order defined by
:supersedes declarations. When two loaded maintainers are both maximal,
both run. The project configuration (via atelier:linter-configuration
component) is the tie-breaker.

Maintainer protocol:

```lisp
(defgeneric prepare-resolution (maintainer finding)
  (:documentation
   "Return a RESOLUTION if MAINTAINER can handle FINDING, or NIL.
    NIL means not applicable to this specific instance. The next
    maintainer in the partial order will be tried."))
```

Introspection:

```lisp
atelier:list-inspectors    ; -> list of registered inspector symbols
atelier:list-maintainers   ; -> list of registered maintainer symbols
```

### 3rd-party extensibility contract

The following are stable public API:

- Classes: inspector, maintainer, finding, file-finding, line-finding,
  region-finding, syntax-finding, resolution, text-resolution,
  syntax-resolution, agent-resolution, composite-resolution.
- Macros: define-inspector, define-maintainer.
- Generics: prepare-resolution.
- Functions: atelier:list-inspectors, atelier:list-maintainers.

A 3rd-party system defining inspectors or maintainers using these
interfaces, and loaded into the image, is picked up by Atelier without
additional configuration.

## Goals

| # | Goal | Completion signal | Status |
|---|------|-----------------|:------:|
| G1 | Project templates generate a working, lintable, testable CL system from a single REPL call | atelier:new-lisp-project produces a system that loads, tests, and lints cleanly with zero manual edits | In Progress |
| G2 | Linter covers file, line, region, and CST level for CL, Elisp, Shell, and Terraform/HCL with ASDF integration | (asdf:operate 'atelier:linter-op :any-system) produces structured findings for all four languages | In Progress |
| G3 | Automatic and agent-assisted fix application for linter findings | At least 80% of linter rules carry a machine-applicable fix; atelier:fix applies them without data loss | Not started |
| G4 | Code formatter for Common Lisp, usable from REPL and CLI, idempotent | atelier:format-file is idempotent; output passes the linter's formatting rules | Not started |
| G5 | MCP server exposes REPL evaluation, debugger and image lifecycle, CLOS introspection, and rename refactorings | An AI agent can evaluate forms, select debugger restarts, restart the image, reload systems, and rename symbols via MCP without human intervention | Not started |
| G6 | Documentation generation from source, human- and machine-readable | atelier:generate-docs produces HTML and a machine-readable index; missing docstrings reported as findings | Not started |
| G7 | Shared AI-first diagnostic schema, versioned, emitted by all reporting tools | All reporting tools emit schema-valid findings; schema version included in every output | Not started |
| G8 | Code coverage and profiling instrumentation | atelier:coverage-op and atelier:profile-op produce structured reports as findings | Not started |

## Revision History

| Date | Change |
|------|--------|
| 2026-04-05 | Initial draft bootstrapped from README and maintainer vision statement |
| 2026-04-05 | Applied maintainer corrections: SLIME/SLY and Quicklisp/OCICL moved to aspirational; ASDF linter-op added; MCP repositioned; CLI as thin wrapper with CI first-class |
| 2026-04-05 | Added: Elisp as targeted language; MELPA to aspirational publication targets; REGION-FINDING to finding hierarchy; ShellCheck/Elisp/Terraform external tool inspectors; LLM-driven maintainers; ASDF components for project and linter configuration; editor plugins as aspirational; CFFI bridge and Terraform ASDF components (C4 stack model) as aspirational; new MCP tools; purpose statement names the three finding-producing tools explicitly |
