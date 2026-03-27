# Discovery Log: Atelier
*(Append-only. Most recent entry at the bottom.)*

---

## Vision Session — 2026-04-05
**Source:** Maintainer — solo maintainer of cl-atelier
**Relationship to library:** Author

**Key finding:** The library's identity is an integrated workshop, not
a collection of single-purpose tools. The AI-first diagnostic schema and
the 3rd-party extensibility contract (named-instance pattern, superseding
declaration) are the two design decisions that most distinguish Atelier
from existing alternatives (sblint, trivial-formatter, lisp-format).

**Secondary findings:**
- The INSPECTOR → FINDING → MAINTAINER → RESOLUTION pipeline, with M:N
  relationships and explicit superseding via `:supersedes`, is the agreed
  architecture for the linter subsystem.
- COMPOSITE-RESOLUTION is syntax-only (list of SYNTAX-RESOLUTION,
  innermost CST nodes first) for now; generalisation deferred.
- Write-back applies in three phases: file → line → syntax. Each phase's
  output is the next phase's input.
- FINDING carries two descriptive fields: `observation` (per-instance,
  objective measurement) and `rationale` (per-class, why it matters).
- ASDF `linter-op` is a firm requirement, not aspirational.
- CLI is a thin wrapper; CI access is first-class.
- MCP server is a useful and consistent integration for AI users, not a
  flagship. Atelier must be useful for everyone.
- SLIME/SLY integration and Quicklisp/OCICL distribution are aspirational,
  not current scope.
- Pretty-printer choice for syntax write-back (Eclector + trivial-formatter
  vs. custom emitter) is an open question blocking the CST-level inspector
  and formatter slices.

**Backlog impact:**
- Backlog items #1–#5 established as the active queue.
- Backlog item #32 (pretty-printer decision) flagged as a blocking open
  question for CST-level inspectors and formatter.
- MCP design session still open — must complete before slice #5 starts.

---

## Vision Session (continued) — 2026-04-05
**Source:** Maintainer
**Relationship to library:** Author

**Key finding:** The extensibility contract (named-instance pattern,
self-registration on load) applies uniformly to inspectors, maintainers,
and templates. `atelier/cffi` and `atelier/terraform` are in-tree
subsystems structured exactly as 3rd-party extensions would be — they
demonstrate and validate the contract.

**Secondary findings:**
- `REGION-FINDING` added between `LINE-FINDING` and `SYNTAX-FINDING`.
  `SYNTAX-FINDING` extends `REGION-FINDING`. External tool inspectors
  (Shellcheck, tflint, clang-tidy, Emacs byte-compiler) produce
  `LINE-FINDING` or `REGION-FINDING`.
- Elisp added as a supported companion language.
- LLM-driven maintainers are a first-class `MAINTAINER` kind,
  participating in the same registry and `:supersedes` mechanism.
- `atelier/cffi` scope: single shared library per component, CFFI bridge
  pattern only. Operations: `cffi-compile-op`, `cffi-reload-op`.
- `atelier/terraform` scope: `terraform-file`, `terraform-module`,
  `composable-stack` (C4 container with declared interface),
  `stack-component` (C4 component). Linter enforces interface contract
  and component structure.
- Both subsystems are good candidates to be their own ASDF systems,
  satisfying the extensibility design requirement from the inside.
- ASDF integration expanded: `atelier:project-configuration` and
  `atelier:linter-configuration` are `ASDF:COMPONENT` subtypes, not
  just configuration file formats.
- Editor integration (SLIME/SLY/Emacs) is aspirational and in scope;
  no concrete plan yet.
- Distribution targets (Quicklisp, OCICL, MELPA) are aspirational.
- New MCP tools added to backlog: image restart, system reload, remove
  dangling exports, CLOS find-class, find-method, remove-method.

**Backlog impact:**
- Backlog renumbered; items #8–#15, #18–#22, #26–#27 added.
- Backlog item #32 (pretty-printer decision) remains a blocking open
  question for CST-level inspectors (#17) and formatter (#23).
- MCP design session still open.

---

## Vision Session — 2026-04-05 (continued)
**Source:** Maintainer

**Key finding:** Scope expanded and two new design discussions resolved.

**Secondary findings:**
- Elisp (Emacs Lisp) added as a targeted language alongside CL, Shell,
  and Terraform/HCL. External tool wrappers (Elisp byte compiler, Elisp
  formatter) produce LINE-FINDING or REGION-FINDING via the standard
  inspector registry.
- MELPA package publishing added alongside Quicklisp and OCICL as an
  aspirational distribution target.
- LLM-based MAINTAINER is a first-class maintainer kind in the registry,
  supersedeable like any other. Returns AGENT-RESOLUTION.
- REGION-FINDING added to the finding hierarchy to ease integration of
  external tools (shellcheck, tflint, Elisp byte compiler) that report
  multi-line spans. Handled in the line write-back phase.
- ASDF:COMPONENT for project configuration (atelier:project-configuration)
  and linter configuration (atelier:linter-configuration) added to scope.
  These are read by the linter, templates, and other tools.
- C/C++ CFFI bridge components (atelier:c-source-file,
  atelier:cpp-source-file) added to scope. Motivation: compile one .c/.cpp
  file into a shared library, test it, reload into the Lisp image. Complex
  multi-file C/C++ projects are explicitly out of scope.
- Terraform composable infrastructure stacks: base Terraform support (file
  and module component types, tflint wrapper) is in Atelier proper.
  The composable stack abstraction — enforcing C4-style container
  interfaces and component structure — belongs in a separate
  atelier/infrastructure extension system. Aspirational.
- Editor integration (SLIME/SLY) confirmed in scope as a future goal with
  no concrete plan yet.
- New MCP tools confirmed for backlog: image restart, system reload, remove
  unexported symbols from defpackage, CLOS find-class, find-method,
  remove-method.

**Backlog impact:**
- Backlog items #1–#5 remain the active queue (no change to sequencing).
- Items #8, #9, #13, #14, #15 added (new MCP tools).
- Item #16 added (LLM maintainer).
- Items #19–#21 added (external tool inspectors).
- Item #22 added (CFFI bridge components).
- Item #32 added (atelier/infrastructure, under consideration).
- Pretty-printer decision (#30) confirmed as a blocking open question for
  CST inspectors and the formatter.

---

## Vision Session (continued) — 2026-04-05
**Source:** Maintainer

**Key finding:** The scope is wider than initially captured. Elisp is a
first-class targeted language. The C4 architecture model (Container /
Component) provides the right vocabulary for Terraform ASDF components.
CFFI bridge support is scoped narrowly to the one-file/one-.so/reload
pattern.

**Secondary findings:**
- REGION-FINDING added to finding hierarchy to ease integration of
  external tools (ShellCheck, tflint, Elisp linters) that report
  multi-line diagnostics without precise column information.
- External tool wrappers (ShellCheck, Elisp byte-compiler, package-lint,
  tflint, terraform validate) are INSPECTOR subclasses that translate
  native output to LINE-FINDING or REGION-FINDING — consumers are
  insulated from native tool formats.
- LLM-driven MAINTAINER is a first-class kind: prepare-resolution
  returns an AGENT-RESOLUTION with a structured prompt. Participates
  in the superseding mechanism like any other maintainer.
- ASDF:PROJECT-CONFIGURATION and ASDF:LINTER-CONFIGURATION are distinct
  COMPONENT subclasses — the former consumed by templates and all tools,
  the latter consumed by linter-op and the superseding tie-breaker.
- Editor plugins (SLIME/SLY/Eglot) are explicitly aspirational and in
  scope as a future direction; no concrete plan yet.
- MELPA package publication added to aspirational targets alongside
  Quicklisp and OCICL.
- New MCP tools confirmed: image restart, system reload, remove
  unexported symbols, CLOS FIND-CLASS, FIND-METHOD, REMOVE-METHOD.
- Terraform ASDF components: terraform-file (source file),
  terraform-module (C4 Component, directory of .tf files),
  terraform-stack (C4 Container, module with verifiable interface
  contract). All aspirational.
- CFFI bridge ASDF components: c-source-file and cpp-source-file,
  covering compile-c-op and load-c-op only. Complex C build graphs
  explicitly out of scope.

**Backlog impact:**
- Backlog #1 updated to include REGION-FINDING.
- Backlog #3 expanded to include project-configuration and
  linter-configuration component types.
- Backlog #4 and #27 extended to Elisp.
- Backlog #8–#15 added (new MCP tools).
- Backlog #18–#20 added (external tool wrapper inspectors).
- Backlog #21 added (LLM-driven maintainer).
- Backlog #30–#31 added (CFFI and Terraform ASDF components,
  under consideration).
