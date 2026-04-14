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
- The INSPECTOR -> FINDING -> MAINTAINER -> RESOLUTION pipeline, with M:N
  relationships and explicit superseding via `:supersedes`, is the agreed
  architecture for the linter subsystem.
- COMPOSITE-RESOLUTION is syntax-only (list of SYNTAX-RESOLUTION,
  innermost CST nodes first) for now; generalisation deferred.
- Write-back applies in three phases: file -> line -> syntax. Each phase's
  output is the next phase's input.
- FINDING carries two descriptive fields: `observation` (per-instance,
  objective measurement) and `rationale` (per-class, why it matters).
- ASDF `linter-op` is a firm requirement, not aspirational.
- CLI is a thin wrapper; CI access is first-class.
- SLIME/SLY integration and Quicklisp/OCICL distribution are aspirational,
  not current scope.
- Pretty-printer choice for syntax write-back (Eclector + trivial-formatter
  vs. custom emitter) is an open question blocking the CST-level inspector
  and formatter slices.

**Backlog impact:**
- Backlog items #1-#5 established as the active queue.
- Pretty-printer decision flagged as a blocking open question for CST-level
  inspectors and formatter.

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
- Elisp added as a supported companion language.
- LLM-driven maintainers are a first-class `MAINTAINER` kind.
- ASDF integration expanded: `atelier:project-configuration` and
  `atelier:linter-configuration` are `ASDF:COMPONENT` subtypes.
- Editor integration (SLIME/SLY/Emacs) is aspirational and in scope;
  no concrete plan yet.
- Distribution targets (Quicklisp, OCICL, MELPA) are aspirational.

---

## Vision Session — 2026-04-05 (continued)
**Source:** Maintainer

**Key finding:** Scope expanded and two new design discussions resolved.

**Secondary findings:**
- Elisp (Emacs Lisp) added as a targeted language.
- MELPA package publishing added as an aspirational distribution target.
- LLM-based MAINTAINER is a first-class maintainer kind in the registry.
- REGION-FINDING added to the finding hierarchy for external tools.
- C/C++ CFFI bridge components added to aspirational scope.
- Terraform composable infrastructure stacks: base support in Atelier,
  composable stack abstraction in a separate extension system.
- Editor integration confirmed aspirational with no concrete plan.

---

## Vision Session (continued) — 2026-04-05
**Source:** Maintainer

**Key finding:** The scope is wider than initially captured. Elisp is a
first-class targeted language. The C4 architecture model provides the
right vocabulary for Terraform ASDF components. CFFI bridge support is
scoped narrowly to the one-file/one-.so/reload pattern.

---

## MCP Extraction — 2026-04-14
**Source:** Maintainer

**Key finding:** The MCP server (slices 009-015) has been extracted from
Atelier into its own project (`org.melusina.mcp`). Atelier retains the
linter, inspectors, maintainers, pretty-printer, projectional editor,
templates, and ASDF integration. The MCP server depends on
`org.melusina.atelier/editor` as an adapter but is an independent
project with its own product directory.

**Backlog impact:**
- Goal G5 (MCP server) removed from Atelier.
- MCP-related backlog items removed.
- MCP moved to out-of-scope in both roadmap and backlog.
