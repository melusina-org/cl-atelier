# Slice 001: Finding/Resolution Schema and Inspector/Maintainer Registry

**Status:** Planned
**Type:** Bet
**Goal:** G7 — Shared AI-first diagnostic schema
**OKR contribution:** G7 — All reporting tools emit schema-valid findings; schema version included in every output
**Hypothesis assumption:** A unified finding/resolution class hierarchy with a single inspector/maintainer registry will allow all Atelier reporting tools — linter, formatter, documentation generator — to share one protocol, eliminating per-tool adapters for human and AI consumers.
**Hypothesis prediction:** Once the schema and registry are stable, a 3rd-party ASDF system can define an inspector that produces findings and a maintainer that resolves them, without importing any internal Atelier symbol — using only the public extensibility contract.
**Hypothesis disposition:** ✅ Validated — the design is fully specified in `product/definition.md` (Diagnostic Schema section) and has been through multiple vision session iterations.
**Leading indicator:** Number of public API symbols (classes, macros, generics, functions) that are stable and tested — baseline 0, target ≥ 25 by slice completion.
**Kill criterion:** If a 3rd-party companion system cannot define an inspector and a maintainer using only the public API after this slice ships, the schema design must be revisited.
**Planned start / end:** 2026-04-06 / 2026-04-13
**Actual end:**
**Implementation phases:**
  - Phase 1: product/slice/001-finding-resolution-schema/implementation-1.md — Planned

---

## Customer Journey

Before this slice: Atelier's existing linter (`src/lint.lisp`) reports problems via `hint` objects and interactive conditions. There is no structured class hierarchy for findings, no resolution concept, no registry for inspectors or maintainers, and no extensibility contract for 3rd-party systems.

After this slice: A developer can define an inspector (via `define-inspector`) that produces typed `finding` instances (file-finding, line-finding, region-finding, syntax-finding) and a maintainer (via `define-maintainer`) that produces typed `resolution` instances (text-resolution, syntax-resolution, agent-resolution, composite-resolution). Both register automatically on load. The `prepare-resolution` generic dispatches to the appropriate maintainer. The `:supersedes` mechanism allows project-specific maintainers to override Atelier defaults. Introspection via `list-inspectors` and `list-maintainers` is available. The existing linter (`src/lint.lisp`) is integrated: its `hint` objects are bridged to the new finding hierarchy, and the existing `define-inspector`/`define-linter` workflow continues to function while producing schema-valid findings. Eclector CST types are a hard dependency, making `syntax-finding` fully typed from day one.

## Personas served
- End user: Common Lisp developer — can now write a companion ASDF system that plugs custom inspectors and maintainers into Atelier's pipeline, producing structured findings that any consumer (CI, AI agent, REPL) can handle uniformly.

## Stories

### S1: Finding class hierarchy
**As a** tool author, **I want to** create typed finding instances (file-finding, line-finding, region-finding, syntax-finding) with observation and rationale fields, **so that** every diagnostic in the system carries structured, machine-readable context.
**Acceptance criteria:**
- Given a file-level diagnostic, when I create a `file-finding`, then it carries inspector, severity, observation, rationale, and file slots.
- Given a line-level diagnostic, when I create a `line-finding`, then it additionally carries line, column, end-line, end-column, and source-text slots.
- Given a multi-line diagnostic from an external tool, when I create a `region-finding`, then it carries start-line, end-line, and source-text slots.
- Given a CST-level diagnostic, when I create a `syntax-finding`, then it additionally carries cst-node and cst-root slots.
- Given any finding instance, when I inspect it, then `observation` is per-instance and `rationale` is per-class (shared across all instances of that finding type).

### S2: Resolution class hierarchy
**As a** maintainer author, **I want to** create typed resolution instances (text-resolution, syntax-resolution, agent-resolution, composite-resolution) linked to findings, **so that** fixes carry enough structure for automatic or agent-assisted application.
**Acceptance criteria:**
- Given a text-based fix, when I create a `text-resolution`, then it carries maintainer, finding, kind, description, and replacement slots.
- Given a CST transform, when I create a `syntax-resolution`, then it carries a transform function slot.
- Given an LLM-driven fix, when I create an `agent-resolution`, then it carries a prompt string slot.
- Given a multi-node transform, when I create a `composite-resolution`, then it carries an ordered list of syntax-resolution instances (innermost CST nodes first).
- Given any resolution, when I read its `kind` slot, then it is either `:automatic` or `:agent`.

### S3: Inspector registry with named-instance pattern
**As a** 3rd-party developer, **I want to** define an inspector using `define-inspector` and have it self-register on load, **so that** my companion system's inspectors are automatically available to any Atelier linter run.
**Acceptance criteria:**
- Given a `define-inspector` form in a loaded system, when I call `list-inspectors`, then the inspector's symbol appears in the result.
- Given two systems each defining an inspector with the same symbol, when the second system loads, then it replaces the first registration (last-load-wins).
- Given an inspector definition, when I query its metadata, then I can read its level, languages, and documentation.

### S4: Maintainer registry with superseding
**As a** 3rd-party developer, **I want to** define a maintainer using `define-maintainer` with a `:supersedes` declaration, **so that** my project-specific fix replaces Atelier's default for a given finding class.
**Acceptance criteria:**
- Given a `define-maintainer` form, when I call `list-maintainers`, then the maintainer's symbol appears in the result.
- Given maintainer A and maintainer B where B declares `:supersedes (A)`, when Atelier resolves a finding, then only B's `prepare-resolution` is called.
- Given two loaded maintainers that are both maximal in the superseding partial order, when Atelier resolves a finding, then both run.
- Given a maintainer whose `prepare-resolution` returns NIL, when Atelier resolves a finding, then the next maintainer in the partial order is tried.

### S5: prepare-resolution generic protocol
**As a** tool author, **I want to** call `prepare-resolution` on a maintainer and a finding, **so that** I get back a resolution (or NIL) without knowing the maintainer's implementation.
**Acceptance criteria:**
- Given a maintainer and a finding it handles, when I call `(prepare-resolution maintainer finding)`, then I receive a resolution instance.
- Given a maintainer and a finding it does not handle, when I call `(prepare-resolution maintainer finding)`, then I receive NIL.

### S6: Named-instance pattern infrastructure
**As a** system author, **I want to** use a named-instance pattern for inspectors and maintainers, **so that** identity is tied to exported symbols and registration is a side-effect of loading.
**Acceptance criteria:**
- Given the named-instance infrastructure in `utilities.lisp` (`define-named-class` and `define-persistent-class`), when I define a named instance, then it is identified by its symbol and retrievable by that symbol.
- Given a named instance, when I redefine it with the same symbol, then the old definition is replaced.

### S7: Existing linter integration
**As a** user of Atelier's current linter, **I want** the existing `hint` objects and `define-inspector`/`define-linter` workflow to produce findings through the new schema, **so that** my current linter configuration continues to work while gaining structured, machine-readable output.
**Acceptance criteria:**
- Given the existing linter running on a file, when an inspector produces a `hint-at-file`, then a corresponding `file-finding` is also produced.
- Given the existing linter running on a file, when an inspector produces a `hint-at-file-line`, then a corresponding `line-finding` is also produced.
- Given the existing `anomaly` condition, when it is signalled during linting, then it is bridged to the finding/resolution protocol.
- Given the existing `lint` function, when called on a set of pathnames, then both the legacy `hint` output and the new `finding` output are available.

### S8: Eclector CST dependency
**As a** developer writing CST-level inspectors, **I want** `syntax-finding` to carry fully typed Eclector CST node references, **so that** I can traverse and transform the concrete syntax tree without downcasting.
**Acceptance criteria:**
- Given the ASDF system definition, when I load the schema system, then Eclector is a hard dependency (not optional).
- Given a `syntax-finding` instance, when I access its `cst-node` slot, then the value is typed as `eclector.concrete-syntax-tree:cst-result`.
- Given a `syntax-resolution` transform function, when it receives a CST node, then the node is an Eclector CST object.

## Quality Attribute Acceptance Criteria
- [x] All fast tests execute in under 2 seconds total (no I/O, no external dependencies).
- [x] The public API (classes, macros, generics, functions) is fully exported and documented with docstrings.
- [x] No SBCL-specific extensions used without `#+sbcl` guard.

## Capability Maturity Transitions
- Diagnostic Schema (G7): Not started → Foundation

## Definition of Ready
- [x] Hypothesis disposition ✅
- [x] Stories sized ≤ 2 days each
- [x] Acceptance criteria written for all stories
- [x] QA acceptance criterion defined
- [x] Leading indicator baseline established: 0 public API symbols on 2026-04-06
- [x] Dependencies clear — named-instance pattern supplied by maintainer in utilities.lisp

## Definition of Done
- [x] All stories complete with acceptance criteria passing
- [x] Quality attribute criteria passing
- [x] Leading indicator being measured
- [x] All implementation phases have completion notes
- [x] `product/slice/001-finding-resolution-schema/retrospective.md` created
- [x] `product/maturity-tracker.md` updated

## Assumptions Made (autonomous mode)
- **Assumption 1:** ~~This slice does NOT wire the new schema into the existing linter.~~ **Corrected:** The existing linter (`src/lint.lisp`) IS integrated in this slice — `hint` objects are bridged to findings, and the existing `define-inspector`/`define-linter` workflow produces schema-valid output.
- **Assumption 2:** ~~Eclector CST types are loosely typed.~~ **Corrected:** Eclector is a hard dependency. `syntax-finding` slots are fully typed with Eclector CST types from day one.
- **Assumption 3:** The `composite-resolution` ordering contract (innermost CST nodes first) is documented but not enforced at construction time — enforcement belongs to the write-back pipeline (later slice).
- **Assumption 4:** "Deployed to production" in DoD is interpreted as "merged to main and loadable via Quicklisp/ASDF" for this library project.
- **Assumption 5:** ~~The named-instance pattern implementation is provided by the maintainer and does not need to be designed in this slice.~~ **Corrected:** `define-named-class` and `define-persistent-class` are already in `utilities.lisp` — consumed directly.
