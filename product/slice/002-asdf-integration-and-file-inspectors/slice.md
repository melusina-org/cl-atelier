# Slice 002: ASDF Integration and First File-Level Inspectors

**Status:** Planned
**Type:** Bet
**Goal:** G2 — Linter covers file, line, region, and CST level with ASDF integration
**OKR contribution:** G2 — `(asdf:operate 'atelier:linter-op :any-system)` produces structured findings
**Hypothesis assumption:** An ASDF-native linter operation (`linter-op`) with declarative policy (`linter-configuration` component) and a small set of concrete file-level inspectors will prove that the finding/resolution schema from Slice 001 works end-to-end — from `asdf:operate` through inspector dispatch to structured finding output.
**Hypothesis prediction:** A developer can add `atelier:linter-configuration` and `atelier:project-configuration` components to their `.asd` file, run `(asdf:operate 'atelier:linter-op :my-system)`, and receive structured `file-finding` instances for license header and encoding violations — without writing any inspector code themselves.
**Hypothesis disposition:** ✅ Validated — the schema is stable (Slice 001 ✅), the ASDF integration pattern is well-understood, and file-level inspectors are the simplest category to implement.
**Leading indicator:** Number of concrete inspectors registered and producing findings via `linter-op` — baseline 0, target ≥ 3 by slice completion.
**Kill criterion:** If `(asdf:operate 'atelier:linter-op :my-system)` cannot produce at least one `file-finding` from a registered inspector without custom code in the target system, the ASDF integration design must be revisited.
**Planned start / end:** 2026-04-06 / 2026-04-13
**Actual end:**
**Implementation phases:**
  - Phase 1: product/slice/002-asdf-integration-and-file-inspectors/implementation-1.md — Planned

---

## Customer Journey

Before this slice: The finding/resolution schema and inspector/maintainer registries exist (Slice 001), but there is no way to run inspectors on a project via ASDF. There are no concrete inspectors — only the registry infrastructure. A developer cannot lint their system with `asdf:operate`.

After this slice: A developer adds two ASDF components to their system definition — `atelier:project-configuration` (copyright holder, license, homepage) and `atelier:linter-configuration` (severity overrides, disabled inspectors) — each pointing to a `.sexp` data file. They run `(asdf:operate 'atelier:linter-op :my-system)` and receive structured `file-finding` instances for encoding violations and missing/incorrect SPDX license headers. The inspectors are registered automatically when `org.melusina.atelier` loads.

## Personas served
- End user: Common Lisp developer — can now lint their ASDF system from the REPL with a single `asdf:operate` call and receive structured findings. No tool-specific CLI needed.

## Stories

### S1: project-configuration ASDF component
**As a** system author, **I want to** declare project-level configuration (copyright holder, license SPDX identifier, homepage) as an ASDF component in my `.asd` file, **so that** Atelier tools can read it without a separate configuration file format.
**Acceptance criteria:**
- Given a system definition with `(:atelier:project-configuration "atelier-project")`, when ASDF loads the system, then Atelier reads the corresponding `.sexp` file and makes the configuration accessible.
- Given a project-configuration file containing `(:copyright-holder "A. U. Thor" :license "MIT" :homepage "https://example.com")`, when I query the configuration, then I receive the correct values.
- Given a system without a project-configuration component, when `linter-op` runs, then it proceeds with sensible defaults (no error).

### S2: linter-configuration ASDF component
**As a** system author, **I want to** declare linter policy (severity overrides, disabled inspectors) as an ASDF component, **so that** linter behaviour is project-specific and version-controlled alongside the code.
**Acceptance criteria:**
- Given a system definition with `(:atelier:linter-configuration "atelier-linter")`, when ASDF loads the system, then Atelier reads the `.sexp` policy file with `*read-eval*` bound to NIL.
- Given a linter-configuration that disables inspector `atelier:check-file-encoding`, when `linter-op` runs, then that inspector is skipped.
- Given a linter-configuration that overrides severity for an inspector to `:error`, when `linter-op` runs, then findings from that inspector carry severity `:error`.

### S3: linter-op ASDF operation
**As a** developer, **I want to** run `(asdf:operate 'atelier:linter-op :my-system)` from the REPL, **so that** all source files in my system are inspected and I receive structured findings.
**Acceptance criteria:**
- Given a loaded system with registered inspectors, when I run `(asdf:operate 'atelier:linter-op :my-system)`, then every source file in the system is passed to eligible inspectors.
- Given inspectors that produce findings, when `linter-op` completes, then the findings are collected and returned (or reported via the condition system).
- Given a system with no linter-configuration component, when `linter-op` runs, then it uses default policy (all inspectors enabled, default severities).

### S4: File encoding inspector
**As a** developer, **I want** an inspector that checks whether source files are valid UTF-8, **so that** encoding problems are caught before they cause runtime errors.
**Acceptance criteria:**
- Given a valid UTF-8 file, when the encoding inspector runs, then no finding is produced.
- Given a file with invalid UTF-8 bytes, when the encoding inspector runs, then a `file-finding` is produced with severity `:error`, a descriptive observation, and the file pathname.
- Given the inspector is registered, when I call `(atelier:list-inspectors)`, then `atelier:check-file-encoding` appears.

### S5: SPDX license header inspector
**As a** developer, **I want** an inspector that checks whether source files contain the correct SPDX license identifier in their header, **so that** license compliance is machine-verifiable.
**Acceptance criteria:**
- Given a Common Lisp file with a correct SPDX header matching the project configuration, when the license inspector runs, then no finding is produced.
- Given a file missing an SPDX header, when the license inspector runs, then a `file-finding` is produced with severity `:warning` and an observation naming the expected identifier.
- Given a Shell script with a correct SPDX header in `#` comment style, when the license inspector runs, then no finding is produced.
- Given the project configuration declares license "MIT", when the license inspector checks a file with "GPL-3.0-only" in its header, then a `file-finding` is produced noting the mismatch.

### S6: Inspector runner
**As a** tool author, **I want** a function that runs all eligible inspectors on a file and collects findings, **so that** `linter-op` and other tools can delegate file inspection without managing the registry themselves.
**Acceptance criteria:**
- Given a file and a set of registered inspectors, when I call the runner, then each inspector whose level is `:file` is called on the file.
- Given an inspector that produces a finding, when the runner collects results, then the finding is included in the returned list.
- Given a linter-configuration that disables an inspector, when the runner runs, then the disabled inspector is skipped.

## Quality Attribute Acceptance Criteria
- [x] All fast tests execute in under 2 seconds total.
- [x] All slow tests (filesystem I/O for encoding/license checks) execute in under 10 seconds total.
- [x] The ASDF component types and `linter-op` are fully exported and documented with docstrings.
- [x] Policy files are read with `*read-eval*` bound to NIL — no code execution from configuration.

## Capability Maturity Transitions
- Linter ASDF Integration (G2): Not started → Foundation
- File-level Inspection (G2): Not started → Foundation

## Definition of Ready
- [x] Hypothesis disposition ✅
- [x] Stories sized ≤ 2 days each
- [x] Acceptance criteria written for all stories
- [x] QA acceptance criterion defined
- [x] Leading indicator baseline established: 0 concrete inspectors on 2026-04-06
- [x] Dependencies clear — Slice 001 complete (finding/resolution schema, inspector/maintainer registry)

## Definition of Done
- [x] All stories complete with acceptance criteria passing
- [x] Quality attribute criteria passing
- [x] Leading indicator being measured
- [x] All implementation phases have completion notes
- [x] `product/slice/002-asdf-integration-and-file-inspectors/retrospective.md` created
- [x] `product/maturity-tracker.md` updated

## Assumptions Made (autonomous mode)
- **Assumption 1:** The `.sexp` format for configuration files is a flat plist read with `*read-eval*` bound to NIL. No nested structure or inheritance between systems.
- **Assumption 2:** `linter-op` operates on the source files declared in the system's ASDF components (`:file` components), not on arbitrary directories. It uses ASDF's component tree to discover files.
- **Assumption 3:** The SPDX license header inspector checks for the presence and correctness of the SPDX identifier string in file headers, not the full license text. The expected identifier comes from the project-configuration.
- **Assumption 4:** File-level inspectors run on individual files. The runner calls each eligible inspector on each file. There is no cross-file analysis in this slice.
- **Assumption 5:** "Deployed to production" in DoD is interpreted as "merged to main and loadable via ASDF" for this library project.
