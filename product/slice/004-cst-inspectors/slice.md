# Slice 004: CST-Level Inspectors

**Status:** Planned
**Type:** Bet
**Goal:** G2 — Linter covers file, line, region, and CST level with ASDF integration
**OKR contribution:** G2 — CST-level inspectors produce structured `syntax-finding` instances through the linter pipeline
**Hypothesis assumption:** CST-level inspectors using Eclector's concrete syntax tree can detect naming convention violations and style issues that are invisible to line-level analysis, proving the syntax stage of the inspection pipeline.
**Hypothesis prediction:** A developer running `linter-op` on a system receives `syntax-finding` instances for earmuffs violations, constant naming violations, bare lambdas in mapcar, and bare loop keywords — each carrying the CST node reference for precise source location.
**Hypothesis disposition:** ✅ Validated — the CST is stable (Eclector dependency from Slice 001), the inspector architecture handles multiple levels (Slices 002–003), and the four checks are unambiguous.
**Leading indicator:** Number of CST-level inspectors registered and producing findings — baseline 0, target ≥ 4 by slice completion.
**Kill criterion:** If CST-level inspectors cannot produce `syntax-finding` instances through the existing runner without architectural changes to the pipeline, the syntax stage design must be revisited.
**Planned start / end:** 2026-04-07 / 2026-04-12
**Actual end:**
**Implementation phases:**
  - Phase 1: product/slice/004-cst-inspectors/implementation-1.md — Planned

---

## Customer Journey

Before this slice: The linter pipeline handles file-level and line-level inspectors but has no structural analysis. Naming convention violations (missing earmuffs on special variables, wrong constant naming) and style issues (bare lambdas, bare loop keywords) cannot be detected because they require understanding the structure of Lisp forms.

After this slice: A developer runs `linter-op` and receives `syntax-finding` instances that reference the exact CST node where the violation occurs. Each finding carries the CST node and root, enabling future tools to apply targeted transforms without reparsing.

## Personas served
- End user: Common Lisp developer — receives structural diagnostics enforcing the naming and style conventions described in the project's coding standards.

## Stories

### S1: Earmuffs inspector
**As a** developer, **I want** an inspector that detects `defvar` and `defparameter` forms where the variable name lacks the `*name*` earmuffs convention, **so that** special variables are visually distinguished.
**Acceptance criteria:**
- Given `(defvar *correct-name* 42)`, when the inspector runs, then no finding is produced.
- Given `(defvar wrong-name 42)`, when the inspector runs, then a `syntax-finding` is produced naming the variable and the expected convention.
- Given `(defparameter wrong 1)`, when the inspector runs, then a finding is produced.
- Given the inspector is registered, when I call `(atelier:list-inspectors)`, then `atelier:check-earmuffs` appears.

### S2: Constant naming inspector
**As a** developer, **I want** an inspector that detects `defconstant` forms where the constant name lacks the `+name+` convention, **so that** constants are visually distinguished.
**Acceptance criteria:**
- Given `(defconstant +correct-name+ 42)`, when the inspector runs, then no finding is produced.
- Given `(defconstant wrong-name 42)`, when the inspector runs, then a `syntax-finding` is produced.
- Given the inspector is registered, when I call `(atelier:list-inspectors)`, then `atelier:check-constant-naming` appears.

### S3: Bare lambda in mapcar inspector
**As a** developer, **I want** an inspector that detects `(mapcar (lambda ...) ...)` and similar higher-order calls with inline lambdas, **so that** I use named local functions instead.
**Acceptance criteria:**
- Given `(mapcar #'named-function list)`, when the inspector runs, then no finding is produced.
- Given `(mapcar (lambda (x) (1+ x)) list)`, when the inspector runs, then a `syntax-finding` is produced.
- Given `(remove-if (lambda (x) (null x)) list)`, when the inspector runs, then a finding is produced.
- Given the inspector is registered, when I call `(atelier:list-inspectors)`, then `atelier:check-bare-lambda` appears.

### S4: Bare loop keywords inspector
**As a** developer, **I want** an inspector that detects `loop` forms using bare symbols instead of keyword symbols for clause keywords, **so that** my code uses the `:for`/`:in`/`:collect` convention consistently.
**Acceptance criteria:**
- Given `(loop :for x :in list :collect x)`, when the inspector runs, then no finding is produced.
- Given `(loop for x in list collect x)`, when the inspector runs, then a `syntax-finding` is produced.
- Given the inspector is registered, when I call `(atelier:list-inspectors)`, then `atelier:check-loop-keywords` appears.

### S5: Syntax inspector infrastructure
**As a** tool author, **I want** the runner to support syntax-level inspectors through the same pipeline, **so that** CST inspectors run alongside file and line inspectors.
**Acceptance criteria:**
- Given a registered syntax-inspector, when `run-file-inspectors` runs, then the syntax inspector is called with the parsed CST.
- Given the `inspect-file` method on `syntax-inspector`, when called with a pathname, then it parses the file with Eclector and delegates to `inspect-syntax`.
- Given an `inspect-syntax` method on a concrete inspector, when called with a CST node, then it can traverse the tree and produce `syntax-finding` instances.

## Quality Attribute Acceptance Criteria
- [ ] All fast tests execute in under 2 seconds total.
- [ ] All slow tests execute in under 10 seconds total.
- [ ] Each inspector is exported and documented with docstrings.
- [ ] Each inspector produces `syntax-finding` instances with valid CST node and root references.

## Capability Maturity Transitions
- CST-level Inspection (G2): Not started → Foundation

## Definition of Ready
- [x] Hypothesis disposition ✅
- [x] Stories sized ≤ 2 days each
- [x] Acceptance criteria written for all stories
- [x] QA acceptance criterion defined
- [x] Leading indicator baseline established: 0 CST-level inspectors on 2026-04-07
- [x] Dependencies clear — Slices 001–003 complete, Eclector CST available

## Definition of Done
- [ ] All stories complete with acceptance criteria passing
- [ ] Quality attribute criteria passing
- [ ] Leading indicator being measured
- [ ] All implementation phases have completion notes
- [ ] `product/slice/004-cst-inspectors/retrospective.md` created
- [ ] `product/maturity-tracker.md` updated
