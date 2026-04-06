# Implementation Phase 1: Finding/Resolution Schema and Inspector/Maintainer Registry

**Slice:** product/slice/001-finding-resolution-schema/slice.md
**Phase:** 1 of 1
**Scope:** Implement the full finding/resolution class hierarchy with Eclector CST types, the inspector and maintainer registries using the named-instance pattern with superseding, the `prepare-resolution` generic protocol, and bridge the existing linter's `hint`/`anomaly` workflow to produce schema-valid findings. Move the existing linter to `org.melusina.atelier/legacy` (package `atelier/legacy`).
**Prerequisites:** None (first phase).

---

## Prior Phases

None.

---

## Risk Register

| # | Risk | Category | Mitigation |
|---|------|----------|-----------|
| R1 | `define-named-class` generates symbol names (`*inspectors*`, `find-inspector`, `define-inspector`) that could clash with other `atelier` exports | Scope boundary | Verify generated names against existing package exports before writing code; the legacy symbols are moving to `atelier/legacy` |
| R2 | Moving legacy linter to `atelier/legacy` package breaks the existing testsuite | Scope boundary | Update testsuite references in the same commit; run existing tests after move to verify |
| R3 | Eclector CST types may differ across Quicklisp dist versions | Library API | Pin to `concrete-syntax-tree:cst` base class (stable since 2019); test with installed version |
| R4 | Superseding partial order may contain cycles | State/lifecycle | Detect cycles at `define-maintainer` time and signal a `simple-error` |
| R5 | Legacy `define-inspector` (keyword code + function name) vs new `define-inspector` (named-instance, symbol + keyword args) — name reused with different signature | Scope boundary | Clean separation: legacy in `atelier/legacy`, new in `atelier`; no import between them |
| R6 | `development.lisp` calls `atelier:lint` and `atelier:*parameter-bindings*` — must update after move | Scope boundary | Update `atelier/development` to call `atelier/legacy:lint`; `*parameter-bindings*` stays in `atelier` (templates use it) |
| R7 | `define-named-class` slot initarg `:documentation` clashes with `cl:documentation` symbol | Library API | Use `:description` as initarg and slot name instead |

---

## OSS Components

| Component | Version | License | Rationale | Integration |
|-----------|---------|---------|-----------|-------------|
| eclector-concrete-syntax-tree | Quicklisp 2026-01-01 | BSD | Provides CST node types (`concrete-syntax-tree:cst`, `atom-cst`, `cons-cst`) for `syntax-finding` and `syntax-resolution` | ASDF dependency of `org.melusina.atelier`; types used in slot `:type` declarations |
| concrete-syntax-tree | (transitive) | BSD | Transitive dependency of eclector-concrete-syntax-tree | No direct integration needed |

---

## Phase Scope

**Must deliver (all 8 stories):**
- S1: Finding class hierarchy
- S2: Resolution class hierarchy
- S3: Inspector registry with named-instance pattern
- S4: Maintainer registry with superseding
- S5: `prepare-resolution` generic protocol
- S6: Named-instance pattern infrastructure
- S7: Existing linter integration (bridge)
- S8: Eclector CST dependency

**Deferred to later slices:** None.

---

## File Organisation

```
src/
├── package.lisp              [modify] — remove legacy exports, add schema/registry exports
├── utilities.lisp            [modify] — fix define-named-class to use defclass
├── configuration.lisp        [unchanged]
├── license.lisp              [unchanged]
├── parameter.lisp            [unchanged]
├── template.lisp             [unchanged]
├── finding.lisp              [new] — finding, file-finding, line-finding, region-finding, syntax-finding
├── resolution.lisp           [new] — resolution, text-resolution, syntax-resolution, agent-resolution, composite-resolution
├── inspector.lisp            [new] — inspector named-class + list-inspectors
├── maintainer.lisp           [new] — maintainer named-class + superseding + prepare-resolution + resolve-finding
├── main.lisp                 [unchanged]
└── legacy/
    ├── package.lisp          [new] — atelier/legacy package definition
    ├── lint.lisp             [moved from src/lint.lisp, in-package changed]
    ├── bridge.lisp           [new] — hint-to-finding, lint-with-findings
    └── inspector/
        ├── codestyle-0001.lisp  [moved, in-package changed]
        ├── codestyle-0002.lisp  [moved, in-package changed]
        ├── codestyle-0003.lisp  [moved, in-package changed]
        ├── codestyle-0004.lisp  [moved, in-package changed]
        ├── codestyle-0005.lisp  [moved, in-package changed]
        └── codestyle-0006.lisp  [moved, in-package changed]

testsuite/
├── package.lisp              [modify] — add atelier/legacy imports
├── finding.lisp              [new] — S1 tests
├── resolution.lisp           [new] — S2 tests
├── inspector.lisp            [new] — S3, S6 tests
├── maintainer.lisp           [new] — S4, S5 tests
├── bridge.lisp               [new] — S7 tests
├── lint.lisp                 [modify] — update to atelier/legacy references
├── entrypoint.lisp           [modify] — add new test groups
└── (other files unchanged)

libexec/lisp/
└── development.lisp          [modify] — update atelier:lint → atelier/legacy:lint
```

**Decision rule:** New files are added when introducing a new concept (finding, resolution, inspector class, maintainer class, bridge). Existing files are modified only when their exports or package references change.

---

## Build System Changes

### `org.melusina.atelier` (modified)

New dependency: `#:eclector-concrete-syntax-tree`

Load order:
```
package → utilities → configuration → license → parameter → template
→ finding → resolution → inspector → maintainer → main
```

Files `lint.lisp` and module `inspector/` are removed from this system.

### `org.melusina.atelier/legacy` (new)

```lisp
(asdf:defsystem #:org.melusina.atelier/legacy
  :description "Legacy linter for Atelier"
  :author "Michaël Le Barbier"
  :depends-on (#:org.melusina.atelier)
  :components
  ((:module "src/legacy"
    :components ((:file "package")
                 (:file "lint")
                 (:module "inspector"
                  :components
                  ((:file "codestyle-0001")
                   (:file "codestyle-0002")
                   (:file "codestyle-0003")
                   (:file "codestyle-0004")
                   (:file "codestyle-0005")
                   (:file "codestyle-0006")))
                 (:file "bridge")))))
```

### `org.melusina.atelier/testsuite` (modified)

Add dependency: `#:org.melusina.atelier/legacy`

Add components: `finding`, `resolution`, `inspector`, `maintainer`, `bridge` (after existing files, before `entrypoint`).

### `org.melusina.atelier/development` (modified)

Add dependency: `#:org.melusina.atelier/legacy`

---

## Package / Module Architecture

### Package `atelier` — new exports (grouped by concept)

**Finding hierarchy:**
`finding`, `file-finding`, `line-finding`, `region-finding`, `syntax-finding`,
`finding-inspector`, `finding-severity`, `finding-observation`, `finding-rationale`,
`finding-file`,
`finding-line`, `finding-column`, `finding-end-line`, `finding-end-column`, `finding-source-text`,
`finding-start-line`,
`finding-cst-node`, `finding-cst-root`

**Resolution hierarchy:**
`resolution`, `text-resolution`, `syntax-resolution`, `agent-resolution`, `composite-resolution`,
`resolution-maintainer`, `resolution-finding`, `resolution-kind`, `resolution-description`,
`resolution-replacement`,
`resolution-transform`,
`resolution-prompt`,
`resolution-transforms`

**Inspector registry:**
`inspector`, `*inspectors*`, `define-inspector`, `find-inspector`, `symbol-inspector`,
`inspector-name`, `inspector-level`, `inspector-languages`, `inspector-description`,
`list-inspectors`

**Maintainer registry:**
`maintainer`, `*maintainers*`, `define-maintainer`, `find-maintainer`, `symbol-maintainer`,
`maintainer-name`, `maintainer-reacts-to`, `maintainer-supersedes`, `maintainer-kind`,
`maintainer-prepare-fn`, `maintainer-description`,
`list-maintainers`

**Protocol:**
`prepare-resolution`, `resolve-finding`

### Package `atelier` — removed exports (moved to legacy)

`*linter-interactive-p*`, `hint-at-file`, `hint-at-file-line`, `lint-file`, `lint`,
`linter`, `extensive-linter`, `canonical-source-linter`, `inline-comment-linter`,
`block-comment-linter`, `plain-line-comment-linter`, `plain-block-comment-linter`,
`file-inspectors`, `content-inspectors`, `line-inspectors`, `define-linter`,
`decorate-line`, `decorate-block`

Note: `define-inspector` and `list-inspectors` are **reused** — same names, new definitions.

### Package `atelier/legacy` (new)

Exports the legacy linter API:
`*linter-interactive-p*`, `hint`, `hint-at-file`, `hint-at-file-line`, `anomaly`,
`lint`, `lint-file`, `lint-contents`, `lint-lines`,
`linter`, `extensive-linter`, `canonical-source-linter`,
`plain-line-comment-linter`, `plain-block-comment-linter`,
`define-plain-linter`, `find-plain-linter`,
`define-inspector`, `find-inspector`, `list-inspectors`,
`hint-to-finding`, `lint-with-findings`

Uses: `#:common-lisp`
Imports from `#:atelier`: `file-finding`, `line-finding`, `finding-inspector`, `finding-severity`, `finding-observation`, `finding-rationale`, `finding-file`, `finding-line`, `finding-column`, `finding-source-text`, `*parameter-bindings*`, `parameter-replacement-text`, `find-license`, `license-header`, `list-licenses`, `initialize`, `template-repository-empty-p`
Imports from `#:alexandria`: `read-file-into-string`, `make-keyword`, `lastcar`
Imports from `#:cl-ppcre` (as needed by lint.lisp internals)

---

## Type / Class Hierarchy

### Finding hierarchy

```
finding                            [abstract — not instantiated directly]
├── file-finding                   [concrete]
│   ├── line-finding               [concrete]
│   │   └── syntax-finding         [concrete]
│   └── region-finding             [concrete]
```

### Resolution hierarchy

```
resolution                         [abstract — not instantiated directly]
├── text-resolution                [concrete]
├── syntax-resolution              [concrete]
├── agent-resolution               [concrete]
└── composite-resolution           [concrete]
```

### Named-instance types

```
inspector                          [concrete, named-instance via define-named-class]
maintainer                         [concrete, named-instance via define-named-class]
```

---

## Protocol Definitions

```lisp
(defgeneric prepare-resolution (maintainer finding)
  (:documentation
   "Return a RESOLUTION if MAINTAINER can handle FINDING, or NIL.
    NIL means this maintainer is not applicable to this specific finding
    instance. The default method calls the maintainer's PREPARE-FN slot.")
  (:method ((m maintainer) (f finding))
    (let ((fn (maintainer-prepare-fn m)))
      (when fn (funcall fn f)))))
```

```lisp
(defun resolve-finding (finding)
  "Collect resolutions from all applicable maximal maintainers for FINDING.
   Walks the maintainer registry, filters to those whose REACTS-TO matches
   the finding's class, computes the maximal elements of the superseding
   partial order, calls PREPARE-RESOLUTION on each, and returns the list
   of non-NIL resolutions.")
```

```lisp
(defun list-inspectors ()
  "Return a list of all registered inspector symbols.")
```

```lisp
(defun list-maintainers ()
  "Return a list of all registered maintainer symbols.")
```

```lisp
(defun hint-to-finding (hint)
  "Convert a legacy HINT instance to the corresponding FINDING instance.
   HINT-AT-FILE becomes FILE-FINDING. HINT-AT-FILE-LINE becomes LINE-FINDING.
   The hint's keyword code is used as the inspector identity.
   Severity defaults to :WARNING.")
```

---

## Error / Condition Types

| Name | Superclass | Slots | When signalled |
|------|-----------|-------|----------------|
| (none new) | — | — | Superseding cycles detected at `define-maintainer` time signal `simple-error` via `error`. Legacy `anomaly` condition remains in `atelier/legacy`. |

---

## Test Plan

| Story | Test name | Category | Skip condition |
|-------|-----------|:--------:|----------------|
| S1 | `test-finding-construction` | fast | — |
| S1 | `test-finding-hierarchy` | fast | — |
| S1 | `test-finding-severity-validation` | fast | — |
| S2 | `test-resolution-construction` | fast | — |
| S2 | `test-resolution-kind-validation` | fast | — |
| S2 | `test-composite-resolution-structure` | fast | — |
| S3 | `test-define-inspector-and-list` | fast | — |
| S3 | `test-inspector-last-load-wins` | fast | — |
| S3 | `test-inspector-metadata` | fast | — |
| S4 | `test-define-maintainer-and-list` | fast | — |
| S4 | `test-maintainer-superseding` | fast | — |
| S4 | `test-maintainer-both-maximal` | fast | — |
| S4 | `test-maintainer-nil-fallthrough` | fast | — |
| S5 | `test-prepare-resolution-returns-resolution` | fast | — |
| S5 | `test-prepare-resolution-returns-nil` | fast | — |
| S6 | `test-named-instance-identity` | fast | — |
| S6 | `test-named-instance-redefine` | fast | — |
| S7 | `test-hint-at-file-to-finding` | fast | — |
| S7 | `test-hint-at-file-line-to-finding` | fast | — |
| S7 | `test-lint-with-findings` | slow | Skip if no fixture files on disk |
| S8 | `test-syntax-finding-cst-types` | fast | — |
| S8 | `test-eclector-loads` | fast | — |

---

## Implementation Order (Step Table)

| Step | File | Action | Form(s) | Test name | Category |
|------|------|--------|---------|-----------|:--------:|
| 1 | `src/utilities.lisp` [modify] | Replace `define-persistent-class` with `defclass` in `define-named-class` expansion | `define-named-class` | — | — |
| 2 | `src/finding.lisp` [new] | Implement finding class hierarchy | `finding`, `file-finding`, `line-finding`, `region-finding`, `syntax-finding` + all readers | — | — |
| 3 | `src/resolution.lisp` [new] | Implement resolution class hierarchy | `resolution`, `text-resolution`, `syntax-resolution`, `agent-resolution`, `composite-resolution` + all readers | — | — |
| 4 | `src/inspector.lisp` [new] | Define inspector named-class + `list-inspectors` | `define-named-class inspector`, `list-inspectors` | — | — |
| 5 | `src/maintainer.lisp` [new] | Define maintainer named-class + `prepare-resolution` + `resolve-finding` + superseding logic | `define-named-class maintainer`, `prepare-resolution`, `resolve-finding`, cycle detection | — | — |
| 6 | `src/package.lisp` [modify] | Remove legacy exports, add all new exports, add eclector imports | — | — | — |
| 7 | `org.melusina.atelier.asd` [modify] | Add `eclector-concrete-syntax-tree` dep, add `finding`/`resolution`/`inspector`/`maintainer` to load order, remove `lint` and `inspector/` module, add legacy system definition, update testsuite deps | — | — | — |
| 8 | `src/legacy/package.lisp` [new] | Define `atelier/legacy` package with all legacy exports and required imports | — | — | — |
| 9 | `src/legacy/lint.lisp` [new] | Move `src/lint.lisp` content, change `(in-package #:atelier)` → `(in-package #:atelier/legacy)` | All legacy linter forms | — | — |
| 10 | `src/legacy/inspector/codestyle-0001.lisp` [new] | Move from `src/inspector/`, change in-package | `hint-at-file-when-character-encoding-is-not-utf8` | — | — |
| 11 | `src/legacy/inspector/codestyle-0002.lisp` [new] | Move from `src/inspector/`, change in-package | `hint-at-file-line-when-it-is-very-long` | — | — |
| 12 | `src/legacy/inspector/codestyle-0003.lisp` [new] | Move from `src/inspector/`, change in-package | `hint-at-file-when-it-lacks-canonical-header-line` | — | — |
| 13 | `src/legacy/inspector/codestyle-0004.lisp` [new] | Move from `src/inspector/`, change in-package | `hint-at-file-when-it-lacks-canonical-footer-line` | — | — |
| 14 | `src/legacy/inspector/codestyle-0005.lisp` [new] | Move from `src/inspector/`, change in-package | `hint-at-file-when-it-lacks-canonical-project-identification` | — | — |
| 15 | `src/legacy/inspector/codestyle-0006.lisp` [new] | Move from `src/inspector/`, change in-package | `hint-at-file-when-it-lacks-project-license-information` | — | — |
| 16 | `src/legacy/bridge.lisp` [new] | Implement `hint-to-finding` + `lint-with-findings` | `hint-to-finding`, `lint-with-findings` | — | — |
| 17 | `src/lint.lisp` [delete] | Remove original file (content moved to step 9) | — | — | — |
| 18 | `src/inspector/codestyle-0001.lisp` through `codestyle-0006.lisp` [delete] | Remove original files (content moved to steps 10–15) | — | — | — |
| 19 | `libexec/lisp/development.lisp` [modify] | Change `atelier:lint` → `atelier/legacy:lint`, add `/legacy` to reload list | — | — | — |
| 20 | `testsuite/finding.lisp` [new] | Tests for S1 | `test-finding-construction`, `test-finding-hierarchy`, `test-finding-severity-validation` | `test-finding-*` | fast |
| 21 | `testsuite/resolution.lisp` [new] | Tests for S2 | `test-resolution-construction`, `test-resolution-kind-validation`, `test-composite-resolution-structure` | `test-resolution-*` | fast |
| 22 | `testsuite/inspector.lisp` [new] | Tests for S3, S6 | `test-define-inspector-and-list`, `test-inspector-last-load-wins`, `test-inspector-metadata`, `test-named-instance-identity`, `test-named-instance-redefine` | `test-inspector-*` | fast |
| 23 | `testsuite/maintainer.lisp` [new] | Tests for S4, S5 | `test-define-maintainer-and-list`, `test-maintainer-superseding`, `test-maintainer-both-maximal`, `test-maintainer-nil-fallthrough`, `test-prepare-resolution-returns-resolution`, `test-prepare-resolution-returns-nil` | `test-maintainer-*` | fast |
| 24 | `testsuite/bridge.lisp` [new] | Tests for S7 | `test-hint-at-file-to-finding`, `test-hint-at-file-line-to-finding`, `test-lint-with-findings` | `test-bridge-*` | fast/slow |
| 25 | `testsuite/lint.lisp` [modify] | Update all `atelier::` references to `atelier/legacy::` | — | — | — |
| 26 | `testsuite/package.lisp` [modify] | Add imports for new test dependencies | — | — | — |
| 27 | `testsuite/entrypoint.lisp` [modify] | Add `testsuite-finding`, `testsuite-resolution`, `testsuite-inspector`, `testsuite-maintainer`, `testsuite-bridge` | — | — | — |

---

## Invariants

| # | Invariant |
|---|-----------|
| I1 | All `defsystem` forms live in `org.melusina.atelier.asd`. |
| I2 | Never use `shadowing-import` or `:shadowing-import-from` to resolve symbol conflicts. |
| I3 | `assert-condition` argument order: `(assert-condition FORM CONDITION-TYPE)` — form first. |
| I4 | `assert-t` checks for exactly `T`. Use `(assert-t (not (null X)))` for generalised boolean. |
| I5 | LOOP clause keywords are always keyword symbols (`:for`, `:in`, `:collect`). |
| I6 | `MAPCAR`/`MAPCAN`/`REMOVE-IF` receive a named `FLET` function, never a bare `LAMBDA`. |
| I7 | No SBCL-specific extensions without an explicit `#+sbcl` guard. |
| I8 | Every source file begins with `;;;; filename — Description` and ends with `;;;; End of file 'filename'`. |
| I9 | `define-named-class` expands to plain `defclass`, not `define-persistent-class`. |
| I10 | Named-instance initargs must not use `:documentation` (clashes with `cl:documentation`). Use `:description` instead. |
| I11 | Inspector identity is a symbol. Legacy inspectors use keyword codes (`:codestyle-0001`); keywords are symbols, so this is valid. |
| I12 | `rationale` is a regular instance slot on `finding`, not `:allocation :class`. It is "per-class" by convention: the creator provides the same rationale for all findings of the same kind. |
| I13 | `text-resolution` does not carry its own location — it derives location from its linked `finding`. |
| I14 | `composite-resolution` ordering (innermost CST nodes first) is documented in the docstring but not enforced at construction time. |
| I15 | The `atelier/legacy` package does not `:use` the `atelier` package — it `:import-from` specific symbols to avoid future conflicts. |
| I16 | Superseding cycles in the maintainer registry are detected at `define-maintainer` time and signal `simple-error`. |

---

## Test Fixtures

**Finding tests (fast):** No fixtures. Instances are constructed in-memory with literal slot values.

**Resolution tests (fast):** No fixtures. A mock `prepare-fn` lambda is used for maintainer tests.

**Inspector/maintainer tests (fast):** Test inspectors and maintainers are defined inline within each test case using `define-inspector` and `define-maintainer`. Each test clears the registry (`clrhash *inspectors*` / `clrhash *maintainers*`) in a setup step to isolate tests.

**Bridge tests:**
- Fast tests: construct `hint-at-file` and `hint-at-file-line` instances directly, call `hint-to-finding`, verify the resulting finding slots.
- Slow test (`test-lint-with-findings`): uses existing fixture files (e.g., `testsuite/` source files) as linting targets. Skip condition: files not found on disk (always present when running from source tree).

---

## References to Create

None for this phase.

---

## Acceptance Criteria

| # | Criterion | Verification |
|---|-----------|-------------|
| AC1 | All five finding classes instantiate with correct slot types | `(run-all-tests)` — `test-finding-construction` passes |
| AC2 | `syntax-finding` slots `cst-node` and `cst-root` accept `concrete-syntax-tree:cst` instances | `test-syntax-finding-cst-types` passes |
| AC3 | `define-inspector` registers an inspector retrievable by `find-inspector` and visible in `list-inspectors` | `test-define-inspector-and-list` passes |
| AC4 | `define-maintainer` with `:supersedes` produces correct partial order: only maximal maintainers run in `resolve-finding` | `test-maintainer-superseding` passes |
| AC5 | `prepare-resolution` returns a `resolution` or `NIL` | `test-prepare-resolution-returns-resolution` and `test-prepare-resolution-returns-nil` pass |
| AC6 | `hint-to-finding` converts `hint-at-file` → `file-finding` and `hint-at-file-line` → `line-finding` with correct slot mapping | `test-hint-at-file-to-finding` and `test-hint-at-file-line-to-finding` pass |
| AC7 | Existing testsuite passes after legacy move (all `testsuite-linter` tests) | `(atelier/testsuite:run-all-tests)` — all tests pass |
| AC8 | `(asdf:load-system "org.melusina.atelier")` loads successfully with Eclector as a hard dependency | System loads without error in a fresh SBCL image |
| AC9 | Public API exports ≥ 25 symbols | `(length (loop :for s :being :the :external-symbols :of :atelier :when (or (find-class s nil) (fboundp s) (boundp s)) :collect s))` ≥ 25 |
| AC10 | `atelier/development:lint` still lints the project successfully via the legacy system | `(atelier/development:lint)` completes without error |

---

## Phase Closure Conditions

This phase is complete when:
1. All 10 acceptance criteria (AC1–AC10) are verified.
2. All fast tests pass (< 2 seconds total).
3. All slow tests pass (or individually explained if skipped).
4. `(asdf:load-system "org.melusina.atelier")` and `(asdf:load-system "org.melusina.atelier/legacy")` both succeed in a fresh image.
5. `(atelier/testsuite:run-all-tests)` passes.
6. No SBCL-specific code without `#+sbcl` guard.
