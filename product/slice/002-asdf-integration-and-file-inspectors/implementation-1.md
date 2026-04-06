# Implementation Phase 1: ASDF Integration and First File-Level Inspectors

**Slice:** product/slice/002-asdf-integration-and-file-inspectors/slice.md
**Phase:** 1 of 1
**Scope:** Rework inspector and maintainer to use individual classes with CLOS dispatch (replacing EQL specializers and define-named-class). Implement ASDF `linter-op` operation with `project-configuration` and `linter-configuration` component types. Implement two concrete file-level inspectors (UTF-8 encoding, SPDX license header) with their own finding subclasses. Remove superseded legacy inspectors.
**Prerequisites:** Slice 001 complete (finding/resolution schema, inspector/maintainer registry).

---

## Back-link

Slice: product/slice/002-asdf-integration-and-file-inspectors/slice.md

## Prior Phases

Slice 001 (Phase 1 complete): Finding hierarchy, resolution hierarchy, inspector registry via `define-named-class`, maintainer registry with `define-maintainer` macro and CLOS dispatch on EQL specializers, legacy linter in `org.melusina.atelier/legacy`, bridge layer.

---

## Risk Register

| # | Risk | Category | Mitigation |
|---|------|----------|-----------|
| R1 | ASDF custom component types may behave differently across ASDF versions | Library API | Use standard ASDF API (`component-pathname`, `system-source-directory`); test with current Quicklisp dist |
| R2 | Reworking inspector and maintainer from EQL/define-named-class to individual classes breaks slice 001 tests | Scope boundary | Update all existing tests in the same step; run full suite before proceeding |
| R3 | `.sexp` configuration parsing with `*read-eval*` NIL may reject `#.` reader macros | Library API | Document restriction; it is a security feature |
| R4 | Fixture files for encoding tests may be corrupted by git | Test dependency | Generate non-UTF-8 fixture programmatically at test time |
| R5 | Template files may reference removed exports | Scope boundary | Grep templates for removed symbols during rework |
| R6 | Removing legacy inspectors (codestyle-0001, codestyle-0006) breaks legacy testsuite | Scope boundary | Remove corresponding legacy tests; update testsuite-linter |
| R7 | `define-inspector`/`define-maintainer` signature change breaks no external consumers | Scope boundary | Slice 001 registry was empty; no backward compatibility concern |

---

## OSS Components

None — implementing from scratch. ASDF custom operations and components are standard ASDF API.

---

## Phase Scope

**Must deliver:** S1 (project-configuration), S2 (linter-configuration), S3 (linter-op), S4 (encoding inspector), S5 (SPDX inspector), S6 (runner). Plus rework of inspector/maintainer to individual classes, and removal of superseded legacy inspectors (codestyle-0001, codestyle-0006).

**Deferred:** None.

---

## File Organisation

```
src/
├── package.lisp                  [modify] — update exports
├── inspector.lisp                [rewrite] — manual class hierarchy, custom define-inspector macro
├── maintainer.lisp               [rewrite] — individual classes, custom define-maintainer macro
├── finding.lisp                  [modify] — add encoding-finding, spdx-license-header-finding subclasses
├── asdf.lisp                     [new] — project-configuration, linter-configuration, linter-op
├── runner.lisp                   [new] — run-file-inspectors, inspect-file generic
├── inspectors/
│   ├── check-file-encoding.lisp       [new] — concrete inspector
│   └── check-spdx-license-header.lisp [new] — concrete inspector
└── legacy/
    ├── lint.lisp                 [modify] — remove references to deleted inspectors
    ├── bridge.lisp               [unchanged]
    ├── package.lisp              [modify] — remove deleted inspector exports
    └── inspector/
        ├── codestyle-0001.lisp   [delete] — superseded by check-file-encoding
        ├── codestyle-0002.lisp   [unchanged]
        ├── codestyle-0003.lisp   [unchanged]
        ├── codestyle-0004.lisp   [unchanged]
        ├── codestyle-0005.lisp   [unchanged]
        └── codestyle-0006.lisp   [delete] — superseded by check-spdx-license-header

testsuite/
├── package.lisp                  [modify] — add new imports
├── inspector.lisp                [rewrite] — individual class tests
├── maintainer.lisp               [rewrite] — individual class tests
├── asdf.lisp                     [new] — S1, S2, S3 tests
├── runner.lisp                   [new] — S6 tests
├── inspectors/
│   ├── check-file-encoding.lisp       [new] — S4 tests
│   └── check-spdx-license-header.lisp [new] — S5 tests
├── inspector/
│   ├── codestyle-0001.lisp       [delete] — inspector removed
│   ├── codestyle-0006.lisp       [delete] — inspector removed
│   └── (others unchanged)
├── lint.lisp                     [modify] — remove deleted test references
├── entrypoint.lisp               [modify] — add new test groups
└── fixtures/
    ├── valid-utf8.lisp           [new]
    ├── valid-with-spdx.lisp      [new]
    ├── missing-spdx.lisp         [new]
    └── wrong-spdx.lisp           [new]
```

---

## Build System Changes

### `org.melusina.atelier` (modified)

Load order:
```
package → utilities → configuration → license → parameter → template
→ finding → resolution → inspector → maintainer
→ asdf → runner
→ (:module "inspectors" — check-file-encoding, check-spdx-license-header)
→ main
```

### `org.melusina.atelier/legacy` (modified)

Remove `codestyle-0001` and `codestyle-0006` from inspector module.

### `org.melusina.atelier/testsuite` (modified)

Add: `asdf`, `runner`, `(:module "inspectors" — check-file-encoding, check-spdx-license-header)`.
Remove: `codestyle-0001`, `codestyle-0006` from legacy-inspector module.

---

## Package / Module Architecture

### New exports from `atelier`

**Inspector rework:**
`file-inspector`, `line-inspector`, `region-inspector`, `syntax-inspector`,
`define-file-inspector`, `define-line-inspector`, `define-region-inspector`, `define-syntax-inspector`,
`inspect-file`

**Maintainer rework:**
`automatic-maintainer`, `agent-maintainer`,
`define-automatic-maintainer`, `define-agent-maintainer`

**Finding subclasses:**
`encoding-finding`, `spdx-license-header-finding`

**Concrete inspectors:**
`check-file-encoding`, `check-spdx-license-header`

**ASDF integration:**
`project-configuration`, `linter-configuration`,
`linter-op`,
`make-project-configuration`, `project-configuration-license`,
`project-configuration-copyright-holder`, `project-configuration-homepage`,
`make-linter-configuration`, `linter-configuration-disabled-inspectors`,
`linter-configuration-severity-overrides`,
`read-project-configuration`, `read-linter-configuration`

**Runner:**
`run-file-inspectors`

### Removed exports from `atelier`

`inspector-languages` (already removed in rework-1), `make-maintainer` (replaced by `make-automatic-maintainer` / `make-agent-maintainer`).

---

## Type / Class Hierarchy

### Inspector hierarchy (reworked)

```
inspector                          [abstract base]
├── file-inspector                 [abstract — level :file]
│   ├── check-file-encoding        [concrete singleton]
│   └── check-spdx-license-header  [concrete singleton]
├── line-inspector                 [abstract — level :line]
├── region-inspector               [abstract — level :region]
└── syntax-inspector               [abstract — level :syntax]
```

### Maintainer hierarchy (reworked)

```
maintainer                         [abstract base]
├── automatic-maintainer           [abstract — kind :automatic]
└── agent-maintainer               [abstract — kind :agent]
```

### Finding subclasses (new)

```
file-finding
├── encoding-finding               [concrete]
└── spdx-license-header-finding    [concrete]
```

### ASDF types (new)

```
asdf:component
├── project-configuration          [concrete]
└── linter-configuration           [concrete]

asdf:operation
└── linter-op                      [concrete]
```

---

## Protocol Definitions

```lisp
(defgeneric inspect-file (inspector pathname project-configuration)
  (:documentation "Run INSPECTOR on PATHNAME and return a list of findings or NIL.
PROJECT-CONFIGURATION may be NIL when no project configuration is declared."))
```

```lisp
(defun run-file-inspectors (pathname project-configuration linter-configuration)
  "Run all registered file-level inspectors on PATHNAME, respecting
LINTER-CONFIGURATION policy. Return a list of findings.")
```

```lisp
(defmethod asdf:perform ((operation linter-op) (component asdf:cl-source-file))
  "Inspect COMPONENT's source file with all registered file-level inspectors.")
```

---

## Error / Condition Types

| Name | Superclass | Slots | When signalled |
|------|-----------|-------|----------------|
| (none new) | — | — | Configuration parse errors use `error` with descriptive string |

---

## Test Plan

| Story | Test name | Category | Skip condition |
|-------|-----------|:--------:|----------------|
| Rework | `validate-inspector-individual-class` | fast | — |
| Rework | `validate-maintainer-individual-class` | fast | — |
| Rework | `validate-maintainer-finding-class-dispatch` | fast | — |
| S1 | `validate-read-project-configuration` | slow | Fixture file not found |
| S1 | `validate-project-configuration-defaults` | fast | — |
| S2 | `validate-read-linter-configuration` | slow | Fixture file not found |
| S2 | `validate-linter-configuration-disables-inspector` | fast | — |
| S2 | `validate-linter-configuration-severity-override` | fast | — |
| S3 | `validate-linter-op-on-system` | slow | Fixture system not found |
| S3 | `validate-linter-op-default-policy` | slow | Fixture system not found |
| S4 | `validate-check-file-encoding-valid` | slow | Fixture file not found |
| S4 | `validate-check-file-encoding-invalid` | slow | Programmatic fixture |
| S4 | `validate-check-file-encoding-registered` | fast | — |
| S5 | `validate-check-spdx-header-present` | slow | Fixture file not found |
| S5 | `validate-check-spdx-header-missing` | slow | Fixture file not found |
| S5 | `validate-check-spdx-header-mismatch` | slow | Fixture file not found |
| S6 | `validate-run-file-inspectors` | slow | Fixture file not found |
| S6 | `validate-run-file-inspectors-respects-policy` | fast | — |

---

## Implementation Order (Step Table)

| Step | File | Action | Form(s) | Test name | Category |
|------|------|--------|---------|-----------|:--------:|
| 1 | `src/inspector.lisp` [rewrite] | Rework to manual class hierarchy + custom macro | `inspector`, `file-inspector`, `line-inspector`, `region-inspector`, `syntax-inspector`, `define-inspector`, `define-file-inspector`, `define-line-inspector`, `define-region-inspector`, `define-syntax-inspector`, registry functions | — | — |
| 2 | `src/maintainer.lisp` [rewrite] | Rework to individual classes | `automatic-maintainer`, `agent-maintainer`, `define-automatic-maintainer`, `define-agent-maintainer`, `prepare-resolution` dispatches on class | — | — |
| 3 | `src/finding.lisp` [modify] | Add concrete finding subclasses | `encoding-finding`, `spdx-license-header-finding` | — | — |
| 4 | `src/asdf.lisp` [new] | ASDF component types + linter-op | `project-configuration`, `linter-configuration`, `linter-op`, readers, `perform` methods | — | — |
| 5 | `src/runner.lisp` [new] | Inspector runner | `inspect-file`, `run-file-inspectors` | — | — |
| 6 | `src/inspectors/check-file-encoding.lisp` [new] | Concrete inspector | `check-file-encoding`, `inspect-file` method | — | — |
| 7 | `src/inspectors/check-spdx-license-header.lisp` [new] | Concrete inspector | `check-spdx-license-header`, `inspect-file` method | — | — |
| 8 | `src/package.lisp` [modify] | Update all exports | — | — | — |
| 9 | `org.melusina.atelier.asd` [modify] | Add new files, update load order, remove legacy codestyle-0001/0006 | — | — | — |
| 10 | `src/legacy/inspector/codestyle-0001.lisp` [delete] | Superseded | — | — | — |
| 11 | `src/legacy/inspector/codestyle-0006.lisp` [delete] | Superseded | — | — | — |
| 12 | `src/legacy/package.lisp` [modify] | Remove deleted inspector references | — | — | — |
| 13 | `src/legacy/lint.lisp` [modify] | Remove references to deleted inspectors | — | — | — |
| 14 | `testsuite/inspector.lisp` [rewrite] | Tests for reworked inspectors | `validate-inspector-individual-class` | fast |
| 15 | `testsuite/maintainer.lisp` [rewrite] | Tests for reworked maintainers | `validate-maintainer-individual-class`, `validate-maintainer-finding-class-dispatch` | fast |
| 16 | `testsuite/fixtures/` [new] | Create fixture files for inspector tests | — | — |
| 17 | `testsuite/asdf.lisp` [new] | S1, S2, S3 tests | `validate-read-project-configuration`, `validate-linter-op-on-system`, etc. | slow |
| 18 | `testsuite/runner.lisp` [new] | S6 tests | `validate-run-file-inspectors`, `validate-run-file-inspectors-respects-policy` | fast/slow |
| 19 | `testsuite/inspectors/check-file-encoding.lisp` [new] | S4 tests | `validate-check-file-encoding-*` | fast/slow |
| 20 | `testsuite/inspectors/check-spdx-license-header.lisp` [new] | S5 tests | `validate-check-spdx-header-*` | fast/slow |
| 21 | `testsuite/inspector/codestyle-0001.lisp` [delete] | Superseded | — | — |
| 22 | `testsuite/inspector/codestyle-0006.lisp` [delete] | Superseded | — | — |
| 23 | `testsuite/lint.lisp` [modify] | Remove deleted test references | — | — |
| 24 | `testsuite/entrypoint.lisp` [modify] | Add new test groups | — | — |
| 25 | `testsuite/package.lisp` [modify] | Update imports | — | — |
| 26 | `libexec/lisp/development.lisp` [modify] | Update if needed | — | — |

---

## Invariants

| # | Invariant |
|---|-----------|
| I1 | All `defsystem` forms live in `org.melusina.atelier.asd`. |
| I2 | Never use `shadowing-import` or `:shadowing-import-from`. |
| I3 | `assert-condition` argument order: form first, type second. |
| I4 | `assert-t` checks for exactly `T`. Use `(assert-t (not (null X)))` for generalised boolean. |
| I5 | LOOP clause keywords are always keyword symbols. |
| I6 | `MAPCAR`/`MAPCAN`/`REMOVE-IF` receive a named `FLET` function, never a bare `LAMBDA`. |
| I7 | No SBCL-specific extensions without `#+sbcl` guard. |
| I8 | Every source file begins with `;;;; filename — Description` and ends with `;;;; End of file 'filename'`. |
| I9 | `define-named-class` expands to plain `defclass`. |
| I10 | Named-instance initargs use `:description` not `:documentation`. |
| I11 | Inspector identity is a symbol — also the class name of its singleton. |
| I12 | `rationale` is a regular instance slot on `finding`, per-class by convention. |
| I13 | `text-resolution` derives location from linked finding. |
| I14 | `composite-resolution` ordering documented but not enforced. |
| I15 | `atelier/legacy` uses `:import-from`, not `:use`. |
| I16 | Superseding cycle detection at `define-maintainer` time. |
| I17 | Each concrete inspector is a subclass of a level class (`file-inspector`, `line-inspector`, etc.) with a singleton instance in the registry. |
| I18 | Each concrete maintainer is a subclass of a kind class (`automatic-maintainer`, `agent-maintainer`) with a singleton instance in the registry. |
| I19 | `define-inspector` and `define-maintainer` macros generate: subclass + singleton + method. Convenience macros (`define-file-inspector`, `define-automatic-maintainer`, etc.) pre-fill the superclass. |
| I20 | Finding subclasses are defined independently of inspectors — part of the public schema. |
| I21 | Configuration files are read with `*read-eval*` bound to NIL. |
| I22 | `linter-op` uses ASDF's `perform` per-component — ASDF walks the tree. |
| I23 | Resolutions in this slice are declarative (intent only) — write-back is a later slice. |

---

## Test Fixtures

**Fixture files** (`testsuite/fixtures/`): small Lisp and Shell files with known encoding and SPDX header states.
**Non-UTF-8 fixture**: generated programmatically at test time (write invalid bytes to a temp file) to avoid git corruption (R5).
**Fixture ASDF system**: a minimal `.asd` + source files in a temp directory, created at test time.

---

## References to Create

None.

---

## Acceptance Criteria

| # | Criterion | Verification |
|---|-----------|-------------|
| AC1 | Inspector and maintainer use individual classes with CLOS dispatch | `validate-inspector-individual-class`, `validate-maintainer-individual-class` pass |
| AC2 | `project-configuration` component reads `.sexp` file | `validate-read-project-configuration` passes |
| AC3 | `linter-configuration` component reads `.sexp` with `*read-eval*` NIL | `validate-read-linter-configuration` passes |
| AC4 | `(asdf:operate 'atelier:linter-op :fixture-system)` produces findings | `validate-linter-op-on-system` passes |
| AC5 | `check-file-encoding` produces `encoding-finding` for non-UTF-8 files | `validate-check-file-encoding-invalid` passes |
| AC6 | `check-spdx-license-header` produces `spdx-license-header-finding` for missing/wrong headers | `validate-check-spdx-header-missing`, `validate-check-spdx-header-mismatch` pass |
| AC7 | Runner respects linter-configuration disabled inspectors | `validate-run-file-inspectors-respects-policy` passes |
| AC8 | Legacy codestyle-0001 and codestyle-0006 are removed | Files deleted, testsuite passes without them |
| AC9 | ≥ 3 concrete inspectors registered | `(length (atelier:list-inspectors))` ≥ 3 |
| AC10 | Full test suite passes | `(atelier/testsuite:run-all-tests)` — all tests pass |

---

## Phase Closure Conditions

1. All 10 acceptance criteria verified.
2. All fast tests pass.
3. All slow tests pass (fixture files present when running from source tree).
4. `(asdf:load-system "org.melusina.atelier")` and `(asdf:load-system "org.melusina.atelier/legacy")` succeed.
5. `(atelier/testsuite:run-all-tests)` passes.
6. No SBCL-specific code without `#+sbcl` guard.
