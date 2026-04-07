# Implementation Phase 1: CST-Level Inspectors

**Slice:** product/slice/004-cst-inspectors/slice.md
**Phase:** 1 of 1
**Scope:** Implement syntax inspector infrastructure (Eclector parsing, `inspect-syntax` protocol, runner stage 3), four concrete CST inspectors (earmuffs, constant naming, bare lambda, bare loop keywords), and supporting helpers for CST pattern matching and `syntax-finding` construction.
**Prerequisites:** Slices 001–003 complete.

---

## Back-link

Slice: product/slice/004-cst-inspectors/slice.md

## Prior Phases

None for this slice. Slices 001–003 established: finding/resolution schema, ASDF integration, staged runner (file → line), `inspect-file`/`inspect-line` protocol, `define-file-inspector`/`define-line-inspector` macros.

---

## Risk Register

| # | Risk | Category | Mitigation |
|---|------|----------|-----------|
| R1 | Eclector may fail on non-Lisp files or files with reader macros | Library API | Only run syntax inspectors on `.lisp`/`.asd` files; guard with `handler-case` around parse |
| R2 | CST source positions are character offsets, not line/column — converting requires reading the file | Scope boundary | Provide `source-position-to-line-column` helper that maps offsets using the line vector |
| R3 | `loop` macro keyword detection may have edge cases (e.g., `loop-finish`, `named`) | Scope boundary | Only flag the well-known clause keywords: `for`, `in`, `across`, `collect`, `do`, `when`, `unless`, `while`, `until`, `return`, `finally`, `with`, `repeat`, `initially`, `append`, `nconc`, `count`, `sum`, `maximize`, `minimize` |
| R4 | Bare lambda check must distinguish `(mapcar (lambda ...) ...)` from `(funcall (lambda ...) ...)` which is fine | Scope boundary | Target specific higher-order functions: `mapcar`, `mapcan`, `mapc`, `remove-if`, `remove-if-not`, `find-if`, `find-if-not`, `count-if`, `count-if-not`, `every`, `some`, `notany`, `notevery`, `reduce`, `sort`, `stable-sort` |
| R5 | Parsing the file a third time (after file and line stages) is redundant | Portability | Acceptable for now — the pipeline design defers single-parse optimization to the future reduce-based runner |

---

## OSS Components

None — CST pattern matching is straightforward tree walking on the existing Eclector CST.

---

## Phase Scope

**Must deliver:** S1 (earmuffs), S2 (constant naming), S3 (bare lambda), S4 (bare loop keywords), S5 (syntax inspector infrastructure).

**Deferred:** None.

---

## File Organisation

```
src/
├── package.lisp                              [modify] — new exports
├── finding.lisp                              [modify] — add 4 finding subclasses
├── inspector.lisp                            [modify] — add define-syntax-inspector macro
├── runner.lisp                               [modify] — add stage 3, parse-lisp-file, source helpers
├── inspectors/
│   ├── check-earmuffs.lisp                   [new] — S1
│   ├── check-constant-naming.lisp            [new] — S2
│   ├── check-bare-lambda.lisp                [new] — S3
│   └── check-loop-keywords.lisp              [new] — S4

testsuite/
├── inspectors/
│   ├── check-earmuffs.lisp                   [new] — S1 tests
│   ├── check-constant-naming.lisp            [new] — S2 tests
│   ├── check-bare-lambda.lisp                [new] — S3 tests
│   └── check-loop-keywords.lisp              [new] — S4 tests
├── fixtures/
│   ├── earmuffs-good.lisp                    [new]
│   ├── earmuffs-bad.lisp                     [new]
│   ├── constant-naming-bad.lisp              [new]
│   ├── bare-lambda.lisp                      [new]
│   └── bare-loop-keywords.lisp               [new]
├── entrypoint.lisp                           [modify] — add new test groups

org.melusina.atelier.asd                      [modify] — add new inspector files + test files
```

---

## Build System Changes

### `org.melusina.atelier` — inspectors module

Add: `check-earmuffs`, `check-constant-naming`, `check-bare-lambda`, `check-loop-keywords`.

### `org.melusina.atelier/testsuite` — inspectors module

Add: `check-earmuffs`, `check-constant-naming`, `check-bare-lambda`, `check-loop-keywords`.

---

## Package / Module Architecture

### New exports from `atelier`

**Finding subclasses:**
`earmuffs-finding`, `constant-naming-finding`, `bare-lambda-finding`, `bare-loop-keyword-finding`

**Concrete inspectors:**
`check-earmuffs`, `check-constant-naming`, `check-bare-lambda`, `check-loop-keywords`

**Infrastructure:**
`inspect-syntax`, `parse-lisp-file`, `make-syntax-finding-from-form`,
`source-position-to-line-column`, `define-syntax-inspector`

**CST helpers:**
`cst-form-operator`, `cst-form-operator-p`

---

## Type / Class Hierarchy

### New finding subclasses

```
syntax-finding
├── earmuffs-finding                [concrete]
├── constant-naming-finding         [concrete]
├── bare-lambda-finding             [concrete]
└── bare-loop-keyword-finding       [concrete]
```

### Inspector instances

```
syntax-inspector
├── check-earmuffs                  [concrete singleton]
├── check-constant-naming           [concrete singleton]
├── check-bare-lambda               [concrete singleton]
└── check-loop-keywords             [concrete singleton]
```

---

## Protocol Definitions

```lisp
(defgeneric inspect-syntax (inspector form)
  (:documentation "Run INSPECTOR on a single top-level CST FORM.
Return a list of findings, or NIL. FORM is a CONCRETE-SYNTAX-TREE:CST node.
*CURRENT-PATHNAME* and *CURRENT-CST-ROOT* are bound by the caller.")
  (:method ((inspector inspector) form)
    (declare (ignore form))
    nil))
```

```lisp
(defmethod inspect-file ((inspector syntax-inspector) (pathname pathname))
  "Parse PATHNAME with Eclector and delegate to the CST method.")

(defmethod inspect-file ((inspector syntax-inspector) (cst-root list))
  "Walk top-level CST forms and call INSPECT-SYNTAX on each.")
```

```lisp
(defun parse-lisp-file (pathname)
  "Parse PATHNAME with Eclector and return a list of top-level CST forms.
Return NIL if the file cannot be parsed (non-Lisp, reader errors).")
```

```lisp
(defun make-syntax-finding-from-form (form &key inspector severity observation rationale)
  "Create a SYNTAX-FINDING from CST FORM, deriving file, line, column from source positions.")
```

```lisp
(defun source-position-to-line-column (position line-vector)
  "Convert a character POSITION to (line . column) using LINE-VECTOR.")
```

---

## Error / Condition Types

None new. Eclector parse errors are caught with `handler-case` in `parse-lisp-file`.

---

## Test Plan

| Story | Test name | Category | Skip condition |
|-------|-----------|:--------:|----------------|
| S1 | `validate-check-earmuffs-correct` | slow | Fixture not found |
| S1 | `validate-check-earmuffs-violation` | slow | Fixture not found |
| S1 | `validate-check-earmuffs-registered` | fast | — |
| S2 | `validate-check-constant-naming-violation` | slow | Fixture not found |
| S2 | `validate-check-constant-naming-registered` | fast | — |
| S3 | `validate-check-bare-lambda-violation` | slow | Fixture not found |
| S3 | `validate-check-bare-lambda-named-function` | slow | Fixture not found |
| S3 | `validate-check-bare-lambda-registered` | fast | — |
| S4 | `validate-check-loop-keywords-violation` | slow | Fixture not found |
| S4 | `validate-check-loop-keywords-correct` | slow | Fixture not found |
| S4 | `validate-check-loop-keywords-registered` | fast | — |
| S5 | `validate-parse-lisp-file` | slow | Fixture not found |
| S5 | `validate-syntax-inspector-in-runner` | slow | Fixture not found |

---

## Implementation Order (Step Table)

| Step | File | Action | Form(s) | Test name | Category |
|------|------|--------|---------|-----------|:--------:|
| 1 | `src/finding.lisp` [modify] | Add 4 finding subclasses | `earmuffs-finding`, `constant-naming-finding`, `bare-lambda-finding`, `bare-loop-keyword-finding` | — | — |
| 2 | `src/runner.lisp` [modify] | Add `inspect-syntax` generic, `parse-lisp-file`, `source-position-to-line-column`, `make-syntax-finding-from-form`, `*current-cst-root*`, `inspect-file` methods for `syntax-inspector`, runner stage 3 | `inspect-syntax`, `parse-lisp-file`, `source-position-to-line-column`, `make-syntax-finding-from-form` | — | — |
| 3 | `src/inspector.lisp` [modify] | Add `define-syntax-inspector` macro, `cst-form-operator`, `cst-form-operator-p` | `define-syntax-inspector`, `cst-form-operator`, `cst-form-operator-p` | — | — |
| 4 | `src/inspectors/check-earmuffs.lisp` [new] | Implement | `check-earmuffs`, `inspect-syntax` method | — | — |
| 5 | `src/inspectors/check-constant-naming.lisp` [new] | Implement | `check-constant-naming`, `inspect-syntax` method | — | — |
| 6 | `src/inspectors/check-bare-lambda.lisp` [new] | Implement | `check-bare-lambda`, `inspect-syntax` method | — | — |
| 7 | `src/inspectors/check-loop-keywords.lisp` [new] | Implement | `check-loop-keywords`, `inspect-syntax` method | — | — |
| 8 | `src/package.lisp` [modify] | Add new exports | — | — | — |
| 9 | `org.melusina.atelier.asd` [modify] | Add new inspector and test files | — | — | — |
| 10 | `testsuite/fixtures/earmuffs-good.lisp` [new] | Fixture with correct earmuffs | — | — | — |
| 11 | `testsuite/fixtures/earmuffs-bad.lisp` [new] | Fixture with missing earmuffs | — | — | — |
| 12 | `testsuite/fixtures/constant-naming-bad.lisp` [new] | Fixture with wrong constant name | — | — | — |
| 13 | `testsuite/fixtures/bare-lambda.lisp` [new] | Fixture with bare and named function variants | — | — | — |
| 14 | `testsuite/fixtures/bare-loop-keywords.lisp` [new] | Fixture with bare and keyword loop symbols | — | — | — |
| 15 | `testsuite/inspectors/check-earmuffs.lisp` [new] | S1 tests | `validate-check-earmuffs-*` | slow |
| 16 | `testsuite/inspectors/check-constant-naming.lisp` [new] | S2 tests | `validate-check-constant-naming-*` | slow |
| 17 | `testsuite/inspectors/check-bare-lambda.lisp` [new] | S3 tests | `validate-check-bare-lambda-*` | slow |
| 18 | `testsuite/inspectors/check-loop-keywords.lisp` [new] | S4 tests | `validate-check-loop-keywords-*` | slow |
| 19 | `testsuite/entrypoint.lisp` [modify] | Add new test groups | — | — | — |

---

## Invariants

| # | Invariant |
|---|-----------|
| I1–I30 | Carried forward from slices 001–003 |
| I31 | `inspect-syntax` receives each top-level CST form individually. The inspector decides how deep to walk. |
| I32 | `parse-lisp-file` returns NIL on non-Lisp files or parse errors — never signals. |
| I33 | `make-syntax-finding-from-form` derives line/column from CST source positions using the line vector. |
| I34 | `define-syntax-inspector` generates `inspect-syntax` methods, following the same pattern as `define-line-inspector` generates `inspect-line`. |
| I35 | `*current-cst-root*` is bound to the list of top-level forms during syntax inspection. |
| I36 | Syntax inspectors only run on files parseable by Eclector (`.lisp`, `.asd`). Non-parseable files are silently skipped. |
| I37 | The bare lambda check targets a specific list of higher-order functions (mapcar, mapcan, remove-if, etc.), not all function calls. |
| I38 | The loop keyword check targets well-known clause keywords only. |

---

## Test Fixtures

All fixtures follow canonical project format (header, footer, license, SPDX).

**`earmuffs-good.lisp`** — `(defvar *correct* 1)` and `(defparameter *also-correct* 2)`.
**`earmuffs-bad.lisp`** — `(defvar wrong-name 1)` and `(defparameter also-wrong 2)`.
**`constant-naming-bad.lisp`** — `(defconstant wrong-name 42)`.
**`bare-lambda.lisp`** — both `(mapcar (lambda (x) x) list)` and `(mapcar #'identity list)`.
**`bare-loop-keywords.lisp`** — both `(loop for x in list collect x)` and `(loop :for x :in list :collect x)`.

---

## References to Create

None.

---

## Acceptance Criteria

| # | Criterion | Verification |
|---|-----------|-------------|
| AC1 | `check-earmuffs` produces `earmuffs-finding` for `defvar` without `*name*` | `validate-check-earmuffs-violation` passes |
| AC2 | `check-constant-naming` produces `constant-naming-finding` for `defconstant` without `+name+` | `validate-check-constant-naming-violation` passes |
| AC3 | `check-bare-lambda` produces `bare-lambda-finding` for `(mapcar (lambda ...) ...)` | `validate-check-bare-lambda-violation` passes |
| AC4 | `check-loop-keywords` produces `bare-loop-keyword-finding` for bare loop symbols | `validate-check-loop-keywords-violation` passes |
| AC5 | Syntax inspectors run through the runner pipeline | `validate-syntax-inspector-in-runner` passes |
| AC6 | `parse-lisp-file` parses fixture files into CST forms | `validate-parse-lisp-file` passes |
| AC7 | Findings carry valid CST node references and correct line/column | Verified in each inspector test |
| AC8 | ≥ 4 syntax inspectors registered | `(length (remove-if-not ...))` ≥ 4 |
| AC9 | Full test suite passes | `(atelier/testsuite:run-all-tests)` — all tests pass |

---

## Phase Closure Conditions

1. All 9 acceptance criteria verified.
2. All fast tests pass.
3. All slow tests pass (fixture files present from source tree).
4. `(asdf:load-system "org.melusina.atelier")` and `(asdf:load-system "org.melusina.atelier/legacy")` succeed.
5. `(atelier/testsuite:run-all-tests)` passes.
6. No SBCL-specific code without `#+sbcl` guard.
