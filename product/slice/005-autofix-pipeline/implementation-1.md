# Implementation Phase 1 — Autofix Pipeline

**Slice:** product/slice/005-autofix-pipeline/slice.md
**Phase:** 1 of 1
**Scope:** Deliver the complete autofix pipeline: write-back engine, six automatic maintainers (trailing whitespace, mixed indentation, earmuffs, constant naming, bare loop keywords, bare lambda), pretty-printer dispatch table, and `lint-system :autofix` integration.

---

## Prior Phases

None — this is the first phase.

---

## Risk Register

| # | Risk | Category | Mitigation |
|---|------|----------|-----------|
| R1 | SBCL's default pprint for `flet`/`loop` may not match expected fixture output | Library API uncertainty | Write fixtures first, test against SBCL defaults, add custom dispatch entries only where needed |
| R2 | Character position arithmetic off-by-one for line findings (line/column to char offset) | State and lifecycle | Unit test the conversion function in isolation with known inputs before using in write-back |
| R4 | `rename-file` semantics differ across implementations (SBCL vs others) | Portability | Use `uiop:rename-file-overwriting-target` for atomic write-back |
| R5 | Bare-lambda FLET name generation could collide with existing bindings | Scope boundary | Use a deterministic name derived from the operator and lambda-list; document that the name is a suggestion, not guaranteed unique |
| R6 | `read-file-documents-with-yaml-front-matter` may not handle trailing `---` marker cleanly | Library API uncertainty | Write a focused test for the 5-document case before building on it |
| R7 | Adding `:autofix` keyword to `lint-system` changes its signature | Scope boundary | Current signature takes one positional arg; adding `&key autofix` is backwards-compatible |
| R8 | Pretty-printer output at narrow margins may be unreadable | Scope boundary | 20-char margin fixtures are aspirational; accept degraded but valid output at extreme widths |

---

## OSS Components

None — implementing from scratch using CL built-ins (`pprint`, string operations, `uiop:rename-file-overwriting-target`). Reason: the operations are simple, domain-specific, and tightly coupled to Atelier's finding/resolution types.

---

## Phase Scope

### Must deliver (this phase)

- **S1** — Write-back engine: applies resolutions to source files
- **S2** — Line-level automatic maintainers: `fix-trailing-whitespace`, `fix-mixed-indentation`
- **S3** — CST-level automatic maintainers via text-resolution: `fix-earmuffs`, `fix-constant-naming`, `fix-bare-loop-keywords`
- **S4** — Pretty-printer dispatch table for syntax write-back
- **S5** — Bare-lambda automatic maintainer via syntax-resolution: `fix-bare-lambda`
- **S6** — Batch autofix integration: `lint-system` with `:autofix` keyword

### Deferred

Nothing — all stories are delivered in this phase.

---

## File Organisation

### New source files

```
src/
├── pretty-printer.lisp            [new] Pretty-printer dispatch table and entry point
├── write-back.lisp                [new] Write-back engine: resolution-to-span, apply, safe write
├── maintainers/                   [new module]
│   ├── fix-trailing-whitespace.lisp   [new] Line-level maintainer
│   ├── fix-mixed-indentation.lisp     [new] Line-level maintainer
│   ├── fix-earmuffs.lisp              [new] CST text-resolution maintainer
│   ├── fix-constant-naming.lisp       [new] CST text-resolution maintainer
│   ├── fix-bare-loop-keywords.lisp    [new] CST text-resolution maintainer
│   └── fix-bare-lambda.lisp           [new] CST syntax-resolution maintainer
```

### Modified source files

```
src/package.lisp                   [modify] New exports
src/asdf.lisp                      [modify] lint-system gains :autofix keyword
```

### New test files

```
test/
├── pretty-printer.lisp            [new] Fixture-driven pretty-printer tests
├── write-back.lisp                [new] Write-back engine tests
├── fixtures/
│   ├── pretty-print/              [new directory]
│   │   ├── defun-simple.text
│   │   ├── let-binding.text
│   │   ├── flet-single-binding.text
│   │   ├── flet-multi-binding.text
│   │   ├── loop-keyword-style.text
│   │   ├── function-call.text
│   │   └── nested-forms.text
│   └── maintainer/                [new directory]
│       ├── fix-trailing-whitespace.text
│       ├── fix-mixed-indentation.text
│       ├── fix-earmuffs.text
│       ├── fix-constant-naming.text
│       ├── fix-bare-loop-keywords.text
│       └── fix-bare-lambda.text
├── maintainers/                   [new module]
│   ├── fix-trailing-whitespace.lisp
│   ├── fix-mixed-indentation.lisp
│   ├── fix-earmuffs.lisp
│   ├── fix-constant-naming.lisp
│   ├── fix-bare-loop-keywords.lisp
│   └── fix-bare-lambda.lisp
├── autofix.lisp                   [new] Integration tests for lint-system :autofix
```

### Modified test files

```
test/package.lisp             [modify] New imports
test/entrypoint.lisp          [modify] New testcase calls
```

### Modified ASDF definition

```
org.melusina.atelier.asd           [modify] New modules and files in both systems
```

---

## Build System Changes

### `org.melusina.atelier` — updated load order

```lisp
(:module "src"
  :components ((:file "package")
               (:file "utilities")
               (:file "configuration")
               (:file "license")
               (:file "parameter")
               (:file "template")
               (:file "finding")
               (:file "resolution")
               (:file "inspector")
               (:file "maintainer")
               (:file "asdf")
               (:file "runner")
               (:file "pretty-printer")          ; NEW — after runner
               (:file "write-back")              ; NEW — after pretty-printer
               (:module "inspectors"
                :components
                ((:file "check-file-encoding")
                 (:file "check-spdx-license-header")
                 (:file "check-trailing-whitespace")
                 (:file "check-line-length")
                 (:file "check-mixed-indentation")
                 (:file "check-earmuffs")
                 (:file "check-constant-naming")
                 (:file "check-bare-lambda")
                 (:file "check-loop-keywords")))
               (:module "maintainers"            ; NEW — after inspectors
                :components
                ((:file "fix-trailing-whitespace")
                 (:file "fix-mixed-indentation")
                 (:file "fix-earmuffs")
                 (:file "fix-constant-naming")
                 (:file "fix-bare-loop-keywords")
                 (:file "fix-bare-lambda")))
               (:file "main")))
```

### `org.melusina.atelier/test` — updated load order

```lisp
(:module "testsuite"
  :components ((:file "package")
               (:file "utilities")
               (:file "parameter")
               (:file "license")
               (:file "template")
               (:file "finding")
               (:file "resolution")
               (:file "inspector")
               (:file "maintainer")
               (:file "bridge")
               (:file "runner")
               (:file "asdf")
               (:module "inspectors" ...)
               (:file "pretty-printer")          ; NEW
               (:file "write-back")              ; NEW
               (:module "maintainers"            ; NEW
                :components
                ((:file "fix-trailing-whitespace")
                 (:file "fix-mixed-indentation")
                 (:file "fix-earmuffs")
                 (:file "fix-constant-naming")
                 (:file "fix-bare-loop-keywords")
                 (:file "fix-bare-lambda")))
               (:file "autofix")                 ; NEW
               (:module "legacy-inspector" ...)
               (:file "lint")
               (:file "entrypoint")))
```

---

## Package / Module Architecture

### New exports from `#:atelier`

```lisp
;; Pretty-printer
#:*atelier-pprint-dispatch*
#:pretty-print-form

;; Write-back engine
#:apply-resolutions-to-file
#:resolution-text-span

;; Concrete maintainers
#:fix-trailing-whitespace
#:fix-mixed-indentation
#:fix-earmuffs
#:fix-constant-naming
#:fix-bare-loop-keywords
#:fix-bare-lambda
```

No new packages. All new code lives in the `#:atelier` package.

---

## Type / Class Hierarchy

No new classes. All six maintainers are subclasses of `automatic-maintainer`, generated by `define-automatic-maintainer`. All resolution types (`text-resolution`, `syntax-resolution`) already exist.

```
maintainer
└── automatic-maintainer
    ├── fix-trailing-whitespace      [new, singleton]
    ├── fix-mixed-indentation        [new, singleton]
    ├── fix-earmuffs                 [new, singleton]
    ├── fix-constant-naming          [new, singleton]
    ├── fix-bare-loop-keywords       [new, singleton]
    └── fix-bare-lambda              [new, singleton]
```

---

## Protocol Definitions

### `pretty-print-form (form column &key right-margin) → string`

Pretty-print FORM as a string using `*atelier-pprint-dispatch*`. Each continuation line is indented by at least COLUMN spaces. RIGHT-MARGIN controls `*print-right-margin*` (NIL for implementation default). Returns the pretty-printed string including the COLUMN-offset indentation on continuation lines.

### `resolution-text-span (resolution) → (values start-offset end-offset replacement-string)`

Convert a RESOLUTION to a text span suitable for the write-back engine. Dispatches on resolution type:
- `text-resolution` — derives character offsets from the finding's line/column via the file's line vector, returns the replacement slot.
- `syntax-resolution` — derives character offsets from `(cst:source (finding-cst-node finding))`, calls the transform on `(cst:raw (finding-cst-node finding))`, pretty-prints the result at the finding's column, returns the pretty-printed string.

### `apply-resolutions-to-file (pathname resolutions) → pathname`

Apply RESOLUTIONS to the file at PATHNAME. Reads the file into a string, converts each resolution to a text span via `resolution-text-span`, sorts spans by start-offset descending (end-to-start), applies each replacement, writes the result to a temporary file, then renames over PATHNAME. Returns PATHNAME. Signals an error if any spans overlap.

### `lint-system (system-designator &key autofix) → list`

Extended signature. When AUTOFIX is true, after collecting findings, calls `resolve-finding` on each, groups resolutions by file, and calls `apply-resolutions-to-file` for each group. Returns the full findings list (both fixed and unfixed).

---

## Error / Condition Types

No new condition types. Errors during write-back (e.g., I/O failures) propagate as standard CL conditions.

---

## Test Plan

| Story | Test name | Category | Skip condition |
|-------|-----------|:--------:|---------------|
| S1 | `validate-resolution-text-span` | fast | — |
| S1 | `validate-multiple-resolutions-ordering` | fast | — |
| S1 | `validate-multi-line-span-replacement` | fast | — |
| S1 | `validate-text-resolution-write-back` | slow | — |
| S1 | `validate-syntax-resolution-write-back` | slow | — |
| S2 | `validate-fix-trailing-whitespace` | fast | — |
| S2 | `validate-fix-mixed-indentation` | fast | — |
| S2 | `validate-line-maintainers-registered` | fast | — |
| S3 | `validate-fix-earmuffs` | fast | — |
| S3 | `validate-fix-constant-naming` | fast | — |
| S3 | `validate-fix-bare-loop-keywords` | fast | — |
| S3 | `validate-syntax-text-maintainers-registered` | fast | — |
| S4 | `validate-pretty-print-fixtures` | fast | — |
| S5 | `validate-fix-bare-lambda` | fast | — |
| S5 | `validate-fix-bare-lambda-registered` | fast | — |
| S6 | `validate-lint-system-autofix` | slow | — |
| S6 | `validate-lint-system-no-autofix` | slow | — |
| S6 | `validate-lint-system-partial-autofix` | slow | — |

---

## Implementation Order (Step Table)

### Block A — Test fixture infrastructure

| Step | File | Action | Form(s) | Test name | Cat. |
|------|------|--------|---------|-----------|:----:|
| A1 | `test/fixtures/pretty-print/*.text` [new] | Create | 7 YAML fixture files | — | — |
| A2 | `test/fixtures/maintainer/*.text` [new] | Create | 6 YAML fixture files | — | — |

### Block B — Pretty-printer

| Step | File | Action | Form(s) | Test name | Cat. |
|------|------|--------|---------|-----------|:----:|
| B1 | `src/pretty-printer.lisp` [new] | Implement | `*atelier-pprint-dispatch*`, `pprint-loop`, `pretty-print-form` | — | — |
| B2 | `test/pretty-printer.lisp` [new] | Implement | `read-pretty-print-fixture`, `validate-one-pretty-print-fixture`, `validate-pretty-print-fixtures` | `validate-pretty-print-fixtures` | fast |

### Block C — Write-back engine

| Step | File | Action | Form(s) | Test name | Cat. |
|------|------|--------|---------|-----------|:----:|
| C1 | `src/write-back.lisp` [new] | Implement | `line-column-to-offset`, `resolution-text-span`, `apply-resolutions-to-file` | — | — |
| C2 | `test/write-back.lisp` [new] | Implement | `validate-resolution-text-span`, `validate-multiple-resolutions-ordering`, `validate-multi-line-span-replacement`, `validate-text-resolution-write-back`, `validate-syntax-resolution-write-back` | `validate-write-back` | fast+slow |

### Block D — Line-level maintainers

| Step | File | Action | Form(s) | Test name | Cat. |
|------|------|--------|---------|-----------|:----:|
| D1 | `src/maintainers/fix-trailing-whitespace.lisp` [new] | Implement | `define-automatic-maintainer fix-trailing-whitespace` | — | — |
| D2 | `src/maintainers/fix-mixed-indentation.lisp` [new] | Implement | `define-automatic-maintainer fix-mixed-indentation` | — | — |
| D3 | `test/maintainers/fix-trailing-whitespace.lisp` [new] | Implement | `validate-fix-trailing-whitespace` | `validate-fix-trailing-whitespace` | fast |
| D4 | `test/maintainers/fix-mixed-indentation.lisp` [new] | Implement | `validate-fix-mixed-indentation` | `validate-fix-mixed-indentation` | fast |
| D5 | `test/maintainers/fix-trailing-whitespace.lisp` [modify] | Add | `validate-line-maintainers-registered` | `validate-line-maintainers-registered` | fast |

### Block E — CST text-resolution maintainers

| Step | File | Action | Form(s) | Test name | Cat. |
|------|------|--------|---------|-----------|:----:|
| E1 | `src/maintainers/fix-earmuffs.lisp` [new] | Implement | `define-automatic-maintainer fix-earmuffs` | — | — |
| E2 | `src/maintainers/fix-constant-naming.lisp` [new] | Implement | `define-automatic-maintainer fix-constant-naming` | — | — |
| E3 | `src/maintainers/fix-bare-loop-keywords.lisp` [new] | Implement | `define-automatic-maintainer fix-bare-loop-keywords` | — | — |
| E4 | `test/maintainers/fix-earmuffs.lisp` [new] | Implement | `validate-fix-earmuffs` | `validate-fix-earmuffs` | fast |
| E5 | `test/maintainers/fix-constant-naming.lisp` [new] | Implement | `validate-fix-constant-naming` | `validate-fix-constant-naming` | fast |
| E6 | `test/maintainers/fix-bare-loop-keywords.lisp` [new] | Implement | `validate-fix-bare-loop-keywords` | `validate-fix-bare-loop-keywords` | fast |
| E7 | `test/maintainers/fix-earmuffs.lisp` [modify] | Add | `validate-syntax-text-maintainers-registered` | `validate-syntax-text-maintainers-registered` | fast |

### Block F — Syntax-resolution maintainer (bare lambda)

| Step | File | Action | Form(s) | Test name | Cat. |
|------|------|--------|---------|-----------|:----:|
| F1 | `src/maintainers/fix-bare-lambda.lisp` [new] | Implement | `define-automatic-maintainer fix-bare-lambda`, `generate-flet-name` | — | — |
| F2 | `test/maintainers/fix-bare-lambda.lisp` [new] | Implement | `validate-fix-bare-lambda`, `validate-fix-bare-lambda-registered` | `validate-fix-bare-lambda` | fast |

### Block G — Integration

| Step | File | Action | Form(s) | Test name | Cat. |
|------|------|--------|---------|-----------|:----:|
| G1 | `src/asdf.lisp` [modify] | Modify | `lint-system` — add `&key autofix`, autofix loop | — | — |
| G2 | `src/package.lisp` [modify] | Modify | Add new exports | — | — |
| G3 | `org.melusina.atelier.asd` [modify] | Modify | Add new modules and files | — | — |
| G4 | `test/package.lisp` [modify] | Modify | — | — | — |
| G5 | `test/entrypoint.lisp` [modify] | Modify | Add new testcase calls | — | — |
| G6 | `test/autofix.lisp` [new] | Implement | `validate-lint-system-autofix`, `validate-lint-system-no-autofix`, `validate-lint-system-partial-autofix` | `validate-autofix` | slow |

### Execution order

G2, G3, G4, G5 first (structural scaffolding) → A1, A2 (fixtures) → B1, B2 (pretty-printer) → C1, C2 (write-back) → D1–D5 (line maintainers) → E1–E7 (CST text maintainers) → F1, F2 (bare lambda) → G1, G6 (integration).

---

## Test Fixture Design

### Pretty-printer fixtures (`test/fixtures/pretty-print/*.text`)

YAML-separated documents. Front-matter contains `description`, `column`, and `right-margin` (array parsed from single-line square-bracket notation, max 3 entries). Document 0 is input. Documents 1..N are expected outputs, one per `right-margin` entry. A final `---` marker closes the last document.

```
---
description: FLET with single binding
column: 0
right-margin: [nil, 40]
---
(flet ((compute-total (x) (+ x tax))) (mapcar #'compute-total prices))
---
(flet ((compute-total (x) (+ x tax))) (mapcar #'compute-total prices))
---
(flet ((compute-total (x)
         (+ x tax)))
  (mapcar #'compute-total
          prices))
---
```

Test harness: `READ-FROM-STRING` the input, call `pretty-print-form` at each `(column, right-margin)` pair, compare output with `STRING=` against the corresponding expected document.

When `right-margin` is absent, default to `[nil]` (single unlimited-width test).

### Maintainer fixtures (`test/fixtures/maintainer/*.text`)

YAML-separated documents. Front-matter contains `description` and `inspector`. Document 0 is input. Document 1 is expected output. A final `---` marker closes the last document. Comparison is via `EQUAL` on `READ-FROM-STRING` of both documents (AST comparison).

```
---
description: Earmuffs fix adds star convention
inspector: check-earmuffs
---
(defvar wrong-name 42)
---
(defvar *wrong-name* 42)
---
```

Test harness: `READ-FROM-STRING` the input, run the named inspector to get a finding, call `prepare-resolution` with the corresponding maintainer, extract the resolution's replacement or transform result, `READ-FROM-STRING` the expected document, compare with `EQUAL`.

---

## Invariants

Carried forward from prior slices, plus new invariants for this phase.

- **I1** — Every source file begins with `;;;; filename — Description` and ends with `;;;; End of file 'filename'`.
- **I2** — LOOP clause keywords are always keyword symbols.
- **I3** — `MAPCAR`/`MAPCAN`/`REMOVE-IF` receive a named `FLET` function, never a bare `LAMBDA`.
- **I4** — Tests use `define-testcase` from `org.melusina.confidence`.
- **I5** — No SBCL-specific extensions without `#+sbcl` guard.
- **I6** — All `defsystem` forms live in `org.melusina.atelier.asd`.
- **I7** — Never use `SHADOWING-IMPORT`; qualify at the use site or `UNINTERN` first.
- **I8** — `assert-condition` argument order: form first, condition-type second.
- **I9** — `assert-t` checks for exactly `T`; use `(assert-t (not (null X)))` for generalised boolean.
- **I10** — All new exports are grouped by concept in `package.lisp`.
- **I11** — Maintainers are defined with `define-automatic-maintainer`, one per file under `src/maintainers/`.
- **I12** — Pretty-printer fixtures use the multi-margin YAML format with `right-margin` array, max 3 entries, final `---` marker.
- **I13** — Maintainer fixtures use the two-document YAML format (input + expected) with AST comparison via `EQUAL`.
- **I14** — `*atelier-pprint-dispatch*` is never modified after load time. The global dispatch table is never modified.
- **I15** — Write-back uses temp-file + rename for atomicity; source files are never partially written.
- **I16** — `lint-system` without `:autofix` preserves current behaviour — no files modified.
- **I17** — Resolutions are applied end-to-start (descending offset order) to avoid position corruption.

---

## Test Fixtures

No new fixture classes (no stub pattern needed). The maintainers are tested by running the real inspector to produce a finding, then calling `prepare-resolution` on the real maintainer. Fixtures are `.text` files in the YAML format described above.

---

## References to Create

No new reference files needed. The pretty-printer reference already exists at `product/slice/005-autofix-pipeline/references/pretty-printer.md`.

---

## Acceptance Criteria

1. `(atelier:list-maintainers)` returns a list containing all six maintainer symbols: `fix-trailing-whitespace`, `fix-mixed-indentation`, `fix-earmuffs`, `fix-constant-naming`, `fix-bare-loop-keywords`, `fix-bare-lambda`.
2. Given a file with trailing whitespace, `(lint-system :test-system :autofix t)` removes the trailing whitespace and writes the file back.
3. Given a file with `(defvar wrong-name 42)`, the earmuffs maintainer produces a text-resolution with replacement `*wrong-name*`.
4. Given a file with `(mapcar (lambda (x) (1+ x)) items)`, the bare-lambda maintainer produces a syntax-resolution whose transform returns a FLET-wrapped form.
5. `(pretty-print-form '(flet ((f (x) (+ x 1))) (mapcar #'f items)) 0)` returns valid, correctly indented Lisp source as a string.
6. All pretty-printer fixture tests pass (string equality at each margin width).
7. `(lint-system :test-system)` without `:autofix` does not modify any source files.
8. All fast tests pass in under 2 seconds. All slow tests pass in under 10 seconds.
9. `(asdf:test-system "org.melusina.atelier")` passes with zero failures.

---

## Phase Closure Conditions

- All 9 acceptance criteria verified.
- All fast tests passing.
- All slow tests passing.
- `run-all-tests` passes cleanly.
- `implementation-1-notes.md` records results and any deferred items.
