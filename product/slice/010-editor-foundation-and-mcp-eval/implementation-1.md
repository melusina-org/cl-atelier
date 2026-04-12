# Implementation Phase 1 — Slice 010: Editor Foundation

**Slice:** `product/slice/010-editor-foundation-and-mcp-eval/slice.md`
**Phase:** 1 (of 2 planned)
**Scope:** Create `org.melusina.atelier/editor` (package `atelier/editor`) with
the `toplevel-form` record (four slots), `read-toplevel-form-from-string`,
`write-toplevel-form-to-string`, `normalize-toplevel-form`, and
auto-discovered canonicalize fixtures. Add `atelier:lint-string` to core
atelier. No MCP code. No child image. No file I/O to project files.
**Prerequisites:** SBCL with Quicklisp; `org.melusina.atelier` loadable.

## Back-link

Slice: `product/slice/010-editor-foundation-and-mcp-eval/slice.md`

## Prior phases

None. This is phase 1.

## Project Knowledge Applied

| Source | Entry | How it shaped this plan |
|---|---|---|
| `patterns.md` | *Hallucinated requirements from surface intuition* | Every exported symbol traces to an AC. Reviewer audit at phase closure (R1). |
| `patterns.md` | *Skim-then-code does not work for documented CL surprises* | Explicit Maker protocol step: re-read `product/knowledge/` and Eclector references before step 1 (R8). |
| `patterns.md` | *Plan pass-count predictions* | Prediction is a range (50–150), not a tight number (R9). |
| `patterns.md` | *Test-registry pollution across test runs* | `normalize-toplevel-form` calls `lint-string` which walks the inspector registry. Tests do not register new inspectors — no pollution path. |
| `patterns.md` | *Documentation count drift from live registry* | Phase-closure step updates CLAUDE.md with the new system (R10). |
| `invariants.md` | INV-4 (fresh SBCL subprocess) | T7 and T42 automate fresh-subprocess verification. |
| `invariants.md` | INV-9 (Eclector string-input-stream) | `read-toplevel-form-from-string` parses from a string, not a file stream — character-indexed by construction. |
| `invariants.md` | INV-11 (templates are API consumers) | Phase-closure grep of `resource/template/` for `atelier/editor`. Expected zero matches. |
| `invariants.md` | INV-14 (every define-tool traces to an AC) | Phase 1 adds no `define-tool` forms, but the principle extends: every export traces to an AC. |
| `calibration.md` | Counting the wrong unit, testcases × 3–6 | Range 50–150 assertions. Upper end from fixture expansion (12–15 fixtures × 3 assertions each + 25 non-fixture tests). |
| `reworks.md` | Slice 009 rework #8 (assert-t strictness) | Use `assert-t*` for generalised booleans throughout. |

## Risk Register

| # | Risk | Category | Severity | Mitigation |
|---|---|---|:---:|---|
| R1 | Hallucinated requirements in the editor | Architecture | HIGH | Every exported symbol traces to an AC. Reviewer audit at phase closure. |
| R2 | Eclector has no C3 unparse path | Dependency | HIGH | T26 + T27 run first (step 1). If C3 absent, step 10 builds `pretty-print-cst` (C2). Plan accounts for both paths. |
| R3 | Eclector custom client for `#+`/`#-` preservation fails | Dependency | MED-HI | T26 is gating. Fallback: body as s-expression with current-platform `*features*`. Phase 1 still ships; cross-platform preservation deferred. |
| R4 | `lint-string` interacts badly with inspector dynamic bindings | Integration | MEDIUM | `lint-string` binds `*current-pathname*` to `#p"<lint-string>"`. T1–T4 exercise finding construction. |
| R5 | Position-1 name heuristic wrong for exotic macros | Design | LOW | Acceptable. `name` is metadata for ordering, not code generation. Body CST is the truth. |
| R6 | Round-trip fidelity whitespace differences | Correctness | MEDIUM | Contract is fixed-point: `write(read(write(read(s)))) = write(read(s))`. T25 and fixture suite validate. |
| R7 | `unexpected-toplevel-form` too aggressive | Design | MEDIUM | Start strict, widen based on feedback. Condition carries the form; `continue` restart lets callers accept. |
| R8 | Skim-then-code on CL surprises | Process | MEDIUM | Maker re-reads knowledge files + Eclector references before step 1. |
| R9 | Assertion count prediction | Calibration | LOW | Range 50–150, not tight number. |
| R10 | Documentation count drift | Project-Knowledge | LOW | Phase-closure CLAUDE.md update. |
| R11 | Templates grep (INV-11) | Project-Knowledge | LOW | Phase-closure grep. Expected zero. |

## OSS Components

**No new dependencies.** Phase 1's `org.melusina.atelier/editor` depends only on
`org.melusina.atelier` (which already depends on `alexandria`, `cl-ppcre`,
`trivia`, `uiop`, `eclector-concrete-syntax-tree`).

The OSS and prior-art survey is complete (Stage 2 of the planning interview).
Results are written to `references/prior-art.md` in step 2.

**Summary:** No existing Common Lisp library implements a projectional editor
in the sense we need. ProjecturEd is the only true projectional editor for CL
but is experimental, GUI-oriented, and ~20k LoC. trivial-formatter is a
reference for read/pretty-print cycles, not a dependency. `closer-mop` is a
Phase 2 concern. Build from scratch.

## Phase Scope

**Stories delivered in this phase:** S0, S1, S2, S3, S9, S10, S11 from
`slice.md` (renumbered from the Strategist's original numbering to align
with the architectural changes made during the planning interview).

**Stories deferred to Phase 2:** S4, S5, S6, S7, S8 (concrete
`swank-socket-connection`, child-image lifecycle, MCP tools for eval,
introspection, and testsuite running).

**Story-to-deliverable mapping with API corrections from Stage 4:**

| Story | Deliverable | API name (corrected) |
|---|---|---|
| S0 (new) | `lint-string` in core atelier | `atelier:lint-string` |
| S1 | System + package | `org.melusina.atelier/editor`, `#:atelier/editor` |
| S2 | `toplevel-form` record + read/write | `make-toplevel-form`, `read-toplevel-form-from-string`, `write-toplevel-form-to-string`, `toplevel-form-ast` |
| S3 | Normalize pipeline | `normalize-toplevel-form` |
| S9 | Testsuite module | `testsuite/editor/`, `#:atelier/testsuite/editor` |
| S10 | OSS check (done) | `references/prior-art.md` |
| S11 | Fixture directory | `testsuite/fixtures/editor/canonicalize/` |

## File Organisation

```
src/editor/                             [new]
  package.lisp                          — defpackage #:atelier/editor, 15 exports
  conditions.lisp                       — unexpected-toplevel-form condition
  toplevel-form.lisp                    — toplevel-form class (4 slots), make-toplevel-form,
                                          toplevel-form-ast (derived: CST → s-expr with :features)
  eclector-client.lisp                  — custom Eclector client preserving #+/#- as CST nodes
  read-form.lisp                        — read-toplevel-form-from-string: string → toplevel-form
                                          (parse via custom client, peel eval-when, derive kind+name)
  write-form.lisp                       — write-toplevel-form-to-string: toplevel-form → string
                                          (CST-to-text preserving #+/#-, eval-when wrapping,
                                          pretty-printing; C2 or C3 depending on step 1 probe)
  canonicalize.lisp                     — normalize-toplevel-form: toplevel-form →
                                          (values toplevel-form findings)
                                          (composes read → lint-string → write)

src/runner.lisp                         [modify] or src/lint-string.lisp [new]
  lint-string                           — (content &key pathname levels) →
                                          (values new-content findings)

org.melusina.atelier.asd                [modify]
  — add org.melusina.atelier/editor system
  — add testsuite/editor module to org.melusina.atelier/testsuite

testsuite/editor/                       [new]
  package.lisp                          — defpackage #:atelier/testsuite/editor
  utilities.lisp                        — test helpers, with-isolated-registries if needed
  eclector-capability.lisp              — T26, T27: Eclector #+/#- probe
  lint-string.lisp                      — T1–T4: lint-string tests
  toplevel-form.lisp                    — T8–T17: form record tests
  normalize.lisp                        — T18–T20: normalize pipeline tests
  write-form.lisp                       — T21–T25: write + round-trip tests
  fixtures.lisp                         — T28–T40+: auto-discovered fixture runner
  fresh-sbcl-load.lisp                  — T7: editor-loads-without-MCP subprocess test
  entrypoint.lisp                       — test entry point, wired into run-all-tests

testsuite/fixtures/editor/canonicalize/ [new]
  defun-simple.text
  defclass-simple.text
  defgeneric-simple.text
  defmethod-simple.text
  defmacro-simple.text
  defvar-earmuffs.text
  defparameter-simple.text
  defconstant-simple.text
  deftype-simple.text
  define-condition-simple.text
  eval-when-non-default.text
  feature-inside-body.text
  third-party-macro.text
  docstring-preserved.text
  unexpected-progn.text
```

## Build System Changes

### `org.melusina.atelier.asd` additions

```lisp
(asdf:defsystem #:org.melusina.atelier/editor
  :description "Projectional editor for managed Common Lisp files."
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:org.melusina.atelier)
  :components
  ((:module "src/editor"
    :serial t
    :components ((:file "package")
                 (:file "conditions")
                 (:file "toplevel-form")
                 (:file "eclector-client")
                 (:file "read-form")
                 (:file "write-form")
                 (:file "canonicalize")))))
```

Testsuite module added to `org.melusina.atelier/testsuite`:

```lisp
;; Inside the existing :components of the testsuite module:
(:module "editor"
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "eclector-capability")
               (:file "lint-string")
               (:file "toplevel-form")
               (:file "normalize")
               (:file "write-form")
               (:file "fixtures")
               (:file "fresh-sbcl-load")
               (:file "entrypoint")))
```

Testsuite `:depends-on` gains `#:org.melusina.atelier/editor`.

## Package / Module Architecture

### `#:atelier/editor` exports (15 symbols)

```
;; Class + constructor + readers
toplevel-form
make-toplevel-form
toplevel-form-kind
toplevel-form-name
toplevel-form-body
toplevel-form-eval-when
toplevel-form-ast               ; derived: CST → s-expression, :features kwarg

;; Read / Write
read-toplevel-form-from-string
write-toplevel-form-to-string

;; Pipeline
normalize-toplevel-form         ; → (values toplevel-form findings)

;; Conditions
unexpected-toplevel-form

;; Condition accessors (for unexpected-toplevel-form)
unexpected-toplevel-form-source ; the offending form or string
unexpected-toplevel-form-reason ; keyword: :progn, :side-effect, :in-package, :reader-macro
```

That is 14 named exports. The 15th is the restart name used by
`unexpected-toplevel-form`:

```
decompose                       ; restart name for PROGN decomposition
```

Wait — the examples used `continue` (CL standard restart). Revised per the
planning interview: use a **custom restart `decompose`** rather than overloading
CL's `continue`, because `continue` has standard semantics ("continue from this
condition") that don't match "decompose this PROGN into individual forms."
The condition offers `decompose` when the form is a PROGN, and no restart
when the form is a side-effectful expression (the caller must handle the
condition or let it propagate).

### `#:atelier` new export (1 symbol)

```
lint-string                     ; (content &key pathname levels) → (values string findings)
```

### `#:atelier/testsuite/editor` (internal)

Test package. No public exports beyond what `run-all-tests` dispatches to.

## Type / Class Hierarchy

```
toplevel-form                   ; 4 slots: kind, name, body, eval-when
                                ; No subclasses in Phase 1.

unexpected-toplevel-form        ; condition, subclass of SIMPLE-ERROR
  source                        ; the offending form (string or s-expression)
  reason                        ; keyword: :progn :side-effect :in-package :reader-macro
```

No inheritance beyond standard CL condition hierarchy. No abstract classes.

## Protocol Definitions

### `atelier:lint-string` (new in core)

```lisp
(defun lint-string (content &key (pathname #p"<lint-string>")
                                 (levels '(:line :syntax)))
  "Run the Atelier lint pipeline on CONTENT (a string) at the
requested inspector LEVELS and return (VALUES fixed-content findings).
LEVELS is a list of inspector level keywords; :FILE is excluded by
default because file-level inspectors require header/footer context.
PATHNAME is bound to *CURRENT-PATHNAME* for finding construction."
  ...)
```

### `atelier/editor:make-toplevel-form`

```lisp
(defun make-toplevel-form (&key kind name body
                                (eval-when '(:load-toplevel :execute)))
  "Construct a TOPLEVEL-FORM. KIND is the operator symbol (e.g.
COMMON-LISP:DEFUN). NAME is the defined symbol or NIL. BODY is
an Eclector CST preserving reader conditionals. EVAL-WHEN is the
situation list, default (:LOAD-TOPLEVEL :EXECUTE)."
  ...)
```

### `atelier/editor:toplevel-form-ast`

```lisp
(defgeneric toplevel-form-ast (form &key features)
  (:documentation "Return the form body as a plain s-expression.
Reader conditionals are evaluated against FEATURES (default:
CL:*FEATURES*). The result is lossy: cross-platform branches
for non-matching features are dropped."))
```

### `atelier/editor:read-toplevel-form-from-string`

```lisp
(defun read-toplevel-form-from-string (string)
  "Parse STRING as a single toplevel Common Lisp form and return
a TOPLEVEL-FORM record. EVAL-WHEN wrappers are peeled into the
EVAL-WHEN slot. Reader conditionals inside the body are preserved
as Eclector CST nodes. NAME is derived from position 1 of the form.
Signals UNEXPECTED-TOPLEVEL-FORM for PROGN (with DECOMPOSE restart),
IN-PACKAGE, side-effectful forms, and #. reader macros."
  ...)
```

### `atelier/editor:write-toplevel-form-to-string`

```lisp
(defun write-toplevel-form-to-string (form)
  "Emit FORM as a canonical string. The body CST is pretty-printed
preserving reader conditionals. If EVAL-WHEN is non-default, the
form is wrapped in (EVAL-WHEN (...) ...). The result is a fixed
point: (WRITE (READ (WRITE (READ s)))) = (WRITE (READ s))."
  ...)
```

### `atelier/editor:normalize-toplevel-form`

```lisp
(defun normalize-toplevel-form (form)
  "Run the Atelier lint + maintainer pipeline on FORM and return
(VALUES normalized-form findings). NORMALIZED-FORM is a new
TOPLEVEL-FORM with maintainers applied (earmuffs, loop keywords,
bare-lambda, etc). FINDINGS is a list of FINDING instances
produced during linting. The operation is idempotent:
(NORMALIZE (NORMALIZE f)) yields the same text as (NORMALIZE f)."
  ...)
```

## Error / Condition Types

| Name | Superclass | Slots | When signalled |
|---|---|---|---|
| `unexpected-toplevel-form` | `simple-error` | `source` (form or string), `reason` (keyword) | `read-toplevel-form-from-string` encounters a PROGN, IN-PACKAGE, side-effectful form, or `#.` reader macro at toplevel |

**Restart protocol:**

| Condition reason | Restart available | Restart behaviour |
|---|---|---|
| `:progn` | `decompose` | Decomposes the PROGN into individual `toplevel-form` records; returns a list |
| `:side-effect` | none | Must be handled or propagated |
| `:in-package` | none | Must be handled or propagated |
| `:reader-macro` | none | Must be handled or propagated |

## Test Plan

| Story | Test name | Category | Skip condition |
|---|---|---|---|
| S0 | `validate-lint-string-earmuffs` (T1) | fast | — |
| S0 | `validate-lint-string-clean-form` (T2) | fast | — |
| S0 | `validate-lint-string-syntax-only` (T3) | fast | — |
| S0 | `validate-lint-string-line-and-syntax` (T4) | fast | — |
| S1 | `validate-editor-package-exists` (T5) | fast | — |
| S1 | `validate-editor-exports` (T6) | fast | — |
| S1 | `validate-editor-loads-without-mcp` (T7) | slow | `#-sbcl` |
| S2 | `validate-make-toplevel-form` (T8) | fast | — |
| S2 | `validate-read-simple-defun` (T9) | fast | — |
| S2 | `validate-read-peels-eval-when` (T10) | fast | — |
| S2 | `validate-read-preserves-feature-conditional` (T11) | fast | — |
| S2 | `validate-ast-default-features` (T12) | fast | — |
| S2 | `validate-ast-overridden-features` (T13) | fast | — |
| S2 | `validate-read-unknown-macro-name` (T14) | fast | — |
| S2 | `validate-read-progn-signals` (T15) | fast | — |
| S2 | `validate-read-progn-decompose-restart` (T16) | fast | — |
| S2 | `validate-read-side-effect-signals` (T17) | fast | — |
| S3 | `validate-normalize-earmuffs` (T18) | fast | — |
| S3 | `validate-normalize-returns-findings` (T19) | fast | — |
| S3 | `validate-normalize-idempotent` (T20) | fast | — |
| write | `validate-write-simple-defun` (T21) | fast | — |
| write | `validate-write-preserves-feature-conditional` (T22) | fast | — |
| write | `validate-write-wraps-eval-when` (T23) | fast | — |
| write | `validate-write-elides-default-eval-when` (T24) | fast | — |
| write | `validate-round-trip-fixed-point` (T25) | fast | — |
| probe | `validate-eclector-preserves-feature-expression` (T26) | fast | — |
| probe | `validate-eclector-cst-to-text-round-trip` (T27) | fast | — |
| S11 | `validate-canonicalize-fixtures` (T28–T40+) | fast | — |
| S9 | `validate-editor-testsuite-package` (T41) | fast | — |
| S9 | `validate-full-regression-fresh-sbcl` (T42) | slow | `#-sbcl` |

**Assertion count estimate:** 50–150 new assertions (range, per calibration).

## Implementation Order (Step Table)

| Step | File | Action | Form(s) | Test name(s) | Category |
|---|---|---|---|---|:---:|
| 0 | _Maker protocol_ | Re-read `product/knowledge/` files, Eclector documentation, Common Lisp skill references for Eclector and Confidence | — | — | — |
| 1 | `testsuite/editor/eclector-capability.lisp` [new] | Eclector capability probe | `validate-eclector-preserves-feature-expression`, `validate-eclector-cst-to-text-round-trip` | T26, T27 | fast |
| 2 | `product/slice/010-.../references/prior-art.md` [new] | Write OSS survey results | — | — | — |
| 3 | `product/slice/010-.../references/form-record-decisions.md` [new] | Freeze: 4-slot class, naming conventions, Eclector C2/C3 decision from step 1 | — | — | — |
| 4 | `src/runner.lisp` [modify] or `src/lint-string.lisp` [new] | Implement `lint-string` | `lint-string` | — | — |
| 5 | `testsuite/editor/lint-string.lisp` [new] | Test `lint-string` | `validate-lint-string-earmuffs`, `validate-lint-string-clean-form`, `validate-lint-string-syntax-only`, `validate-lint-string-line-and-syntax` | T1, T2, T3, T4 | fast |
| 6 | `src/package.lisp` [modify] | Export `lint-string` from `#:atelier` | — | — | — |
| 7 | `src/editor/package.lisp` [new] | Define `#:atelier/editor` with 15 exports | — | — | — |
| 8 | `src/editor/conditions.lisp` [new] | Define `unexpected-toplevel-form` condition, `decompose` restart | `unexpected-toplevel-form`, slots, restart | — | — |
| 9 | `src/editor/toplevel-form.lisp` [new] | Define `toplevel-form` class (4 slots), `make-toplevel-form`, `toplevel-form-ast` | `toplevel-form`, `make-toplevel-form`, `toplevel-form-ast` | — | — |
| 10 | `src/editor/eclector-client.lisp` [new] | Custom Eclector client preserving `#+`/`#-`; thin wrapper if C3 exists, full client if C3 absent (decision from step 1) | client class or configuration | — | — |
| 11 | `src/editor/read-form.lisp` [new] | Implement `read-toplevel-form-from-string` | `read-toplevel-form-from-string` | — | — |
| 12 | `src/editor/write-form.lisp` [new] | Implement `write-toplevel-form-to-string` (C2 or C3 path from step 1) | `write-toplevel-form-to-string` | — | — |
| 13 | `src/editor/canonicalize.lisp` [new] | Implement `normalize-toplevel-form` | `normalize-toplevel-form` | — | — |
| 14 | `org.melusina.atelier.asd` [modify] | Add `org.melusina.atelier/editor` system definition | `defsystem` | — | — |
| 15 | `org.melusina.atelier.asd` [modify] | Add `testsuite/editor` module to `org.melusina.atelier/testsuite`, add `/editor` to testsuite `:depends-on` | module entry | — | — |
| 16 | `testsuite/editor/package.lisp` [new] | Define `#:atelier/testsuite/editor` | `defpackage` | — | — |
| 17 | `testsuite/editor/utilities.lisp` [new] | Test helpers (if needed) | — | — | — |
| 18 | `testsuite/editor/toplevel-form.lisp` [new] | Test `make-toplevel-form` | `validate-make-toplevel-form` | T8 | fast |
| 19 | `testsuite/editor/toplevel-form.lisp` [modify] | Test `read-toplevel-form-from-string` — simple defun | `validate-read-simple-defun` | T9 | fast |
| 20 | `testsuite/editor/toplevel-form.lisp` [modify] | Test eval-when peeling | `validate-read-peels-eval-when` | T10 | fast |
| 21 | `testsuite/editor/toplevel-form.lisp` [modify] | Test `#+` preservation inside body | `validate-read-preserves-feature-conditional` | T11 | fast |
| 22 | `testsuite/editor/toplevel-form.lisp` [modify] | Test `toplevel-form-ast` with default features | `validate-ast-default-features` | T12 | fast |
| 23 | `testsuite/editor/toplevel-form.lisp` [modify] | Test `toplevel-form-ast` with overridden features | `validate-ast-overridden-features` | T13 | fast |
| 24 | `testsuite/editor/toplevel-form.lisp` [modify] | Test unknown macro name derivation | `validate-read-unknown-macro-name` | T14 | fast |
| 25 | `testsuite/editor/toplevel-form.lisp` [modify] | Test PROGN signals `unexpected-toplevel-form` | `validate-read-progn-signals` | T15 | fast |
| 26 | `testsuite/editor/toplevel-form.lisp` [modify] | Test `decompose` restart on PROGN | `validate-read-progn-decompose-restart` | T16 | fast |
| 27 | `testsuite/editor/toplevel-form.lisp` [modify] | Test side-effectful form signals | `validate-read-side-effect-signals` | T17 | fast |
| 28 | `testsuite/editor/write-form.lisp` [new] | Test write simple defun | `validate-write-simple-defun` | T21 | fast |
| 29 | `testsuite/editor/write-form.lisp` [modify] | Test write preserves `#+` | `validate-write-preserves-feature-conditional` | T22 | fast |
| 30 | `testsuite/editor/write-form.lisp` [modify] | Test write wraps non-default eval-when | `validate-write-wraps-eval-when` | T23 | fast |
| 31 | `testsuite/editor/write-form.lisp` [modify] | Test write elides default eval-when | `validate-write-elides-default-eval-when` | T24 | fast |
| 32 | `testsuite/editor/write-form.lisp` [modify] | Test round-trip fixed point | `validate-round-trip-fixed-point` | T25 | fast |
| 33 | `testsuite/editor/normalize.lisp` [new] | Test normalize applies earmuffs | `validate-normalize-earmuffs` | T18 | fast |
| 34 | `testsuite/editor/normalize.lisp` [modify] | Test normalize returns findings | `validate-normalize-returns-findings` | T19 | fast |
| 35 | `testsuite/editor/normalize.lisp` [modify] | Test normalize idempotency | `validate-normalize-idempotent` | T20 | fast |
| 36 | `testsuite/fixtures/editor/canonicalize/*.text` [new] | Create initial fixture set (12–15 files) | — | — | — |
| 37 | `testsuite/editor/fixtures.lisp` [new] | Auto-discovery runner for canonicalize fixtures | `validate-canonicalize-fixtures` | T28–T40+ | fast |
| 38 | `testsuite/editor/package.lisp` [modify] | Verify package exists | `validate-editor-testsuite-package` | T41 | fast |
| 39 | `testsuite/editor/fresh-sbcl-load.lisp` [new] | Editor-loads-without-MCP subprocess test | `validate-editor-loads-without-mcp` | T7 | slow |
| 40 | `testsuite/editor/entrypoint.lisp` [new] | Wire into `run-all-tests` | — | — | — |
| 41 | `testsuite/entrypoint.lisp` [modify] | Call editor testsuite from combined entry point | — | — | — |
| 42 | _Full regression_ | Run `(asdf:test-system "org.melusina.atelier")` in fresh SBCL subprocess | `validate-full-regression-fresh-sbcl` | T42 | slow |
| 43 | `CLAUDE.md` [modify] | Add editor section: system name, package, entry point `normalize-toplevel-form`, 4-slot class shape | — | — | — |
| 44 | _Phase-closure checks_ | INV-11 grep (`resource/template/` for `atelier/editor`); export audit (every symbol traces to an AC); assertion count within 50–150 range | — | — | — |

## Invariants

**Carried forward (all confirmed):** INV-1 through INV-16.

**New for Phase 1:**

- **INV-17:** `org.melusina.atelier/editor` loads in a fresh SBCL without pulling in `com.inuoe.jzon`, `bordeaux-threads`, `closer-mop`, or the `atelier/mcp` package. Enforced by T7.
- **INV-18:** The `toplevel-form` body is an Eclector CST preserving reader conditionals (`#+`/`#-`) as structure. Converting to s-expression is a lossy operation available via `toplevel-form-ast`.
- **INV-19:** `normalize-toplevel-form` is idempotent: applying it twice yields the same text as applying it once. Enforced by T20 and every fixture in S11.
- **INV-20:** The round-trip `write(read(write(read(s)))) = write(read(s))` holds for any string `s` that `read-toplevel-form-from-string` accepts. Enforced by T25 and the fixture suite.

## Test Fixtures

### Fixture format for `testsuite/fixtures/editor/canonicalize/`

Each `.text` file has YAML front matter and two `---`-separated documents:
input source and expected normalized output.

```yaml
---
description: "DEFVAR with missing earmuffs"
kind: defvar
---
(defvar foo 42)
---
(defvar *foo* 42)
```

The auto-discovery runner:
1. Parses front-matter for the description (used in test output).
2. Reads document 1 as the input string.
3. Reads document 2 as the expected normalized string.
4. Calls `(read-toplevel-form-from-string input)` → form.
5. Calls `(normalize-toplevel-form form)` → normalized.
6. Calls `(write-toplevel-form-to-string normalized)` → actual.
7. Asserts `actual` = expected (byte-for-byte).
8. Asserts idempotency: `(write (normalize (read actual)))` = actual.
9. Asserts round-trip: `(write (read actual))` = actual.

Each fixture produces **3 assertions**: normalized match, idempotency,
round-trip.

### Initial fixture set (step 36)

| File | Tests |
|---|---|
| `defun-simple.text` | Simple DEFUN, canonical indentation |
| `defclass-simple.text` | DEFCLASS with slots |
| `defgeneric-simple.text` | DEFGENERIC with lambda-list |
| `defmethod-simple.text` | DEFMETHOD with specializers |
| `defmacro-simple.text` | DEFMACRO |
| `defvar-earmuffs.text` | DEFVAR, fix-earmuffs applied |
| `defparameter-simple.text` | DEFPARAMETER |
| `defconstant-simple.text` | DEFCONSTANT |
| `deftype-simple.text` | DEFTYPE |
| `define-condition-simple.text` | DEFINE-CONDITION |
| `eval-when-non-default.text` | Non-default eval-when wrapper |
| `feature-inside-body.text` | `#+sbcl` inside body preserved |
| `third-party-macro.text` | `confidence:define-testcase`, name from position 1 |
| `docstring-preserved.text` | DEFUN with docstring, preserved in body |

14 fixtures × 3 assertions = 42 fixture assertions.

## References to Create

| Step | File | Contents |
|---|---|---|
| 2 | `references/prior-art.md` | OSS survey: ProjecturEd, trivial-formatter, Eclector, closer-mop. Verdict: build from scratch. |
| 3 | `references/form-record-decisions.md` | Frozen: 4-slot `toplevel-form` (kind, name, body, eval-when). Body is Eclector CST. No docstring slot. No source-position. No feature-expression slot. Name from position 1. Eclector C2/C3 decision. |

## Acceptance Criteria

| # | Criterion | Story | Verified by |
|---|---|---|---|
| AC1 | `(asdf:load-system "org.melusina.atelier/editor")` in fresh SBCL: zero warnings | S1 | T7 (subprocess) |
| AC2 | `/editor` loads without MCP deps (`com.inuoe.jzon`, `bordeaux-threads`, `atelier/mcp` absent) | S1 | T7 |
| AC3 | 15 symbols exported from `#:atelier/editor` | S1 | T6 |
| AC4 | `make-toplevel-form` constructs a record with correct slot values | S2 | T8 |
| AC5 | `read-toplevel-form-from-string` peels eval-when, derives kind + name | S2 | T9, T10, T14 |
| AC6 | Eclector CST body preserves `#+`/`#-` inside forms | S2 | T11, T26 |
| AC7 | `toplevel-form-ast` evaluates reader conditionals per `:features` | S2 | T12, T13 |
| AC8 | PROGN signals `unexpected-toplevel-form` with `decompose` restart | S2 | T15, T16 |
| AC9 | Side-effectful forms signal `unexpected-toplevel-form`, no restart | S2 | T17 |
| AC10 | `write-toplevel-form-to-string` preserves `#+`/`#-`, wraps eval-when correctly | write | T22, T23, T24 |
| AC11 | Round-trip `write(read(write(read(s)))) = write(read(s))` | write | T25, fixtures |
| AC12 | `normalize-toplevel-form` applies `fix-earmuffs` on DEFVAR | S3 | T18 |
| AC13 | `normalize-toplevel-form` returns findings as second value | S3 | T19 |
| AC14 | `normalize-toplevel-form` is idempotent | S3 | T20, fixtures |
| AC15 | `atelier:lint-string` runs syntax-level inspectors on a string | S0 | T1, T3 |
| AC16 | `testsuite/editor/` module runs via `(asdf:test-system "org.melusina.atelier")` | S9 | T41, T42 |
| AC17 | Full regression (base atelier + MCP + editor) passes in fresh SBCL subprocess | S9 | T42 |
| AC18 | Fixture auto-discovery produces at least 14 × 3 = 42 assertions | S11 | T28–T40+ |

18 acceptance criteria. Each maps to at least one test.

## Phase Closure Conditions

- [ ] All fast tests passing.
- [ ] Both slow tests passing (T7, T42).
- [ ] All 18 acceptance criteria met.
- [ ] `CLAUDE.md` updated with editor section.
- [ ] `resource/template/` grep for `atelier/editor` returns zero matches.
- [ ] Every exported symbol from `#:atelier/editor` traces to an AC (Reviewer audit).
- [ ] No `src/editor/*.lisp` file references `atelier/mcp` or `org.melusina.atelier/mcp`.
- [ ] Assertion count within 50–150 range (documented in notes if outside).
- [ ] `references/prior-art.md` and `references/form-record-decisions.md` committed.
- [ ] Eclector C2/C3 decision recorded in `form-record-decisions.md`.
- [ ] Any plan deviations documented via the append-not-rewrite amendment protocol.
