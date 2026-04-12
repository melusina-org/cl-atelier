# Form Record Decisions — Frozen for Phase 1

**Recorded:** 2026-04-12
**Status:** Frozen. Changes require a plan amendment.

## Class: `toplevel-form`

CLOS class with five slots:

| Slot | Type | Default | Description |
|---|---|---|---|
| `kind` | `symbol` | (required) | Operator symbol from the form's CAR. E.g., `common-lisp:defun`, `confidence:define-testcase`. Third-party macros are first-class. |
| `name` | `(or symbol null)` | `nil` | Defined symbol, derived from position 1 (second element). NIL for expression forms. |
| `body` | Eclector CST | (required) | Complete form as Eclector CST with `#+`/`#-` preserved as `skipped-cst`/`annotated-cons-cst` nodes. Docstring remains in the body. |
| `eval-when` | `list` | `'(:load-toplevel :execute)` | Situation list. Default is elided on write. |
| `source-text` | `(or string null)` | `nil` | Original input string from which this form was parsed. Set by `read-toplevel-form-from-string`. NIL for forms made via `make-toplevel-form`. Used by the C2 write path for verbatim copy of non-matching `#+`/`#-` branches. |

## Slots deliberately omitted

- **`docstring`** — remains in body CST. Kind-specific extraction breaks for unknown macros.
- **`feature-expression`** — whole-form `#+` guards handled at ASDF `:components` level.
- **`source-position`** — editor owns ordering via topological sort + alphabetical tie-break.
- **`depends-on`** — computed on demand from body, never stored.

## Naming conventions

| Entity | Name | CL convention |
|---|---|---|
| Class | `toplevel-form` | — |
| Constructor | `make-toplevel-form` | `make-*` prefix |
| Reader: string → form | `read-toplevel-form-from-string` | `read-*-from-string` |
| Writer: form → string | `write-toplevel-form-to-string` | `write-*-to-string` |
| Normalizer | `normalize-toplevel-form` | verb-noun |
| Derived accessor | `toplevel-form-ast` | class-slot pattern, `:features` kwarg |
| Condition | `unexpected-toplevel-form` | descriptive noun phrase |
| Restart | `decompose` | verb (for PROGN decomposition) |

## `#+`/`#-` preservation strategy

**Read path:** Custom Eclector client (`preserving-cst-client`) with:
- `make-skipped-input-result` → returns `skipped-cst` (subclass of `cst:atom-cst`)
  with `reason` slot `(cons context feature-expression)` and `source` range.
- `make-expression-result :around` → filters `skipped-cst` from children before
  `call-next-method`; annotates result via `change-class` to `annotated-cons-cst`
  carrying the skipped nodes.

**Write path (C2):** Walk the CST. For normal nodes: pretty-print via
`*atelier-pprint-dispatch*`. For `annotated-cons-cst` nodes: pretty-print
matching children, copy non-matching `skipped-cst` regions verbatim from
`source-text` using `(subseq source-text start end)`.

**C3 does not exist.** Eclector has no general CST-to-text unparse. The
`concrete-syntax-tree` package exports `unparse-lambda-list` etc. for
lambda-lists only.

## `toplevel-form-ast` derived accessor

```lisp
(toplevel-form-ast form &key (features cl:*features*))
```

Returns the body as a plain s-expression with reader conditionals evaluated
against FEATURES. Lossy: non-matching `#+`/`#-` branches are dropped.
The canonical representation is the Eclector CST in the `body` slot.

## `kind` derivation

The `kind` is `(car (cst:raw body))` — the operator symbol. No keyword
translation. Unknown third-party macros produce their own symbol as `kind`.

## `name` derivation

Position 1 (second element) of the form's raw s-expression. Works for all
known `def*`/`define-*` macros and most third-party macros following the CL
convention. Returns NIL for expression forms (no `cadr`).

## Unexpected toplevel forms

`read-toplevel-form-from-string` signals `unexpected-toplevel-form` for:

| Form | Reason keyword | Restart |
|---|---|---|
| `(progn ...)` | `:progn` | `decompose` — splits into individual `toplevel-form` records |
| `(format t ...)`, `(setf ...)`, etc. | `:side-effect` | none |
| `(in-package ...)` | `:in-package` | none |
| `#.(...)` | `:reader-macro` | none |

Allow-list for "expected" operators: any `def*` or `define-*` symbol (by
naming convention), plus `declaim`, `proclaim`, `eval-when`, `export`,
`defsetf`, `define-setf-expander`, `define-compiler-macro`,
`define-method-combination`, `define-modify-macro`, `define-symbol-macro`.
Everything else signals. The `continue` restart (where provided under the
name `decompose`) lets callers accept the form anyway.

## Round-trip contract

`write(read(write(read(s)))) = write(read(s))` for any string `s` accepted
by `read-toplevel-form-from-string`. Matching `#+`/`#-` branches are
pretty-printed (canonical). Non-matching branches are copied verbatim from
source text (preserved). The contract holds because pretty-printing is
idempotent and verbatim copy is identity.
