# Implementation Phase 1: Slice 010 — Pretty-Printer / Emacs Compatibility

**Phase:** 1
**Slice:** product/slice/010-pretty-printer-emacs-compatibility/slice.md
**Scope:** S1 (reference fixture), S2 (reformat-file-with-emacs), S3 (fixed-point test), plus `reformat-file` and `reformat-system` as production entry points.
**Prerequisites:** None.

---

## Back-link

Slice: product/slice/010-pretty-printer-emacs-compatibility/slice.md

## Prior phases

None — this is phase 1.

## Project Knowledge Applied

- **INV-3:** The pretty-printer is the single authority on canonical Lisp text. `reformat-file` implements this authority as a user-facing function.
- **INV-8:** Write-back to source files is atomic via tmpize + rename-overwriting. `reformat-file` must use this pattern.
- **INV-9:** Eclector CST parsing must read the file as a string first (byte vs. character offset issue).
- **Rework (slice 005):** When the pretty-printer is in the loop, it is the only valid source of expected output. The fixture must be derived from the pretty-printer, not hand-written.

## Risk Register

| # | Risk | Category | Mitigation |
|---|------|----------|-----------|
| R1 | SBCL default pprint diverges from expected output on forms not covered by custom dispatch | Library API | Fixture is derived by running `reformat-file` — we do not hand-write expected output (per slice 005 rework lesson) |
| R2 | `read > write` round-trip loses comments inside the fixture | Scope boundary | Reference fixture uses no inline comments; only file header/footer (handled by convention, not the pretty-printer). Forms use docstrings for commentary. |
| R3 | Emacs not available in CI | Test dependency | Emacs test is slow category, skipped when `emacs` not on PATH |
| R4 | Reader features (`#+`/`#-`) complicate toplevel form splitting | State/lifecycle | Use standard `read` for the reference fixture (no feature conditionals that would be skipped). Feature-conditional forms use both branches present syntax. |
| R5 | Form separator heuristic (blank lines between forms) doesn't match existing file conventions | Scope boundary | `reformat-file` preserves inter-form separation: two blank lines between toplevel forms, matching project convention |

## OSS Components

None — Emacs is used as an external tool, not a library dependency.

## Phase Scope

**Stories covered:** S1, S2, S3.
**Stories deferred to phase 2:** S4 (research), S5 (convergence), S6 (documentation).

## File Organisation

```
src/
  pretty-printer.lisp          [modify] — add reformat-file, reformat-system
  package.lisp                 [modify] — export reformat-file, reformat-system
  asdf.lisp                    [read only] — reuse collect-all-source-files, collect-system-source-files
test/
  pretty-printer.lisp          [modify] — add reformat-file-with-emacs, *emacs-pretty-printer-configuration*,
                                           validate-reference-fixture-fixed-point,
                                           validate-reference-fixture-emacs-fixed-point
  entry-point.lisp             [modify] — wire new testcases into run-all-tests
  fixtures/
    pretty-print/
      reference.lisp           [new] — comprehensive reference fixture
```

## Build System Changes

No ASDF changes — all modified files are already in the system definitions.

## Package / Module Architecture

New exports in `atelier` package:
- `reformat-file` — reformat a single `.lisp` file using the pretty-printer
- `reformat-system` — reformat all source files in an ASDF system

No new packages.

## Protocol Definitions

```lisp
(defun reformat-file (pathname)
  "Reformat PATHNAME using Atelier's pretty-printer.
Reads all toplevel forms, pretty-prints each via PRETTY-PRINT-FORM,
and writes the result back atomically. Preserves the file header
(all lines before the first toplevel form) and footer. Emits two
blank lines between toplevel forms.
Does not run the linter — purely read > pretty-print > write.")

(defun reformat-system (system-designator &key (sibling-systems t))
  "Reformat all Common Lisp source files in SYSTEM-DESIGNATOR.
When SIBLING-SYSTEMS is true (the default), includes all systems
defined in the same .asd file. Calls REFORMAT-FILE on each
CL source file. Does not reformat the .asd file itself.")
```

## Error / Condition Types

None. `reformat-file` signals standard I/O conditions on file errors. No new condition classes needed.

## Test Plan

| Story | Test name | Category | Skip condition |
|-------|-----------|:--------:|----------------|
| S3 | `validate-reference-fixture-fixed-point` | fast | — |
| S2 | `validate-reference-fixture-emacs-fixed-point` | slow | `emacs` not on PATH |

## Implementation Order (Step Table)

| Step | File | Action | Form(s) | Test name | Category |
|------|------|--------|---------|-----------|:--------:|
| 1 | `test/fixtures/pretty-print/reference.lisp` [new] | Create reference fixture | — | — | — |
| 2 | `src/package.lisp` [modify] | Export new symbols | `reformat-file`, `reformat-system` | — | — |
| 3 | `src/pretty-printer.lisp` [modify] | Implement | `reformat-file` | `validate-reference-fixture-fixed-point` | fast |
| 4 | `src/pretty-printer.lisp` [modify] | Implement | `reformat-system` | — | — |
| 5 | `test/pretty-printer.lisp` [modify] | Implement | `*emacs-pretty-printer-configuration*` | — | — |
| 6 | `test/pretty-printer.lisp` [modify] | Implement | `reformat-file-with-emacs` | — | — |
| 7 | `test/pretty-printer.lisp` [modify] | Implement | `validate-reference-fixture-fixed-point` | `validate-reference-fixture-fixed-point` | fast |
| 8 | `test/pretty-printer.lisp` [modify] | Implement | `validate-reference-fixture-emacs-fixed-point` | `validate-reference-fixture-emacs-fixed-point` | slow |
| 9 | `test/entry-point.lisp` [modify] | Wire testcases | — | — | — |

### Step Details

**Step 1: Create reference fixture**

Create `test/fixtures/pretty-print/reference.lisp` as a comprehensive Common Lisp file. It must cover at least 15 distinct form types:

- `in-package`
- `defpackage` with `:use`, `:import-from`, `:export`
- `defvar` / `defparameter` with complex property-list initialisers
- `defconstant`
- `deftype`
- `defstruct` with typed slots and options
- `defclass` with complex slot specs, `:metaclass`, `:documentation`
- `defgeneric` with `:method-combination`, `:documentation`
- `defmethod` with specialisers
- `defun` with simple body
- `defun` with long keyword lambda-list and multi-line `declare`
- `defmacro` with `&body`
- `let` / `let*` with multiple bindings
- `flet` / `labels` with long function names
- `loop` with keyword-style clauses
- `cond` / `case` / `ecase` / `typecase`
- `when` / `unless`
- `lambda`
- `eval-when`
- `handler-case` / `handler-bind` / `restart-case`
- Multi-line strings (docstrings, `format` control strings)
- `concatenate`, `coerce`
- `declare` forms spanning multiple lines
- Forms guarded by `#+` / `#-` features

The file follows project conventions: header block, `(in-package ...)`, forms separated by two blank lines, footer line.

**Important:** The fixture will be initially written, then `reformat-file` will be run on it to derive the canonical version. The committed fixture is the output of `reformat-file`, not the hand-written draft. This follows the slice 005 rework lesson.

**Step 2: Export new symbols**

Add `#:reformat-file` and `#:reformat-system` to the `:export` list in `src/package.lisp`.

**Step 3: Implement `reformat-file`**

Algorithm:
1. Read the file into a string (INV-9).
2. Identify the header block: all text before the first toplevel form's source position.
3. Read toplevel forms using `read` from a string stream (standard CL reader, not Eclector — we want `read > print` round-trip without CST overhead).
4. Pretty-print each form via `(pretty-print-form form 0)`.
5. Reassemble: header + forms joined by two blank lines + trailing newline.
6. Write atomically via `uiop:tmpize-pathname` + `uiop:rename-file-overwriting-target` (INV-8).

The header block is everything before the first `(` that starts a toplevel form at column 0. This preserves `;;;;` comment lines, copyright blocks, and SPDX headers.

**Step 4: Implement `reformat-system`**

```lisp
(defun reformat-system (system-designator &key (sibling-systems t))
  (let* ((system (asdf:find-system system-designator))
         (files (if sibling-systems
                    (collect-all-source-files system)
                    (collect-system-source-files system))))
    (dolist (pathname files)
      (when (string-equal "lisp" (pathname-type pathname))
        (reformat-file pathname)))))
```

Filters to `.lisp` files only (skips `.asd` files — those have `defsystem` forms that should not be reformatted by the pretty-printer).

**Step 5: Define `*emacs-pretty-printer-configuration*`**

Default value is `nil` (no custom setup). The user will populate this during phase 2 convergence.

**Step 6: Implement `reformat-file-with-emacs`**

```lisp
(defun reformat-file-with-emacs (pathname &key (elisp-setup *emacs-pretty-printer-configuration*))
  "Reformat PATHNAME using Emacs in batch mode.
ELISP-SETUP is an Elisp string eval'd before reformatting."
  ...)
```

Uses `uiop:run-program` to call:
```
emacs --batch
      --eval "(require 'cl-indent)"
      --eval ELISP-SETUP        ;; when non-nil
      --eval "(find-file \"PATHNAME\")"
      --eval "(indent-region (point-min) (point-max))"
      --eval "(save-buffer)"
      --eval "(kill-emacs)"
```

**Step 7: Implement `validate-reference-fixture-fixed-point`**

Copies `reference.lisp` to a temp file, calls `(atelier:reformat-file temp)`, asserts the content is unchanged (string= before vs. after).

**Step 8: Implement `validate-reference-fixture-emacs-fixed-point`**

Same pattern but calls `reformat-file-with-emacs`. Skips if `emacs` not on PATH (checked via `(uiop:find-exe-on-path "emacs")`). This test will initially **fail** — that's expected. It exists so phase 2 convergence work has a mechanical check.

**Step 9: Wire testcases**

Add `validate-reference-fixture-fixed-point` to `testsuite-pretty-printer`. The Emacs test is slow and opt-in — add it to `testsuite-pretty-printer` with the skip guard.

## Invariants

All prior invariants (INV-1 through INV-14) confirmed. No new invariants in this phase.

## Test Fixtures

| Fixture | Location | Purpose |
|---------|----------|---------|
| `reference.lisp` | `test/fixtures/pretty-print/reference.lisp` | Comprehensive pretty-printer reference, must be a fixed point |

## References to Create

None.

## Acceptance Criteria

| # | Criterion | Verified by |
|---|-----------|-------------|
| AC1 | `reference.lisp` contains at least 15 distinct CL form types | Manual inspection of the fixture |
| AC2 | `(atelier:reformat-file reference.lisp)` produces output identical to its input | `validate-reference-fixture-fixed-point` |
| AC3 | `reformat-file` uses atomic write-back (INV-8) | Code review of the implementation |
| AC4 | `reformat-system` reformats all `.lisp` files in a system | Manual REPL test: `(atelier:reformat-system "org.melusina.atelier")` produces no changes on already-formatted code |
| AC5 | `reformat-file-with-emacs` calls Emacs in batch mode and reformats the file | Manual REPL test |
| AC6 | `reformat-file` and `reformat-system` are exported from the `atelier` package | `(find-symbol "REFORMAT-FILE" :atelier)` returns an external symbol |
| AC7 | Full regression suite passes with no failures and no new skips | `(asdf:test-system "org.melusina.atelier")` |
| AC8 | No existing autofix-cycle or pretty-printer fixture breaks | Subset of AC7 |

## Phase Closure Conditions

- All fast tests pass in a fresh SBCL subprocess (INV-4).
- `reference.lisp` is committed as a pretty-printer fixed point.
- `reformat-file` and `reformat-system` are exported and functional.
- `reformat-file-with-emacs` is functional (Emacs test may fail on the fixture — that is expected pre-convergence).
- Phase 1 completion notes written.
