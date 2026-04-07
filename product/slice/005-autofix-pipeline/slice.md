# Slice 005: Autofix Pipeline with Pretty-Printer

**Status:** Planned
**Type:** Bet
**Goal:** G2 — Linter covers file, line, region, and CST level with ASDF integration; G4 — Common Lisp code formatter
**OKR contribution:** G2 — Findings produce resolutions that are applied back to source files automatically. G4 — Pretty-printer emits correctly indented Lisp forms for structural transforms.
**Hypothesis assumption:** The full finding → resolution → write-back cycle can be delivered using text-resolution for targeted replacements and syntax-resolution with CL's built-in pretty-printer for structural transforms, without requiring a third-party formatting library.
**Hypothesis prediction:** A developer running `lint-system` with autofix enabled on a system with style violations receives corrected source files: trailing whitespace removed, indentation normalised, special variable and constant names gain their conventional markers, bare loop keywords replaced with keyword symbols, and bare lambdas in higher-order calls extracted to FLET — all preserving the surrounding code and formatting.
**Hypothesis disposition:** ✅ Validated — all prerequisite infrastructure exists (findings with source positions, resolution schema, maintainer registry with superseding), and the six target fixes range from trivial text replacements to one structural transform.
**Leading indicator:** Number of automatic maintainers producing applicable resolutions — baseline 0, target ≥ 6 by slice completion.
**Kill criterion:** If CL's built-in pretty-printer cannot emit correctly indented Lisp forms for the FLET wrapping transform (bare-lambda fix), the pretty-printer decision must be escalated. If write-back corrupts source files for any of the six fix types, the write-back engine design must be revisited.
**Planned start / end:** 2026-04-08 / 2026-04-15
**Actual end:**
**Implementation phases:**
  - Phase 1: product/slice/005-autofix-pipeline/implementation-1.md — Planned

---

## Customer Journey

Before this slice: The linter produces findings (file, line, and syntax level) but cannot fix them. A developer sees the diagnostic and must make corrections by hand. The resolution and maintainer registries exist as empty infrastructure — no concrete maintainer is defined, no write-back engine exists.

After this slice: A developer calls `(atelier:lint-system :my-system :autofix t)` and the linter both reports findings and applies automatic fixes to the source files. Trailing whitespace is stripped, tab/space indentation is normalised, special variable and constant names gain their conventional markers, bare loop keywords become keyword symbols, and bare lambdas in mapcar/remove-if/etc. are extracted into named FLET functions. Each fix is a resolution produced by a registered maintainer, applied through a write-back engine that preserves surrounding code. Structural transforms (bare-lambda → FLET) use CL's built-in pretty-printer to emit correctly indented replacement forms.

## Personas served
- End user: Common Lisp developer — runs the linter and gets automatic fixes applied to source files.
- Tool author: Third-party inspector author — sees the pattern for writing a maintainer that pairs with an inspector, and learns how to use text-resolution vs syntax-resolution.

## Stories

### S1: Write-back engine
**As a** tool author, **I want** a write-back engine that applies resolutions to source files, **so that** findings can be fixed automatically.
**Acceptance criteria:**
- Given a file with a finding and a text-resolution with replacement text, when the write-back engine runs, then the source text at the finding's location is replaced and the file is written back.
- Given a file with a finding and a syntax-resolution with a transform function, when the write-back engine runs, then the transform is applied, the result is pretty-printed, and the file is written back.
- Given multiple resolutions on the same file at different positions, when the write-back engine runs, then all are applied without position corruption (end-to-start ordering).
- Given a resolution whose finding spans multiple lines, when the write-back engine runs, then the entire span is replaced.

### S2: Line-level automatic maintainers
**As a** developer, **I want** automatic maintainers for trailing whitespace and mixed indentation, **so that** these findings are fixed without manual editing.
**Acceptance criteria:**
- Given a trailing-whitespace-finding, when `prepare-resolution` is called, then a text-resolution is returned with the line stripped of trailing whitespace.
- Given a mixed-indentation-finding with style `:spaces`, when `prepare-resolution` is called, then a text-resolution is returned with tabs replaced by spaces.
- Given a mixed-indentation-finding with style `:tabs`, when `prepare-resolution` is called, then a text-resolution is returned with leading spaces replaced by tabs.
- Given both maintainers are registered, when I call `(atelier:list-maintainers)`, then `fix-trailing-whitespace` and `fix-mixed-indentation` appear.

### S3: CST-level automatic maintainers via text-resolution
**As a** developer, **I want** automatic maintainers for earmuffs, constant naming, and bare loop keywords, **so that** naming and style conventions are enforced automatically.
**Acceptance criteria:**
- Given an earmuffs-finding for `(defvar wrong-name ...)`, when `prepare-resolution` is called, then a text-resolution is returned replacing `wrong-name` with `*wrong-name*`.
- Given a constant-naming-finding for `(defconstant wrong-name ...)`, when `prepare-resolution` is called, then a text-resolution is returned replacing `wrong-name` with `+wrong-name+`.
- Given a bare-loop-keyword-finding for bare `for`, when `prepare-resolution` is called, then a text-resolution is returned replacing `for` with `:for`.
- Given all three maintainers are registered, when I call `(atelier:list-maintainers)`, then `fix-earmuffs`, `fix-constant-naming`, and `fix-bare-loop-keywords` appear.

### S4: Pretty-printer for syntax write-back
**As a** tool author, **I want** a pretty-printer that emits Lisp forms as correctly indented source text, **so that** syntax-resolution transforms produce readable replacement code.
**Acceptance criteria:**
- Given a simple form `(defvar *x* 1)` and column 0, when the pretty-printer runs, then the output matches CL conventions (lowercase, standard indentation).
- Given a FLET form and column 4, when the pretty-printer runs, then each line after the first is indented by at least 4 spaces.
- Given a `loop` form with keyword clause symbols, when the pretty-printer runs, then clause keywords remain keyword symbols (`:for`, `:collect`, etc.).
- Given a form with nested lists, when the pretty-printer runs, then the output stays within a reasonable right margin (configurable, default 100).
- All pretty-printer tests use the YAML fixture format (see Test Fixture Design below).

### S5: Bare-lambda automatic maintainer via syntax-resolution
**As a** developer, **I want** an automatic maintainer that extracts bare lambdas in higher-order calls into named FLET functions, **so that** the code follows the named-function convention.
**Acceptance criteria:**
- Given `(mapcar (lambda (x) (1+ x)) items)`, when `prepare-resolution` is called, then a syntax-resolution is returned whose transform produces a FLET-wrapped form.
- Given the FLET form is pretty-printed at the original column, then the output is correctly indented.
- Given the maintainer is registered, when I call `(atelier:list-maintainers)`, then `fix-bare-lambda` appears.
- The maintainer tests use the YAML fixture format with AST comparison (see Test Fixture Design below).

### S6: Batch autofix integration
**As a** developer, **I want** `lint-system` to accept an autofix option, **so that** I can lint and fix in one call.
**Acceptance criteria:**
- Given `(lint-system :my-system :autofix t)`, when the system has fixable findings, then the source files are modified and the returned findings list reflects what was fixed.
- Given `(lint-system :my-system)` without autofix, when the system has findings, then no source files are modified (current behaviour preserved).
- Given a file with both fixable and unfixable findings, when autofix runs, then only the fixable findings are resolved; unfixable findings are reported as before.

---

## Test Fixture Design

This slice introduces a YAML-separated document format for test fixtures, used in two modes with different comparison strategies. The format reuses the existing `read-file-documents-with-yaml-front-matter` utility.

### Fixture format

A `.text` file containing three YAML-separated documents:

```
---
description: <human-readable description of what is tested>
column: <integer, insertion column for pretty-printer tests>
---
<input: source text or Lisp form>
---
<expected: the correct output>
```

The front-matter (first document) carries test parameters as a YAML alist. The second document is the input. The third document is the expected output.

### Two comparison modes

**String comparison — for pretty-printer tests (S4):**

The pretty-printer is a pure function from `(form, column) → text`. Tests `READ-FROM-STRING` the input document to obtain a Lisp form, pass it to the pretty-printer at the declared column, and compare the result to the expected document using **string equality**. This verifies exact formatting: indentation, line breaks, case.

```
---
description: FLET with single binding
column: 0
---
(flet ((compute-total (x) (+ x tax))) (mapcar #'compute-total prices))
---
(flet ((compute-total (x)
         (+ x tax)))
  (mapcar #'compute-total prices))
```

```
---
description: DEFUN with docstring
column: 0
---
(defun compute-discount (price rate) "Return PRICE reduced by RATE." (* price (- 1.0 rate)))
---
(defun compute-discount (price rate)
  "Return PRICE reduced by RATE."
  (* price (- 1.0 rate)))
```

```
---
description: LOOP with keyword clause symbols at column 4
column: 4
---
(loop :for item :in items :when (valid-item-p item) :collect (transform-item item))
---
(loop :for item :in items
          :when (valid-item-p item)
            :collect (transform-item item))
```

**AST comparison — for maintainer/linter tests (S2, S3, S5):**

Maintainer transforms are functions from `finding → resolution`. The resolution carries either replacement text (text-resolution) or a transform function (syntax-resolution) that produces a new sexp. Tests `READ-FROM-STRING` both the input and expected documents, apply the maintainer's transform to the input, and compare the result to the expected output using **`EQUAL` on the read forms**. This verifies the structural correctness of the transform without coupling to formatting.

```
---
description: Earmuffs fix adds star convention
inspector: check-earmuffs
---
(defvar wrong-name 42)
---
(defvar *wrong-name* 42)
```

```
---
description: Bare lambda extracted to FLET
inspector: check-bare-lambda
---
(mapcar (lambda (x) (1+ x)) items)
---
(flet ((compute-successor (x) (1+ x)))
  (mapcar #'compute-successor items))
```

```
---
description: Bare loop keywords replaced with keyword symbols
inspector: check-loop-keywords
---
(loop for item in items when (valid-item-p item) collect (transform-item item))
---
(loop :for item :in items :when (valid-item-p item) :collect (transform-item item))
```

### Why two modes

The pretty-printer and the maintainers are tested independently:

- **Pretty-printer tests** prove that given any sexp and a column, the output is correctly formatted text. No linter, no findings, no transforms.
- **Maintainer tests** prove that given a finding, the transform produces the correct sexp. The comparison is structural (AST), so the test does not break if the pretty-printer's formatting changes.

A maintainer's transform feeds into the pretty-printer during write-back, but each is tested in isolation. Integration tests in S1 (write-back engine) and S6 (batch autofix) exercise the full pipeline.

### Fixture directory layout

```
testsuite/fixtures/
├── pretty-print/                    ← S4: string comparison
│   ├── flet-single-binding.text
│   ├── flet-multi-arg.text
│   ├── defun-with-docstring.text
│   ├── loop-keyword-style.text
│   ├── let-binding.text
│   ├── nested-forms.text
│   └── ...
├── maintainer/                      ← S2, S3, S5: AST comparison
│   ├── fix-trailing-whitespace.text
│   ├── fix-mixed-indentation.text
│   ├── fix-earmuffs.text
│   ├── fix-constant-naming.text
│   ├── fix-bare-loop-keywords.text
│   ├── fix-bare-lambda-simple.text
│   ├── fix-bare-lambda-multi-arg.text
│   └── ...
```

---

## Pretty-Printer Design Decision (resolves backlog #23)

**Decision:** Use CL's built-in `pprint` with an isolated, Atelier-specific `*print-pprint-dispatch*` table. No third-party formatting library.

**Rationale:**
- SBCL's built-in pprint already handles `defun`, `let`, `let*`, `flet`, `labels`, `cond`, `case`, `progn`, `block`, `tagbody`, and most standard special forms with correct indentation.
- We only emit individual transformed forms, not reformat entire files. Comment preservation (which would require trivial-formatter or Eclector's `make-skipped-input-result`) is not needed — our transforms generate new code.
- A custom pprint-dispatch table is a first-class CL object. We create `*atelier-pprint-dispatch*` at load time and bind it dynamically only inside our code writer. The global table is never modified.
- If this proves insufficient for future transforms, we can layer trivial-formatter or Eclector comment-preservation on top without changing the architecture.

**Scope of custom dispatch entries** (expected to be small):
- `loop` — ensure keyword clause formatting matches project conventions
- Others added only if SBCL's defaults produce incorrect output for a specific fixture test

**What is deferred:**
- Whole-file reformatting (backlog #22) — requires comment preservation, which is a different problem
- Eclector `make-skipped-input-result` override for comment-preserving CST — future slice when structural transforms must carry interior comments

---

## Quality Attribute Acceptance Criteria
- [ ] All fast tests execute in under 2 seconds total.
- [ ] All slow tests execute in under 10 seconds total.
- [ ] Write-back never corrupts a source file: original content is recoverable (write to temp, then rename).
- [ ] Each maintainer is exported and documented with docstrings.
- [ ] Pretty-printer output is valid, readable Common Lisp that compiles without error.
- [ ] Autofix is opt-in — default `lint-system` behaviour is unchanged.

## Capability Maturity Transitions
- Autofix / Write-back (G2): Not started → Foundation
- Pretty-printer (G4): Not started → Foundation

## Definition of Ready
- [ ] Hypothesis disposition ✅
- [ ] Stories sized ≤ 2 days each
- [ ] Acceptance criteria written for all stories
- [ ] QA acceptance criterion defined
- [ ] Leading indicator baseline established: 0 automatic maintainers
- [ ] Dependencies clear — Slices 001–004 complete (findings, resolutions, maintainer registry, inspectors)
- [ ] Pretty-printer decision resolved within slice scope (backlog #23)

## Definition of Done
- [ ] All stories complete with acceptance criteria passing
- [ ] Quality attribute criteria passing
- [ ] Leading indicator being measured
- [ ] All implementation phases have completion notes
- [ ] `product/slice/005-autofix-pipeline/retrospective.md` created
- [ ] `product/maturity-tracker.md` updated
