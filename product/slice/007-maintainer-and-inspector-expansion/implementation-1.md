# Implementation Phase 1: Slice 007 — Autofix Cycle Redesign and New CST Inspectors

**Slice:** [product/slice/007-maintainer-and-inspector-expansion/slice.md](slice.md)
**Phase:** 1 of 1
**Scope:** Rename `testsuite/fixtures/maintainer/` to `testsuite/fixtures/autofix/`; redesign fixture format to make the `(inspector, finding, maintainer, resolution)` quadruple explicit; migrate the 10 existing maintainer fixtures; add self-idempotency (N=1) assertion; cross-populate each fixture's expected fixed code into the pretty-printer fixture set; add three new CST inspectors (IF→WHEN/UNLESS, single-form PROGN, WHEN-NOT→UNLESS).

---

## Prior Phases

None — this is the first implementation phase of slice 007.

Slice 007 was rescoped on 2026-04-09: fix-line-too-long implementation was removed from this slice because the text-vs-CST comparison question for string-breaking fixtures is unresolved. The skeleton maintainer and its 20 text fixtures remain in place but are not under test in this phase. See [../../roadmap.md](../../roadmap.md) for the deferral.

---

## Risk Register

| # | Risk | Category | Mitigation |
|---|------|----------|-----------|
| R1 | One or more existing maintainers fail the self-idempotency assertion after migration, blocking the slice | Scope boundary | The kill criterion in `slice.md` allows at most one failing maintainer to block. If found, isolate the fixture, mark the underlying bug, and skip the assertion for that fixture with a documented exclusion (recorded in invariants and notes). |
| R2 | Plist slot-check scheme doesn't cleanly express the CST-node assertion for syntax findings | Library API | Define a synthetic accessor `cst-node-raw` that reduces `(cst:raw (finding-cst-node finding))`. Fixtures using CST-node checks use `:cst-node-raw` as a slot key. Documented in the fixture format spec. |
| R3 | Directory rename (`maintainer/` → `autofix/`) breaks ASDF paths, git history, or downstream tooling | Portability | Use `git mv` to preserve history. `.asd` file references only `testsuite/fixtures/...` via `asdf:system-source-directory`; no hard-coded paths to `maintainer/` outside `testsuite/utilities.lisp` and `testsuite/autofix.lisp`. Grep confirmed before the move. |
| R4 | Cross-populated pretty-printer fixtures produce read/print round-trip failures for existing fixed-code documents (exposing a latent pretty-printer bug) | Library API | Run the cross-populated assertion once in a scratch buffer before committing the migration. If any fail, file the pretty-printer bug separately; the idempotency scope does not block on pretty-printer bugs, so exclude affected fixtures from the pretty-printer cross-population with a documented reason. |
| R5 | New fixture format's 4-document structure (front-matter + 3 body documents) exceeds what `read-file-documents-with-yaml-front-matter` currently supports | Library API | Read current function; extend it to handle N documents instead of hard-coding 2. Unit-test the parser on the new format before migrating fixtures. |
| R6 | Renaming the test function breaks any external caller (REPL bindings, other test files) | Scope boundary | Grep repo for `validate-one-maintainer-fixture` before renaming; update all call sites in the same commit. |

---

## OSS Components

None — implementing with existing infrastructure (Eclector, CL built-in pprint, cl-ppcre, org.melusina.confidence).

---

## Phase Scope

**Must deliver (this phase):** S1–S7 from `slice.md`.

**Deferred:** fix-line-too-long implementation (removed from slice scope on 2026-04-09; see `../../roadmap.md` Later section).

---

## Architecture

### Decision 1 — Autofix-cycle fixture format

Each fixture is a single `.text` file with YAML front-matter plus exactly three body documents separated by `---`:

```
---
description: Bare LOOP clause keyword replaced with keyword symbol
inspector: check-loop-keywords
finding: bare-loop-keyword-finding
maintainer: fix-bare-loop-keywords
resolution: text-resolution
---
(loop for item in items when (valid-item-p item) collect (transform-item item))
---
(:cst-node-raw for :severity :style)
---
(loop :for item :in items :when (valid-item-p item) :collect (transform-item item))
---
```

- **Front-matter** declares the diagnostic quadruple. All four symbol fields
  (`inspector`, `finding`, `maintainer`, `resolution`) are mandatory. Symbols
  are interned in `:atelier` at fixture-load time.
- **Document 1** — input source. Typically one top-level form; may be
  multi-line.
- **Document 2** — expected finding slot values as a plist. The plist keys
  are either (a) slot reader names for line-findings (`:line`, `:column`,
  `:end-line`, `:end-column`, `:source-text-substring`), or (b) synthetic
  keys for syntax-findings (`:cst-node-raw`, `:severity`, `:observation-matches`).
  Only the named slots are checked; unnamed slots are ignored. The plist may
  be `nil` (empty) to skip finding slot checks entirely.
- **Document 3** — expected fixed code, as source text. The test reads this
  with `read-from-string` to obtain the expected AST for comparison.

### Decision 2 — Structural comparison level

For SYNTAX-INSPECTORS, the test asserts `(equal expected-form result-form)`
where both are produced by `read-from-string` on their respective strings.
CST node identity is never compared; raw forms are.

For LINE-INSPECTORS (currently: check-trailing-whitespace,
check-line-length, check-mixed-indentation), the test asserts at the level
appropriate to the maintainer's output. For fix-trailing-whitespace and
fix-mixed-indentation, the fixes are textual and the comparison remains
textual (`string=`). For future line-level maintainers that produce
structural fixes, the fixture will declare the level via the front-matter
`comparison` field (defaulting to `form`).

### Decision 3 — Finding slot-check scheme

Plist keys are resolved in order:

1. If the key is a slot reader name on the finding class, the corresponding
   slot is read and compared with `equal`.
2. If the key is a synthetic accessor defined in `autofix.lisp`
   (currently: `:cst-node-raw`, `:observation-matches`,
   `:source-text-substring`), the corresponding helper is invoked.
3. Otherwise the fixture load fails with a clear error.

Synthetic accessors:

- `:cst-node-raw` → `(cst:raw (finding-cst-node finding))`; compared with `equal`.
- `:observation-matches` → substring match on `(finding-observation finding)`.
- `:source-text-substring` → substring match on `(finding-source-text finding)`.

### Decision 4 — Self-idempotency assertion

After the primary assertion (`result-form = expected-form` or
`result-string = expected-string` per fixture level):

1. Re-run the same `(inspector, maintainer)` pair on the **result** (not on
   the original input).
2. Collect any resolutions and apply them.
3. Assert that the second result equals the first result at the same level
   used for the primary assertion.

Convention: **N = 1**. A maintainer that requires a second pass to produce
stable output is a bug (see risk R1 for the exclusion protocol). This is
informed by the shallow review of Ruff and ESLint in
`references/linter-convergence.md` — both linters cap at some N > 1
(`MAX_AUTOFIX_PASSES = 10` in ESLint; unnamed in Ruff) because they target
*pipeline* convergence, not *per-rule* idempotency. Our scope here is
per-rule, so N = 1 is the right contract.

### Decision 5 — Pretty-printer cross-population

Every autofix-cycle fixture's document 3 (expected fixed code) is also read
by the pretty-printer fixture test. For each such document, the pretty-printer
test asserts:

```
(string= expected-fixed-code
         (pretty-print-form (read-from-string expected-fixed-code) 0))
```

i.e., the expected fixed code is a fixed point of `read ⟫ pretty-print`.
This is the property that allows the maintainer to produce AST and delegate
textual output entirely to the pretty-printer.

The cross-population lives in a new discovery function
`discover-autofix-cycle-fixtures-for-pretty-printer`, which walks the same
directory and returns the list of expected-fixed-code strings. The
pretty-printer test calls this in addition to the existing
`discover-pretty-printer-fixtures`.

### Decision 6 — Three new CST inspectors

Each is a standalone syntax inspector following the `check-earmuffs`
pattern: `define-syntax-inspector`, recursive CST walk with `labels`,
collect findings into a list via `push`, `nreverse` on return.

| Inspector | Finding class | Pattern detected |
|-----------|---------------|-----------------|
| `check-single-branch-if` | `single-branch-if-finding` | `(if T E nil)`, `(if T nil E)`, `(if T E)` |
| `check-single-form-progn` | `single-form-progn-finding` | `(progn F)` with exactly one body form |
| `check-when-not` | `when-not-finding` | `(when (not X) ...)` |

No maintainers in this phase — the inspectors are diagnostic-only. Writing
the maintainers can be a follow-up slice.

---

## File Organisation

```
src/
├── finding.lisp                             [modify] — add 3 new finding classes
├── package.lisp                             [modify] — export new finding + inspector symbols
├── inspectors/
│   ├── check-single-branch-if.lisp          [new]
│   ├── check-single-form-progn.lisp         [new]
│   └── check-when-not.lisp                  [new]
testsuite/
├── utilities.lisp                           [modify] — extend read-file-documents-with-yaml-front-matter; rewrite read-maintainer-fixture → read-autofix-cycle-fixture; update fixture/ accessors; discover-autofix-cycle-fixtures
├── autofix.lisp                             [modify] — rewrite validate-one-maintainer-fixture → validate-one-autofix-cycle-fixture; level-dispatched inspection; plist slot check; self-idempotency assertion; pretty-printer cross-population entry point
├── fixtures/
│   ├── autofix/                             [new dir; replaces maintainer/]
│   │   ├── fix-earmuffs/                    [migrated from maintainer/]
│   │   ├── fix-constant-naming/             [migrated]
│   │   ├── fix-bare-lambda/                 [migrated]
│   │   ├── fix-bare-loop-keywords/          [migrated]
│   │   ├── fix-labels-to-flet/              [migrated]
│   │   ├── fix-trailing-whitespace/         [migrated]
│   │   ├── fix-mixed-indentation/           [migrated]
│   │   ├── fix-header-line/                 [migrated]
│   │   ├── fix-footer-line/                 [migrated]
│   │   ├── fix-project-identification/      [migrated]
│   │   └── fix-line-too-long/               [MIGRATED AS SKELETON; tests skipped]
│   └── inspector/
│       ├── check-single-branch-if/
│       │   ├── when.lisp                    [new]
│       │   ├── unless.lisp                  [new]
│       │   ├── clean.lisp                   [new]
│       │   └── implicit-nil.lisp            [new]
│       ├── check-single-form-progn/
│       │   ├── baseline.lisp                [new]
│       │   ├── clean.lisp                   [new]
│       │   └── empty.lisp                   [new]
│       └── check-when-not/
│           ├── baseline.lisp                [new]
│           └── clean.lisp                   [new]
├── inspectors/
│   ├── check-single-branch-if.lisp          [new]
│   ├── check-single-form-progn.lisp         [new]
│   └── check-when-not.lisp                  [new]
testsuite/fixtures/maintainer/               [REMOVED — directory deleted via git mv]
org.melusina.atelier.asd                     [modify] — register 3 new inspector source files and 3 new test files
CLAUDE.md                                    [modify] — update fixture directory path and test function name in the fixture section
```

**Decision rule for new fixture files:** inspector fixtures go under
`testsuite/fixtures/inspector/<inspector-name>/<case-name>.lisp`.
Autofix-cycle fixtures go under `testsuite/fixtures/autofix/<maintainer-name>/<case-name>.text`.
The directory name carries the maintainer/inspector identity; the case name
describes the scenario.

---

## Build System Changes

In `org.melusina.atelier.asd`:

- **`:org.melusina.atelier/src/inspectors` module** — append three components
  after the existing inspector files: `check-single-branch-if`,
  `check-single-form-progn`, `check-when-not`.
- **`:org.melusina.atelier/testsuite/inspectors` module** — append three
  corresponding test files.

No new ASDF systems. No new dependencies. No load-order changes: the new
inspector files have no cross-dependencies beyond the existing
`define-syntax-inspector` macro.

---

## Package / Module Architecture

New exports in `src/package.lisp`, grouped by concept:

**Findings (new):**

```lisp
#:single-branch-if-finding
#:single-form-progn-finding
#:when-not-finding
```

**Inspectors (new):**

```lisp
#:check-single-branch-if
#:check-single-form-progn
#:check-when-not
```

No changes to the public API surface of `atelier` itself for the autofix
cycle rework. The fixture format and test function names are
`atelier/testsuite`-internal.

---

## Type / Class Hierarchy

```
finding > file-finding > line-finding > syntax-finding
├── single-branch-if-finding          [new, concrete]
├── single-form-progn-finding         [new, concrete]
└── when-not-finding                  [new, concrete]
```

No new classes in the resolution hierarchy.

---

## Protocol Definitions

### `read-autofix-cycle-fixture` (new, in `testsuite/utilities.lisp`)

```lisp
(defun read-autofix-cycle-fixture (pathname)
  "Read an autofix-cycle fixture from PATHNAME.
Returns (values front-matter input-source expected-finding-slots expected-fixed-code)
where FRONT-MATTER is an alist with entries for :description, :inspector,
:finding, :maintainer, :resolution; INPUT-SOURCE is a string; EXPECTED-FINDING-SLOTS
is a plist (or NIL); EXPECTED-FIXED-CODE is a string.
Signals an error if the file does not have a front-matter block followed by
exactly three body documents, or if any of the mandatory front-matter fields
is missing.")
```

### `discover-autofix-cycle-fixtures` (new, in `testsuite/utilities.lisp`)

```lisp
(defun discover-autofix-cycle-fixtures ()
  "Return an alist (maintainer-symbol . list-of-fixture-pathnames).
Walks testsuite/fixtures/autofix/*/ and maps each subdirectory name to its
.text files. Fixtures for maintainers whose symbols cannot be found in
:atelier are silently skipped with a warning.")
```

### `validate-one-autofix-cycle-fixture` (renamed from `validate-one-maintainer-fixture`)

```lisp
(define-testcase validate-one-autofix-cycle-fixture (maintainer-symbol
                                                     &optional (case-name "baseline"))
  "Validate one autofix-cycle fixture. Exercises the full (inspector, finding,
maintainer, resolution) quadruple: runs the inspector, asserts the expected
finding slots, runs the maintainer, asserts the resolution class, applies
the resolution, asserts the fixed code matches, and finally asserts
self-idempotency under one additional pass.")
```

### Pretty-printer cross-population entry point (new)

```lisp
(define-testcase validate-autofix-cycle-fixtures-as-pretty-printer-fixed-points ()
  "For every autofix-cycle fixture, assert that the expected fixed code is
a fixed point of read ⟫ pretty-print-form. This property lets the maintainer
delegate all textual output to the pretty-printer.")
```

---

## Error / Condition Types

No new conditions. Fixture load errors signal `simple-error` with a clear
message naming the offending file and field.

---

## Test Plan

| Story / Criterion | Test name | Category | Skip condition |
|---|---|:---:|---|
| S1: new fixture format — all migrated fixtures load | `validate-one-autofix-cycle-fixture` per migrated fixture (auto-discovered) | fast | — |
| S1: test function rename | Grep confirms no caller of `validate-one-maintainer-fixture` after the rename | fast | — |
| S2: structural comparison | Each syntax-inspector fixture asserts `(equal expected-form result-form)` | fast | — |
| S2: finding slot check | Each fixture's plist keys are resolved against the produced finding | fast | — |
| S3: self-idempotency | Each fixture's second pass matches the first pass | fast | — |
| S4: pretty-printer cross-population | `validate-autofix-cycle-fixtures-as-pretty-printer-fixed-points` | fast | — |
| S5: check-single-branch-if | `validate-inspector-fixtures` for check-single-branch-if (4 fixtures) | fast | — |
| S6: check-single-form-progn | `validate-inspector-fixtures` for check-single-form-progn (3 fixtures) | fast | — |
| S7: check-when-not | `validate-inspector-fixtures` for check-when-not (2 fixtures) | fast | — |
| QA: no regressions | `(asdf:test-system "org.melusina.atelier")` | fast | — |

All tests are fast (in-memory only). No slow or snail tests are added in
this phase.

---

## Implementation Order (Step Table)

Rule: every implementation step's verification test must only depend on forms written in prior steps. The order below reflects that.

| Step | File | Action | Form(s) | Test name | Cat |
|------|------|--------|---------|-----------|:---:|
| 1 | `testsuite/utilities.lisp` [modify] | Verify `read-file-documents-with-yaml-front-matter` returns all `---`-separated documents (not a fixed N). Extend if needed. | `read-file-documents-with-yaml-front-matter` | — | — |
| 2 | `testsuite/utilities.lisp` [modify] | Implement `read-autofix-cycle-fixture`. Reads front-matter, extracts mandatory symbol fields, reads three body documents. Signals a clear error when any mandatory field is missing. | `read-autofix-cycle-fixture` | — | — |
| 3 | `testsuite/utilities.lisp` [modify] | Implement `discover-autofix-cycle-fixtures`. Walks `testsuite/fixtures/autofix/*/`. Returns empty list if directory does not yet exist. | `discover-autofix-cycle-fixtures` | — | — |
| 4 | `testsuite/utilities.lisp` [modify] | Add `:autofix` kind to `fixture` function. Leave the existing `:maintainer` kind in place for one step as a transition aid. | `fixture` (extended) | — | — |
| 5 | `testsuite/fixtures/` [dir rename] | `git mv testsuite/fixtures/maintainer testsuite/fixtures/autofix`. Preserves git history. | — | — | — |
| 6 | `testsuite/fixtures/autofix/fix-earmuffs/baseline.text` [modify] | Migrate this one fixture to the new 4-part format as a probe. Keeps the implementation step (7) verifiable against a real fixture. | — | — | — |
| 7 | `testsuite/autofix.lisp` [modify] | Implement `validate-one-autofix-cycle-fixture` — primary assertion only, no idempotency yet. Dispatches on `inspector-level`: `:syntax` calls `inspect-syntax` on parsed CST; `:line` calls `inspect-line` per line with `*current-pathname*` and `*current-line-vector*` bound. Runs the maintainer, collects resolutions, applies, asserts at the correct level. Includes the plist slot-check helper with synthetic accessors (`:cst-node-raw`, `:observation-matches`, `:source-text-substring`). | `validate-one-autofix-cycle-fixture`, `autofix-cycle-finding-slot-check` | `(validate-one-autofix-cycle-fixture 'fix-earmuffs)` — verifies against the fix-earmuffs probe fixture migrated in step 6 | fast |
| 8 | `testsuite/autofix.lisp` [modify] | Extend `validate-one-autofix-cycle-fixture` with the second-pass self-idempotency assertion. | — | `(validate-one-autofix-cycle-fixture 'fix-earmuffs)` | fast |
| 9 | `testsuite/autofix.lisp` [modify] | Implement `validate-autofix-cycle-fixtures` entry point — iterates `discover-autofix-cycle-fixtures`, calling the single-fixture test for each. | `validate-autofix-cycle-fixtures` | `(validate-autofix-cycle-fixtures)` — currently exercises only fix-earmuffs | fast |
| 10 | `testsuite/autofix.lisp` [modify] | Implement `validate-autofix-cycle-fixtures-as-pretty-printer-fixed-points`. For each discovered autofix-cycle fixture, read its expected-fixed-code document and assert it is a fixed point of `read ⟫ pretty-print-form`. | `validate-autofix-cycle-fixtures-as-pretty-printer-fixed-points` | `(validate-autofix-cycle-fixtures-as-pretty-printer-fixed-points)` | fast |
| 11 | `testsuite/fixtures/autofix/fix-constant-naming/baseline.text` [modify] | Migrate to new 4-part format. | — | `(validate-one-autofix-cycle-fixture 'fix-constant-naming)` | fast |
| 12 | `testsuite/fixtures/autofix/fix-bare-lambda/*.text` [modify] | Migrate baseline and chain cases. | — | `(validate-one-autofix-cycle-fixture 'fix-bare-lambda)` | fast |
| 13 | `testsuite/fixtures/autofix/fix-bare-loop-keywords/baseline.text` [modify] | Migrate. | — | `(validate-one-autofix-cycle-fixture 'fix-bare-loop-keywords)` | fast |
| 14 | `testsuite/fixtures/autofix/fix-labels-to-flet/*.text` [modify] | Migrate. | — | `(validate-one-autofix-cycle-fixture 'fix-labels-to-flet)` | fast |
| 15 | `testsuite/fixtures/autofix/fix-trailing-whitespace/baseline.text` [modify] | Migrate. Line-level; textual comparison. | — | `(validate-one-autofix-cycle-fixture 'fix-trailing-whitespace)` | fast |
| 16 | `testsuite/fixtures/autofix/fix-mixed-indentation/baseline.text` [modify] | Migrate. | — | `(validate-one-autofix-cycle-fixture 'fix-mixed-indentation)` | fast |
| 17 | `testsuite/fixtures/autofix/fix-header-line/baseline.text` [modify] | Migrate. | — | `(validate-one-autofix-cycle-fixture 'fix-header-line)` | fast |
| 18 | `testsuite/fixtures/autofix/fix-footer-line/baseline.text` [modify] | Migrate. | — | `(validate-one-autofix-cycle-fixture 'fix-footer-line)` | fast |
| 19 | `testsuite/fixtures/autofix/fix-project-identification/baseline.text` [modify] | Migrate. | — | `(validate-one-autofix-cycle-fixture 'fix-project-identification)` | fast |
| 20 | `testsuite/autofix.lisp` [modify] | Delete the old `validate-one-maintainer-fixture` and its callers. Update `testsuite-autofix` entry point to call `validate-autofix-cycle-fixtures` and `validate-autofix-cycle-fixtures-as-pretty-printer-fixed-points`. | — | `(validate-autofix-cycle-fixtures)` + `(validate-autofix-cycle-fixtures-as-pretty-printer-fixed-points)` | fast |
| 21 | `testsuite/utilities.lisp` [modify] | Remove the transitional `:maintainer` kind from `fixture`. | — | — | — |
| 22 | `src/finding.lisp` [modify] | Add three new finding classes via `define-findings`. | `single-branch-if-finding`, `single-form-progn-finding`, `when-not-finding` | — | — |
| 23 | `src/package.lisp` [modify] | Export three new finding classes and three new inspector names. | — | — | — |
| 24 | `src/inspectors/check-single-branch-if.lisp` [new] | Implement. Recursive walk. Detect `(if T E nil)`, `(if T nil E)`, `(if T E)`. | `check-single-branch-if` | — | — |
| 25 | `testsuite/fixtures/inspector/check-single-branch-if/` [new] | Create four fixtures (when, unless, clean, implicit-nil). | — | — | — |
| 26 | `testsuite/inspectors/check-single-branch-if.lisp` [new] | Test case delegating to fixture auto-discovery. | `validate-check-single-branch-if` | `(validate-check-single-branch-if)` | fast |
| 27 | `src/inspectors/check-single-form-progn.lisp` [new] | Implement. Detect `(progn F)` with exactly one body form. | `check-single-form-progn` | — | — |
| 28 | `testsuite/fixtures/inspector/check-single-form-progn/` [new] | Create three fixtures (baseline, clean, empty). | — | — | — |
| 29 | `testsuite/inspectors/check-single-form-progn.lisp` [new] | Test case. | `validate-check-single-form-progn` | `(validate-check-single-form-progn)` | fast |
| 30 | `src/inspectors/check-when-not.lisp` [new] | Implement. Detect `(when (not X) ...)`. | `check-when-not` | — | — |
| 31 | `testsuite/fixtures/inspector/check-when-not/` [new] | Create two fixtures (baseline, clean). | — | — | — |
| 32 | `testsuite/inspectors/check-when-not.lisp` [new] | Test case. | `validate-check-when-not` | `(validate-check-when-not)` | fast |
| 33 | `org.melusina.atelier.asd` [modify] | Register three new inspector source files and three new test files in the respective modules. | — | — | — |
| 34 | `CLAUDE.md` [modify] | Update the fixture section: rename `testsuite/fixtures/maintainer/` to `testsuite/fixtures/autofix/`, rename `validate-one-maintainer-fixture` to `validate-one-autofix-cycle-fixture`, document the new 4-part format and the N=1 self-idempotency contract. | — | — | — |
| 35 | — | Full regression. | — | `(asdf:test-system "org.melusina.atelier")` | fast |

---

## Invariants

Carried forward from prior slices:

| # | Invariant |
|---|-----------|
| I1 | Every source file begins with `;;;; filename — Description` and ends with `;;;; End of file 'filename'`. |
| I2 | LOOP clause keywords are always keyword symbols. |
| I3 | `MAPCAR`/`MAPCAN`/`REMOVE-IF` receive a named `FLET` function, never a bare `LAMBDA`. |
| I4 | Tests use `define-testcase` from `org.melusina.confidence`. |
| I5 | No SBCL-specific extensions without `#+sbcl` guard. |
| I6 | All `defsystem` forms live in `org.melusina.atelier.asd`. |
| I7 | Never use `SHADOWING-IMPORT`; qualify at use site or `UNINTERN` first. |
| I8 | `assert-condition` argument order: form first, condition-type second. |
| I9 | `assert-t` checks for exactly `T`; use `(assert-t (not (null X)))` for generalised boolean. |
| I10 | All new exports are grouped by concept in `package.lisp`. |
| I11 | Maintainers are defined with `define-automatic-maintainer`, one per file under `src/maintainers/`. |
| I12 | Pretty-printer fixtures use the multi-margin YAML format. |
| I13 | (superseded by I46, I47 below). |
| I14 | `*atelier-pprint-dispatch*` is never modified after load time. |
| I15 | Write-back uses temp-file + rename for atomicity; source files are never partially written. |
| I16 | `lint-system` without `:autofix` preserves current behaviour — no files modified. |
| I17 | Resolutions are applied end-to-start (descending offset order) to avoid position corruption. |
| I31 | `inspect-syntax` receives each top-level CST form individually. |
| I32 | `parse-lisp-file` returns NIL on non-Lisp files or parse errors — never signals. |
| I33 | `make-syntax-finding-from-form` derives line/column from CST source positions. |
| I34 | `define-syntax-inspector` generates `inspect-syntax` methods. |
| I35 | `*current-cst-root*` is bound to the list of top-level forms during syntax inspection. |
| I36 | Syntax inspectors only run on files parseable by Eclector. |
| I37 | The bare lambda check targets a specific list of higher-order functions. |
| I38 | The loop keyword check targets well-known clause keywords only. |

New invariants for this phase:

| # | Invariant |
|---|-----------|
| I46 | Autofix-cycle fixtures live under `testsuite/fixtures/autofix/<maintainer-name>/`. Each fixture declares `inspector`, `finding`, `maintainer`, `resolution` in its YAML front-matter as mandatory symbol fields interned in `:atelier`. |
| I47 | Autofix-cycle fixtures have exactly three body documents separated by `---`: input source, expected finding slots plist, expected fixed code. |
| I48 | For SYNTAX-INSPECTOR fixtures, the primary assertion compares forms via `(equal expected-form result-form)` where both are produced by `read-from-string`. For LINE-INSPECTOR fixtures, the default is `string=` on the raw text. |
| I49 | Every autofix-cycle fixture asserts **self-idempotency at N=1**: re-running the same `(inspector, maintainer)` pair on the result yields the same result as the first pass. Non-idempotent maintainers are bugs. |
| I50 | Every autofix-cycle fixture's expected fixed code document is also a pretty-printer fixed-point fixture: `(string= x (pretty-print-form (read-from-string x) 0))` holds for every such document. |
| I51 | Finding slot checks use a plist where keys are either slot reader names or documented synthetic accessors (`:cst-node-raw`, `:observation-matches`, `:source-text-substring`). Unknown keys signal an error at fixture load time. |
| I52 | The autofix-cycle test function is `validate-one-autofix-cycle-fixture`. The legacy name `validate-one-maintainer-fixture` is no longer in the source tree after this phase. |

---

## Test Fixtures

**Autofix-cycle fixtures (migrated):** 10 existing fixtures under
`testsuite/fixtures/maintainer/*/` are moved to `testsuite/fixtures/autofix/*/`
via `git mv` (preserving history) and rewritten to the new 4-part format.
Each keeps its input source and expected fixed code; the finding-slots plist
is filled in per fixture.

The 20 fix-line-too-long text fixtures under
`testsuite/fixtures/maintainer/fix-line-too-long/` are moved to
`testsuite/fixtures/autofix/fix-line-too-long/` as-is (preserving history)
but are **not migrated to the new format and are not exercised by the test
suite** in this phase. A fixture-loader guard silently skips fixtures whose
first line is not `---` or whose front-matter lacks mandatory fields.

**Inspector fixtures (new):**

| Inspector | Fixture | Content | Expected |
|-----------|---------|---------|----------|
| check-single-branch-if | when.lisp | `(if (foo) (bar) nil)` | Finding |
| check-single-branch-if | unless.lisp | `(if (foo) nil (bar))` | Finding |
| check-single-branch-if | clean.lisp | `(if (foo) (bar) (baz))` | No finding |
| check-single-branch-if | implicit-nil.lisp | `(if (foo) (bar))` | Finding |
| check-single-form-progn | baseline.lisp | `(progn (foo))` | Finding |
| check-single-form-progn | clean.lisp | `(progn (foo) (bar))` | No finding |
| check-single-form-progn | empty.lisp | `(progn)` | No finding |
| check-when-not | baseline.lisp | `(when (not x) (foo))` | Finding |
| check-when-not | clean.lisp | `(when x (foo))` | No finding |

---

## References to Create

Already committed as part of this phase's planning:

- `product/slice/007-maintainer-and-inspector-expansion/references/linter-convergence.md` — shallow research on Ruff and ESLint convergence, informing Decision 4 (N=1 contract).

No further reference files needed for this phase.

---

## Acceptance Criteria

| # | Criterion | Verification |
|---|-----------|-------------|
| AC1 | Directory renamed: `testsuite/fixtures/autofix/` exists; `testsuite/fixtures/maintainer/` does not | Filesystem check; git history shows `git mv` |
| AC2 | All 10 existing maintainer fixtures migrated to the new 4-part format and loading successfully | `(validate-autofix-cycle-fixtures)` runs without "fixture load error" signals |
| AC3 | All 10 migrated fixtures pass their primary assertion (inspector finds the expected finding; maintainer produces the expected fix) | `(validate-autofix-cycle-fixtures)` passes all 10 |
| AC4 | All 10 migrated fixtures pass the self-idempotency assertion at N=1 | Same test — zero fixtures trip the second-pass assertion |
| AC5 | All 10 migrated fixtures' expected fixed code documents are `read ⟫ pretty-print-form` fixed points | `(validate-autofix-cycle-fixtures-as-pretty-printer-fixed-points)` passes |
| AC6 | `check-single-branch-if` detects IF→WHEN and IF→UNLESS patterns | 4 fixtures pass via `(validate-inspector-fixtures)` |
| AC7 | `check-single-form-progn` detects single-body PROGN | 3 fixtures pass via `(validate-inspector-fixtures)` |
| AC8 | `check-when-not` detects WHEN (NOT ...) pattern | 2 fixtures pass via `(validate-inspector-fixtures)` |
| AC9 | `validate-one-maintainer-fixture` no longer exists in the source tree | `Grep validate-one-maintainer-fixture .` returns zero hits |
| AC10 | No regressions | `(asdf:test-system "org.melusina.atelier")` passes in full |

---

## Phase Closure Conditions

- All 10 acceptance criteria verified.
- `(asdf:test-system "org.melusina.atelier")` passes with zero failures.
- 10 migrated autofix-cycle fixtures auto-discovered and passing primary + idempotency + pretty-printer-fixed-point assertions.
- Three new CST inspectors registered and their fixtures passing.
- `implementation-1-notes.md` records AC verification, any deferred items, and confirms that no existing maintainer had to be excluded from the self-idempotency assertion. If any had to be excluded, the note lists which and why, and a follow-up issue is filed.
