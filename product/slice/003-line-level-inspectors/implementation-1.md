# Implementation Phase 1: Line-Level Inspectors

**Slice:** product/slice/003-line-level-inspectors/slice.md
**Phase:** 1 of 1
**Scope:** Implement three line-level inspectors (trailing whitespace, line length, mixed indentation), extend linter-configuration with indentation style, refactor `inspect-file` to 2-argument signature with special variables for configuration, update existing inspectors, extend runner to include line-inspector instances, remove legacy codestyle-0002.
**Prerequisites:** Slice 002 complete (ASDF integration, runner, file inspectors).

---

## Back-link

Slice: product/slice/003-line-level-inspectors/slice.md

## Prior Phases

None for this slice. Slice 002 established: `inspect-file` generic (3-arg, being changed to 2-arg), `run-file-inspectors`, `file-inspector` / `line-inspector` subclasses, `linter-configuration` with `disabled-inspectors` and `severity-overrides`.

---

## Risk Register

| # | Risk | Category | Mitigation |
|---|------|----------|-----------|
| R1 | Changing `inspect-file` from 3 args to 2 breaks existing inspectors | Scope boundary | Update `check-file-encoding` and `check-spdx-license-header` in same step; run full suite |
| R2 | Removing legacy codestyle-0002 breaks legacy testsuite | Scope boundary | Update `testsuite/lint.lisp` and legacy module in same step |
| R3 | Line reading may differ across platforms (CRLF vs LF) | Portability | Use `read-line` which handles both |
| R4 | Mixed indentation flags Makefile-style files | Scope boundary | Default `:spaces`; document that Makefile projects configure `:tabs` |

---

## OSS Components

None — trivial string operations.

---

## Phase Scope

**Must deliver:** S1 (trailing whitespace), S2 (line length), S3 (mixed indentation), S4 (indentation-style config), S5 (remove codestyle-0002).

**Deferred:** None.

---

## File Organisation

```
src/
├── package.lisp                              [modify] — new exports
├── finding.lisp                              [modify] — add 3 finding subclasses
├── asdf.lisp                                 [modify] — add indentation-style slot, export *current-*
├── runner.lisp                               [modify] — bind *current-* vars, include line-inspector
├── inspector.lisp                            [modify] — refactor define-inspector macros, ignorable inspector, no project-config param
├── inspectors/
│   ├── check-file-encoding.lisp              [modify] — update to 2-arg inspect-file
│   ├── check-spdx-license-header.lisp        [modify] — update to 2-arg inspect-file
│   ├── check-trailing-whitespace.lisp        [new] — S1
│   ├── check-line-length.lisp                [new] — S2
│   └── check-mixed-indentation.lisp          [new] — S3
└── legacy/
    ├── inspector/codestyle-0002.lisp          [delete] — S5
    └── lint.lisp                             [modify] — remove codestyle-0002 reference

testsuite/
├── inspectors/
│   ├── check-file-encoding.lisp              [modify] — update for 2-arg
│   ├── check-spdx-license-header.lisp        [modify] — update for 2-arg
│   ├── check-trailing-whitespace.lisp        [new] — S1 tests
│   ├── check-line-length.lisp                [new] — S2 tests
│   └── check-mixed-indentation.lisp          [new] — S3 tests
├── asdf.lisp                                 [modify] — indentation-style test
├── runner.lisp                               [modify] — update for 2-arg
├── inspector/codestyle-0002.lisp             [delete] — S5
├── lint.lisp                                 [modify] — remove codestyle-0002 test reference
├── entrypoint.lisp                           [modify] — add new test groups
└── fixtures/
    ├── trailing-whitespace.lisp              [new]
    ├── long-lines.lisp                       [new]
    ├── mixed-indentation-spaces.lisp         [new]
    └── mixed-indentation-tabs.lisp           [new]

org.melusina.atelier.asd                      [modify] — add new inspector files, remove codestyle-0002
```

---

## Build System Changes

### `org.melusina.atelier` — inspectors module

Add: `check-trailing-whitespace`, `check-line-length`, `check-mixed-indentation`.

### `org.melusina.atelier/legacy` — inspector module

Remove: `codestyle-0002`.

### `org.melusina.atelier/testsuite` — inspectors module

Add: `check-trailing-whitespace`, `check-line-length`, `check-mixed-indentation`.
Remove: `codestyle-0002` from legacy-inspector module.

---

## Package / Module Architecture

### New exports from `atelier`

**Finding subclasses:**
`trailing-whitespace-finding`, `line-too-long-finding`, `mixed-indentation-finding`

**Concrete inspectors:**
`check-trailing-whitespace`, `check-line-length`, `check-mixed-indentation`

**Configuration:**
`linter-configuration-indentation-style`, `*current-project-configuration*`, `*current-linter-configuration*`

### Modified exports

`inspect-file` — signature changes from `(inspector pathname project-configuration)` to `(inspector pathname)`.

---

## Type / Class Hierarchy

### New finding subclasses

```
line-finding
├── trailing-whitespace-finding    [concrete]
├── line-too-long-finding          [concrete]
└── mixed-indentation-finding      [concrete]
```

### Inspector instances (all line-inspector subclasses)

```
line-inspector
├── check-trailing-whitespace      [concrete singleton]
├── check-line-length              [concrete singleton]
└── check-mixed-indentation        [concrete singleton]
```

---

## Protocol Definitions

```lisp
(defgeneric inspect-file (inspector pathname)
  (:documentation "Run INSPECTOR on the file at PATHNAME.
Return a list of findings or NIL. *CURRENT-PROJECT-CONFIGURATION* and
*CURRENT-LINTER-CONFIGURATION* are bound by the runner before this is called."))
```

No new generics.

---

## Error / Condition Types

None new.

---

## Test Plan

| Story | Test name | Category | Skip condition |
|-------|-----------|:--------:|----------------|
| S1 | `validate-check-trailing-whitespace-clean` | slow | Fixture not found |
| S1 | `validate-check-trailing-whitespace-dirty` | slow | Fixture not found |
| S1 | `validate-check-trailing-whitespace-registered` | fast | — |
| S2 | `validate-check-line-length-short` | slow | Fixture not found |
| S2 | `validate-check-line-length-long` | slow | Fixture not found |
| S2 | `validate-check-line-length-skips-definitions` | slow | Fixture not found |
| S2 | `validate-check-line-length-registered` | fast | — |
| S3 | `validate-check-mixed-indentation-clean` | slow | Fixture not found |
| S3 | `validate-check-mixed-indentation-tabs` | slow | Fixture not found |
| S3 | `validate-check-mixed-indentation-registered` | fast | — |
| S4 | `validate-indentation-style-configuration` | fast | — |
| S5 | `validate-legacy-codestyle-0002-removed` | fast | — |

---

## Implementation Order (Step Table)

| Step | File | Action | Form(s) | Test name | Category |
|------|------|--------|---------|-----------|:--------:|
| 1 | `src/finding.lisp` [modify] | Add finding subclasses | `trailing-whitespace-finding`, `line-too-long-finding`, `mixed-indentation-finding` | — | — |
| 2 | `src/asdf.lisp` [modify] | Add `indentation-style` slot to `linter-configuration`, export `*current-project-configuration*` and `*current-linter-configuration*` | `linter-configuration-indentation-style` | — | — |
| 3 | `src/inspector.lisp` [modify] | Refactor `define-inspector` / convenience macros: `inspector` declared `ignorable`, `inspect-file` method takes 2 args, macro asserts `*current-project-configuration*` and `*current-linter-configuration*` are bound | `define-inspector`, `define-file-inspector`, `define-line-inspector` | — | — |
| 4 | `src/runner.lisp` [modify] | Bind `*current-project-configuration*` and `*current-linter-configuration*`, include `line-inspector` in dispatch, update `inspect-file` calls to 2 args | `run-file-inspectors` | — | — |
| 5 | `src/inspectors/check-file-encoding.lisp` [modify] | Update to 2-arg `inspect-file`, use `*current-project-configuration*` | — | — | — |
| 6 | `src/inspectors/check-spdx-license-header.lisp` [modify] | Update to 2-arg `inspect-file`, use `*current-project-configuration*` | — | — | — |
| 7 | `src/inspectors/check-trailing-whitespace.lisp` [new] | Implement | `check-trailing-whitespace`, `inspect-file` method | — | — |
| 8 | `src/inspectors/check-line-length.lisp` [new] | Implement | `check-line-length`, `inspect-file` method | — | — |
| 9 | `src/inspectors/check-mixed-indentation.lisp` [new] | Implement | `check-mixed-indentation`, `inspect-file` method | — | — |
| 10 | `src/package.lisp` [modify] | Add new exports | — | — | — |
| 11 | `org.melusina.atelier.asd` [modify] | Add new inspector files, remove legacy codestyle-0002 | — | — | — |
| 12 | `src/legacy/inspector/codestyle-0002.lisp` [delete] | Superseded | — | — | — |
| 13 | `src/legacy/lint.lisp` [modify] | Remove codestyle-0002 reference | — | — | — |
| 14 | `testsuite/fixtures/trailing-whitespace.lisp` [new] | Fixture with trailing spaces | — | — | — |
| 15 | `testsuite/fixtures/long-lines.lisp` [new] | Fixture with long lines, definitions, single-word lines | — | — | — |
| 16 | `testsuite/fixtures/mixed-indentation-spaces.lisp` [new] | Fixture with tabs in spaces-only file | — | — | — |
| 17 | `testsuite/fixtures/mixed-indentation-tabs.lisp` [new] | Fixture with spaces in tabs-only file | — | — | — |
| 18 | `testsuite/inspectors/check-file-encoding.lisp` [modify] | Update for 2-arg inspect-file | — | — | — |
| 19 | `testsuite/inspectors/check-spdx-license-header.lisp` [modify] | Update for 2-arg inspect-file | — | — | — |
| 20 | `testsuite/runner.lisp` [modify] | Update for 2-arg, test line-inspector inclusion | — | — | — |
| 21 | `testsuite/asdf.lisp` [modify] | Add indentation-style test | `validate-indentation-style-configuration` | fast |
| 22 | `testsuite/inspectors/check-trailing-whitespace.lisp` [new] | S1 tests | `validate-check-trailing-whitespace-*` | slow |
| 23 | `testsuite/inspectors/check-line-length.lisp` [new] | S2 tests | `validate-check-line-length-*` | slow |
| 24 | `testsuite/inspectors/check-mixed-indentation.lisp` [new] | S3 tests | `validate-check-mixed-indentation-*` | slow |
| 25 | `testsuite/inspector/codestyle-0002.lisp` [delete] | Superseded | — | — | — |
| 26 | `testsuite/lint.lisp` [modify] | Remove codestyle-0002 test reference | — | — | — |
| 27 | `testsuite/entrypoint.lisp` [modify] | Add new test groups | — | — | — |

---

## Invariants

| # | Invariant |
|---|-----------|
| I1–I23 | Carried forward from slices 001–002 |
| I24 | `inspect-file` takes 2 arguments: `(inspector pathname)`. Configuration is accessed via `*current-project-configuration*` and `*current-linter-configuration*` special variables. |
| I25 | `define-file-inspector` / `define-line-inspector` / etc. declare `inspector` as `ignorable` in the generated method. |
| I26 | The runner binds `*current-project-configuration*` and `*current-linter-configuration*` before calling any inspector. |
| I27 | Line-level inspectors are `line-inspector` subclasses. The runner includes both `file-inspector` and `line-inspector` instances. |
| I28 | Default indentation style is `:spaces`. Configurable via `linter-configuration` `:indentation-style` slot. |

---

## Test Fixtures

**`testsuite/fixtures/trailing-whitespace.lisp`** — a well-formed Lisp file with trailing spaces on specific lines. Must follow canonical header/footer format.

**`testsuite/fixtures/long-lines.lisp`** — a well-formed Lisp file with: a normal line, a line > 100 chars, a `(defun ...)` line > 100 chars (should be skipped), a single-word line > 100 chars (should be skipped).

**`testsuite/fixtures/mixed-indentation-spaces.lisp`** — a file using spaces for indentation with one line using a tab.

**`testsuite/fixtures/mixed-indentation-tabs.lisp`** — a file using tabs for indentation with one line using spaces.

All fixtures must follow canonical project format (header, footer, license block) to avoid legacy linter complaints.

---

## References to Create

None.

---

## Acceptance Criteria

| # | Criterion | Verification |
|---|-----------|-------------|
| AC1 | `check-trailing-whitespace` produces `trailing-whitespace-finding` for lines with trailing spaces | `validate-check-trailing-whitespace-dirty` passes |
| AC2 | `check-line-length` produces `line-too-long-finding`, skipping definitions and single-word lines | `validate-check-line-length-long` and `validate-check-line-length-skips-definitions` pass |
| AC3 | `check-mixed-indentation` produces `mixed-indentation-finding` for tabs in spaces-default file | `validate-check-mixed-indentation-tabs` passes |
| AC4 | `linter-configuration` `:indentation-style` is respected | `validate-indentation-style-configuration` passes |
| AC5 | Legacy codestyle-0002 removed, full suite passes | `run-all-tests` passes, codestyle-0002 files absent |
| AC6 | `inspect-file` takes 2 args, existing inspectors updated | `validate-check-file-encoding-*` and `validate-check-spdx-header-*` pass with new signature |
| AC7 | Runner includes both file-inspector and line-inspector | `run-file-inspectors` returns findings from both levels |
| AC8 | ≥ 3 line-level inspectors registered | `(length (remove-if-not ...))` ≥ 3 |
| AC9 | `atelier/development:lint` completes without error | Manual verification |

---

## Phase Closure Conditions

1. All 9 acceptance criteria verified.
2. All fast tests pass.
3. All slow tests pass (fixture files present from source tree).
4. `(asdf:load-system "org.melusina.atelier")` and `(asdf:load-system "org.melusina.atelier/legacy")` succeed.
5. `(atelier/testsuite:run-all-tests)` passes.
6. No SBCL-specific code without `#+sbcl` guard.
