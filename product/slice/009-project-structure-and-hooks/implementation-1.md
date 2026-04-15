# Implementation Phase 1: Slice 009 — Project Structure and Hooks

**Phase:** 1
**Slice:** product/slice/009-project-structure-and-hooks/slice.md
**Scope:** All six stories (S1–S6).

## Prior Phases

None — first phase.

## Project Knowledge Applied

- **INV-2:** Per-maintainer self-idempotency at N=1 is required.
- **INV-6:** One finding subclass per inspector (CLOS dispatch).
- **INV-10:** Autofix is opt-in.
- **Pattern: Hallucinated requirements** — every slot on a new class
  must trace to a specific acceptance criterion.
- **Pattern: Wrong dispatch mechanism** — acceptance criteria must
  name the dispatch mechanism explicitly.

## Risk Register

| # | Risk | Category | Mitigation |
|---|------|----------|-----------|
| R1 | Pre-commit hook portability across macOS/Linux shells | Portability | Use POSIX sh, test with `#!/bin/sh`. |
| R2 | ASDF system tree walking for component comparison | Library API | Use documented `asdf:component-children` and `asdf:component-name`. |
| R3 | File rename during autofix might break references | Scope boundary | Rename is a file-level operation; cross-file reference update is out of scope for this slice (per CLAUDE.md: refactoring is a separate protocol). |
| R4 | Detecting the "main system" from an `.asd` file | Phase boundary | Convention: the first `defsystem` in the `.asd` file whose name has no `/` separator is the main system. |

## OSS Components

None — no new dependencies.

## Phase Scope

**Covered:** S1 (pre-commit hook), S2 (system naming inspector),
S3 (file naming inspector), S4 (test mirror inspector),
S5 (deprecated system name maintainer), S6 (deprecated component name
maintainer).

**Deferred:** Nothing.

## File Organisation

```
src/
├── inspectors/
│   ├── check-system-naming.lisp     [new] — S2, S3
│   └── check-test-mirror.lisp      [new] — S4
├── maintainers/
│   └── fix-deprecated-names.lisp   [new] — S5, S6
├── git.lisp                         [new] — S1
├── package.lisp                     [modify] — new exports
├── finding.lisp                     [modify] — new finding subclasses

testsuite/
├── inspectors/
│   └── check-system-naming.lisp    [new] — tests for S2, S3, S4
├── maintainers/
│   └── fix-deprecated-names.lisp   [new] — tests for S5, S6
├── git.lisp                         [new] — tests for S1
├── entrypoint.lisp                  [modify] — call new test suites

org.melusina.atelier.asd             [modify] — new components
```

## Build System Changes

In `org.melusina.atelier.asd`, the main system's `src` module gains:
- `(:file "git")` after `(:file "main")` (S1 only needs the package)
- `(:file "check-system-naming")` in the `inspectors` module
- `(:file "check-test-mirror")` in the `inspectors` module
- `(:file "fix-deprecated-names")` in the `maintainers` module

The testsuite system gains mirror components.

## Package / Module Architecture

New exports from `#:atelier`:

```
;; Git hooks (S1)
#:install-pre-commit-hook
#:pre-commit-hook-exists           ; condition

;; Finding subclasses (S2, S3, S4)
#:non-canonical-system-name-finding
#:deprecated-system-name-finding
#:deprecated-component-name-finding
#:missing-test-component-finding
#:test-component-order-finding

;; Inspectors (S2, S3, S4)
#:check-system-naming
#:check-test-mirror

;; Maintainers (S5, S6)
#:fix-deprecated-system-name
#:fix-deprecated-component-name
```

## Type / Class Hierarchy

All new finding subclasses inherit from `file-finding`:

```
file-finding
├── non-canonical-system-name-finding    (S2 — unrecognised system suffix)
├── deprecated-system-name-finding       (S2 — /testsuite → /test)
├── deprecated-component-name-finding    (S3 — entrypoint → entry-point, etc.)
├── missing-test-component-finding       (S4 — component in main but not in test)
└── test-component-order-finding         (S4 — test components out of order)
```

File-finding level is correct because these inspectors operate on the
`.asd` file as a whole (examining `defsystem` structure), not on
individual lines or CST forms within a source file.

## Protocol Definitions

### `install-pre-commit-hook` (S1)

```lisp
(defun install-pre-commit-hook (git-repository-pathname
                                &key system-name)
  "Install a git pre-commit hook at GIT-REPOSITORY-PATHNAME.
When SYSTEM-NAME is not supplied, it is derived from the .asd file
in the repository root."
  ...)
```

### `check-system-naming` (S2, S3)

A file-inspector that reads the `.asd` file, parses `defsystem` forms,
and checks system names and component names against conventions.

```lisp
(define-file-inspector check-system-naming ((pathname pathname))
  "Check that ASDF system and component names follow conventions."
  ...)
```

### `check-test-mirror` (S4)

A file-inspector that reads the `.asd` file and compares the main
system's component tree with the test system's component tree.

```lisp
(define-file-inspector check-test-mirror ((pathname pathname))
  "Check that the test system mirrors the main system's component structure."
  ...)
```

### Maintainers (S5, S6)

```lisp
(define-automatic-maintainer fix-deprecated-system-name
    ((finding deprecated-system-name-finding))
  "Rename a deprecated system name in the .asd file."
  ...)

(define-automatic-maintainer fix-deprecated-component-name
    ((finding deprecated-component-name-finding))
  "Rename a deprecated component name in the .asd file and on disk."
  ...)
```

## Error / Condition Types

| Name | Superclass | Slots | When signalled |
|------|-----------|-------|----------------|
| `pre-commit-hook-exists` | `warning` | `pathname` | `install-pre-commit-hook` when `.git/hooks/pre-commit` already exists |

Restarts on `pre-commit-hook-exists`:
- `replace-hook` — overwrite the existing hook
- `keep-existing-hook` — do nothing (default if unhandled)

## Test Plan

| Story | Test name | Category | Skip condition |
|-------|-----------|:--------:|----------------|
| S1 | `test-install-pre-commit-hook` | slow | — (uses temp dir) |
| S1 | `test-install-pre-commit-hook-no-git` | fast | — |
| S1 | `test-install-pre-commit-hook-exists` | slow | — (uses temp dir) |
| S1 | `test-install-pre-commit-hook-replace` | slow | — (uses temp dir) |
| S2 | `test-check-system-naming-canonical` | fast | — |
| S2 | `test-check-system-naming-non-canonical` | fast | — |
| S2 | `test-check-system-naming-deprecated` | fast | — |
| S3 | `test-check-component-naming-deprecated` | fast | — |
| S3 | `test-check-component-naming-clean` | fast | — |
| S4 | `test-check-test-mirror-missing` | fast | — |
| S4 | `test-check-test-mirror-order` | fast | — |
| S4 | `test-check-test-mirror-clean` | fast | — |
| S4 | `test-check-test-mirror-excludes-config` | fast | — |
| S5 | `test-fix-deprecated-system-name` | fast | — |
| S5 | `test-fix-deprecated-system-name-idempotent` | fast | — |
| S6 | `test-fix-deprecated-component-name` | fast | — |
| S6 | `test-fix-deprecated-component-name-idempotent` | fast | — |

## Implementation Order (Step Table)

| Step | File | Action | Form(s) | Test name | Category |
|------|------|--------|---------|-----------|:--------:|
| 1 | `src/finding.lisp` | modify | Add 5 finding subclasses via `define-findings` | — | — |
| 2 | `src/package.lisp` | modify | Add all new exports | — | — |
| 3 | `org.melusina.atelier.asd` | modify | Add new source and test components | — | — |
| 4 | `src/git.lisp` | new | `install-pre-commit-hook`, `pre-commit-hook-exists` | `test-install-pre-commit-hook` | slow |
| 5 | `src/inspectors/check-system-naming.lisp` | new | `check-system-naming` inspector | `test-check-system-naming-*` | fast |
| 6 | `src/inspectors/check-test-mirror.lisp` | new | `check-test-mirror` inspector | `test-check-test-mirror-*` | fast |
| 7 | `src/maintainers/fix-deprecated-names.lisp` | new | `fix-deprecated-system-name`, `fix-deprecated-component-name` | `test-fix-deprecated-*` | fast |
| 8 | `testsuite/git.lisp` | new | All S1 tests | — | — |
| 9 | `testsuite/inspectors/check-system-naming.lisp` | new | All S2, S3, S4 tests | — | — |
| 10 | `testsuite/maintainers/fix-deprecated-names.lisp` | new | All S5, S6 tests | — | — |
| 11 | `testsuite/entrypoint.lisp` | modify | Call new test suites | — | — |
| 12 | Full test suite | verify | `(asdf:test-system "org.melusina.atelier")` | — | — |

## Invariants

Prior invariants confirmed:
- INV-1 through INV-11: all apply unchanged.

New invariants:
- INV-12: The system naming inspector operates on `.asd` files only. It
  uses `file-inspector` level, dispatching on `(string-equal "asd" (pathname-type pathname))`.
- INV-13: Deprecated name maintainers produce `text-resolution` (not
  `syntax-resolution`) because they operate at the file level, replacing
  text spans in the `.asd` file.
- INV-14: The pre-commit hook is a self-contained POSIX shell script
  with no dependency on Atelier being loadable via Quicklisp — it
  calls `sbcl --non-interactive --load <asd-file> --eval ...`.

## Test Fixtures

No auto-discovered fixtures for this slice. The system-naming inspector
operates on `.asd` files, not Lisp source files, so the autofix-cycle
fixture format does not apply. Tests use in-memory strings written to
temporary `.asd` files.

## References to Create

None.

## Acceptance Criteria

| # | Criterion | Mapped to |
|---|-----------|-----------|
| AC1 | `(install-pre-commit-hook <temp-git-dir>)` creates `.git/hooks/pre-commit` with mode `#o755` | S1 |
| AC2 | The hook script runs `sbcl --non-interactive` with `lint-system :autofix t` and `test-system` | S1 |
| AC3 | Re-calling `install-pre-commit-hook` on an existing hook signals `pre-commit-hook-exists` with `replace-hook` and `keep-existing-hook` restarts | S1 |
| AC4 | Calling on a directory without `.git/` signals an error | S1 |
| AC5 | An `.asd` file with a non-canonical secondary system name produces `non-canonical-system-name-finding` | S2 |
| AC6 | An `.asd` file with `MAIN/TESTSUITE` produces `deprecated-system-name-finding` | S2 |
| AC7 | Components named `entrypoint`, `main`, or `testsuite` produce `deprecated-component-name-finding` | S3 |
| AC8 | A test system missing a component from the main system produces `missing-test-component-finding` | S4 |
| AC9 | A test system with components in wrong order produces `test-component-order-finding` | S4 |
| AC10 | `fix-deprecated-system-name` rewrites `MAIN/TESTSUITE` to `MAIN/TEST` in the `.asd` file and is self-idempotent | S5 |
| AC11 | `fix-deprecated-component-name` rewrites `entrypoint` to `entry-point` (etc.) in the `.asd` file and is self-idempotent | S6 |
| AC12 | Full test suite passes: `(asdf:test-system "org.melusina.atelier")` | All |

## Phase Closure Conditions

- All 12 acceptance criteria verified.
- All fast and slow tests passing.
- Full test suite passes in a fresh SBCL subprocess (INV-4).
- No regressions in existing inspectors or maintainers.
