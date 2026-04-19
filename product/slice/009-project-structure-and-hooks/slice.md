# Slice 009: Project Structure and Hooks

**Status:** Complete
**Type:** New capability
**Goal addressed:** G2 (linter coverage)
**Backlog items:** #16, #17
**Planned start / end:** 2026-04-14 / 2026-04-18
**Actual end:** 2026-04-14
**Implementation phases:**
  - Phase 1: product/slice/009-project-structure-and-hooks/implementation-1.md — Complete

---

## What changes for users

After this slice, a user can:

1. Call `(atelier:install-pre-commit-hook #p"/path/to/repo/")` and get a
   git pre-commit hook that runs the linter with autofix and the test
   suite before every commit. The hook is a simple shell script.

2. Run the linter on a project and receive findings when:
   - An ASDF system in the `.asd` file does not follow the canonical
     naming convention (`MAIN-SYSTEM`, `MAIN-SYSTEM/TEST`,
     `MAIN-SYSTEM/DEVELOPMENT`, `MAIN-SYSTEM/OPERATION`,
     `MAIN-SYSTEM/EXPERIMENT`).
   - A system named `MAIN-SYSTEM/TESTSUITE` exists (should be `/TEST`).
   - A file is named `entrypoint` or `main` (should be `entry-point`).
   - A file is named `testsuite` (should be `test`).
   - The test system's component structure does not mirror the main
     system's structure (same components, same names, same order),
     excluding non-Lisp components like project/linter configuration.

3. Run `(atelier:lint-system "my-system" :autofix t)` and have the
   naming maintainers automatically rename `MAIN-SYSTEM/TESTSUITE`
   to `MAIN-SYSTEM/TEST` in the `.asd` file, and rename files
   `entrypoint` to `entry-point`, `main` to `entry-point`, and
   `testsuite` to `test`.

## Specification references

- Git hooks: https://git-scm.com/docs/githooks#_pre_commit
- ASDF system naming: ASDF manual, "The Object model / System"

## Stories

### S1: Pre-commit hook installer

**In order to** enforce linter and test discipline automatically, **a**
Common Lisp developer **can** call `(atelier:install-pre-commit-hook
#p"/path/to/repo/")` and get a working git pre-commit hook.

**Acceptance criteria:**
- Given a directory with a `.git/` subdirectory, when
  `install-pre-commit-hook` is called, then a file
  `.git/hooks/pre-commit` is created with mode `#o755`.
- Given the hook is installed, the script runs `sbcl --non-interactive`
  loading the project's ASDF system, running
  `(atelier:lint-system "SYSTEM" :autofix t)`, and running
  `(asdf:test-system "SYSTEM")`.
- Given `.git/hooks/pre-commit` already exists, the function signals a
  `pre-commit-hook-exists` warning with a `replace-hook` restart and a
  `keep-existing-hook` restart. The default (no handler) keeps the
  existing hook.
- Given the directory has no `.git/` subdirectory, the function signals
  an error.

### S2: System naming inspector

**In order to** maintain canonical ASDF system structure, **a** Common
Lisp developer **can** run the linter and receive warnings for
non-canonical system names in the `.asd` file.

**Acceptance criteria:**
- Given an `.asd` file with `(defsystem "my-project/foo")` where `foo`
  is not one of `test`, `development`, `operation`, `experiment`, when
  the linter runs, then a `non-canonical-system-name` finding is
  produced with severity `:warning`.
- Given an `.asd` file with `(defsystem "my-project/test")`, when
  the linter runs, then a `deprecated-system-name` finding is produced
  recommending rename to `my-project/test`.
- Given an `.asd` file with only canonical system names, when the linter
  runs, then no system-naming findings are produced.

### S3: File naming inspector

**In order to** maintain canonical file naming, **a** Common Lisp
developer **can** run the linter and receive warnings for files named
`entrypoint`, `main`, or `testsuite`.

**Acceptance criteria:**
- Given an ASDF component named `"entrypoint"` or `"main"`, when the
  linter runs, then a `deprecated-component-name` finding is produced
  recommending rename to `"entry-point"`.
- Given an ASDF component named `"testsuite"`, when the linter runs,
  then a `deprecated-component-name` finding is produced recommending
  rename to `"test"`.
- Given only canonical component names, no findings are produced.

### S4: Test mirror inspector

**In order to** keep test structure aligned with source structure, **a**
Common Lisp developer **can** run the linter and receive warnings when
the test system's component tree does not mirror the main system's.

**Acceptance criteria:**
- Given a main system with components `(a b c)` and a test system with
  components `(a c)` (missing `b`), when the linter runs, then a
  `missing-test-component` finding is produced naming `b`.
- Given a main system with components `(a b c)` and a test system with
  components `(b a c)` (wrong order), when the linter runs, then a
  `test-component-order` finding is produced.
- Given non-Lisp ASDF components (e.g., `asdf-project-configuration`,
  `asdf-linter-configuration`, static files), these are excluded from
  the mirror comparison.
- Given matching component structure, no findings are produced.

### S5: Deprecated system name maintainer

**In order to** automatically fix deprecated system names, **a** Common
Lisp developer **can** run `lint-system :autofix t` and have
`MAIN-SYSTEM/TESTSUITE` renamed to `MAIN-SYSTEM/TEST` in the `.asd`
file.

**Acceptance criteria:**
- Given a `deprecated-system-name` finding for `my-project/test`,
  when autofix runs, then the `defsystem` form in the `.asd` file is
  rewritten with the name `"my-project/test"`.
- The maintainer is self-idempotent at N=1.

### S6: Deprecated component name maintainer

**In order to** automatically fix deprecated file names, **a** Common
Lisp developer **can** run `lint-system :autofix t` and have components
named `entrypoint`, `main`, or `testsuite` renamed.

**Acceptance criteria:**
- Given a `deprecated-component-name` finding for `"entrypoint"`, when
  autofix runs, then the component name in the `.asd` file is rewritten
  to `"entry-point"` and the corresponding `.lisp` file is renamed on
  disk.
- Given a `deprecated-component-name` finding for `"testsuite"`, when
  autofix runs, then the component name is rewritten to `"test"` and
  the file is renamed.
- The maintainer is self-idempotent at N=1.

## Quality Criteria

- [ ] All new inspectors produce findings that follow the existing
      finding class hierarchy and carry `observation` and `rationale`.
- [ ] All new maintainers are self-idempotent at N=1, verified by
      autofix-cycle fixtures or explicit testcases.
- [ ] The pre-commit hook script is portable across macOS and Linux
      (POSIX sh, no bashisms).
- [ ] Full test suite passes: `(asdf:test-system "org.melusina.atelier")`.

## Definition of Ready

- [x] Stories traceable to backlog items
- [x] Stories sized ≤ 2 days each
- [x] Acceptance criteria written
- [x] Quality criterion defined
- [x] Spec references identified

## Definition of Done

- [ ] All stories complete with acceptance criteria passing
- [ ] Quality criteria passing
- [ ] Full test suite passes
- [ ] All implementation phases have completion notes
- [ ] `product/slice/009-project-structure-and-hooks/retrospective.md` created
- [ ] `product/backlog.md` updated
- [ ] `product/roadmap.md` updated
