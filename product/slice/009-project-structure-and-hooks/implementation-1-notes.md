# Implementation Phase 1 Notes: Slice 009 — Project Structure and Hooks

**Phase:** 1
**Plan:** product/slice/009-project-structure-and-hooks/implementation-1.md
**Recorded:** 2026-04-14
**Status:** Complete

## Stories delivered in this phase

- S1 (Pre-commit hook installer) — acceptance criteria: all passed
- S2 (System naming inspector) — acceptance criteria: all passed
- S3 (File naming inspector) — acceptance criteria: all passed
- S4 (Test mirror inspector) — acceptance criteria: all passed
- S5 (Deprecated system name maintainer) — acceptance criteria: all passed
- S6 (Deprecated component name maintainer) — acceptance criteria: all passed

## Acceptance criteria results

| # | Criterion | Result | Verified by |
|---|-----------|:------:|-------------|
| AC1 | install-pre-commit-hook creates .git/hooks/pre-commit with mode #o755 | Pass | test-install-pre-commit-hook |
| AC2 | Hook script runs sbcl with lint-system :autofix t and test-system | Pass | test-install-pre-commit-hook (search assertions) |
| AC3 | Re-calling signals pre-commit-hook-exists with restarts | Pass | test-install-pre-commit-hook-exists, test-install-pre-commit-hook-replace |
| AC4 | No .git/ signals error | Pass | test-install-pre-commit-hook-no-git |
| AC5 | Non-canonical system suffix produces non-canonical-system-name-finding | Pass | test-check-system-naming-non-canonical |
| AC6 | /test produces deprecated-system-name-finding | Pass | test-check-system-naming-deprecated |
| AC7 | entrypoint/main/test produce deprecated-component-name-finding | Pass | test-check-component-naming-deprecated |
| AC8 | Missing test component produces missing-test-component-finding | Pass | test-check-test-mirror-missing |
| AC9 | Wrong order produces test-component-order-finding | Pass | test-check-test-mirror-order |
| AC10 | fix-deprecated-system-name rewrites and is idempotent | Pass | test-fix-deprecated-system-name, test-fix-deprecated-system-name-idempotent |
| AC11 | fix-deprecated-component-name rewrites and is idempotent | Pass | test-fix-deprecated-component-name, test-fix-deprecated-component-name-idempotent |
| AC12 | Full test suite passes | Pass | 435/435 |

## Test results

- Fast: 26 passed (14 system-naming, 12 maintainers)
- Slow: 10 passed (git hooks)
- Total new: 36 assertions
- Full suite: 435/435 (0 regressions)

## Invariants established or confirmed

- INV-12: The system naming inspector operates on .asd files only,
  dispatching on `(string-equal "asd" (pathname-type pathname))`.
- INV-13: Deprecated name findings are line-finding subclasses (not
  file-finding as originally planned) because text-resolution requires
  line/column positions for the write-back engine.
- INV-14: The pre-commit hook is a self-contained POSIX shell script
  with no dependency on Quicklisp — it calls sbcl --non-interactive
  --load <asd-file> --eval ...

## Deferred items

None — all stories delivered.

## Reworks performed

- Finding hierarchy: changed from file-finding to line-finding for
  deprecated-system-name-finding, deprecated-component-name-finding,
  and non-canonical-system-name-finding. Trigger: text-resolution
  write-back engine requires finding-line/finding-column. Impact: minor,
  caught during first test run.
- Pre-existing issue: removed stale fresh-sbcl-load.lisp reference
  from the testsuite ASDF system definition (leftover from MCP
  extraction).

## New risks discovered

None.

## Technical decisions made

- Maintainers replace the finding's source-text line (not the whole
  file content) via text-resolution. This keeps the resolution span
  minimal and compatible with the write-back engine.
- System naming inspector reads .asd files with *read-eval* NIL and
  *package* KEYWORD for safety, then locates defsystem lines by
  searching file content.
- fix-deprecated-component-name is marked :experimental maturity since
  it only renames in the .asd file, not on disk.

## Notes for Strategist retrospective

- Stories remaining: none
- Snail tests requiring confirmation: none
- CLAUDE.md updated: inspector count 15→17, maintainer count 10→12
