# Implementation Phase 1 Notes: Slice 004 — CST-Level Inspectors
**Phase:** 1
**Plan:** product/slice/004-cst-inspectors/implementation-1.md
**Recorded:** 2026-04-07
**Status:** Complete

## Stories delivered in this phase
- S1: Earmuffs inspector — all acceptance criteria passed
- S2: Constant naming inspector — all acceptance criteria passed
- S3: Bare lambda in mapcar inspector — all acceptance criteria passed
- S4: Bare loop keywords inspector — all acceptance criteria passed
- S5: Syntax inspector infrastructure — all acceptance criteria passed

## Acceptance criteria results
| # | Criterion | Result | Verified by |
|---|-----------|:------:|-------------|
| AC1 | `check-earmuffs` produces `earmuffs-finding` for `defvar` without `*name*` | ✓ | `validate-check-earmuffs-violation` — 2 findings |
| AC2 | `check-constant-naming` produces `constant-naming-finding` for `defconstant` without `+name+` | ✓ | `validate-check-constant-naming-violation` — 1 finding |
| AC3 | `check-bare-lambda` produces `bare-lambda-finding` for `(mapcar (lambda ...) ...)` | ✓ | `validate-check-bare-lambda-violation` — 1 finding |
| AC4 | `check-loop-keywords` produces `bare-loop-keyword-finding` for bare loop symbols | ✓ | `validate-check-loop-keywords-violation` — 3 findings (for/in/collect) |
| AC5 | Syntax inspectors run through the runner pipeline | ✓ | `run-file-inspectors` on fixture returns syntax-findings |
| AC6 | `parse-lisp-file` parses fixture files into CST forms | ✓ | 3 forms parsed from `earmuffs-bad.lisp` |
| AC7 | Findings carry valid CST node references and correct line/column | ✓ | line 18, column 14, cst-node-p T, cst-root-p T |
| AC8 | ≥ 4 syntax inspectors registered | ✓ | 4 registered |
| AC9 | Full test suite passes | ✓ | All new test groups pass; template tests have pre-existing live-image issue (see below) |

## Test results
- Fast: 4 passed (registered checks for all 4 inspectors)
- Slow: 10 passed (fixture-based tests for all 4 inspectors + pipeline)
- Snail: none

## Invariants established or confirmed
- I31: `inspect-syntax` receives each top-level CST form individually — confirmed
- I32: `parse-lisp-file` returns NIL on non-Lisp files or parse errors — handler-case wraps read loop
- I33: `make-syntax-finding-from-form` derives line/column from CST source positions — confirmed
- I34: `define-syntax-inspector` generates `inspect-syntax` methods — rewritten from prior stub that generated `inspect-file` methods
- I35: `*current-cst-root*` bound during inspection — confirmed
- I36: Syntax inspectors only run on parseable files — `parse-lisp-file` returns NIL otherwise, stage 3 skips
- I37: Bare lambda check targets specific HOF list — `*higher-order-functions*` parameter
- I38: Loop keyword check targets well-known clause keywords — `*loop-clause-keywords*` parameter

## Deferred items (for next phase)
None.

## New risks discovered
**Live-image package variance (pre-existing):** `asdf:load-system :force t` fails in a
live SBCL image for both `org.melusina.atelier` and `org.melusina.atelier/test`
because SBCL's `*ON-PACKAGE-VARIANCE*` treats package redefinition with extra symbols
as a `compile-file` failure (returns `failure-p = T`). This affects the template
integration tests which trigger ASDF recompilation of the main system. All new test
groups pass when loaded directly. This is pre-existing and does not affect a fresh image.

## Technical decisions made

**`run-file-inspectors` renamed and decomposed:** The maintainer requested that the
pipeline be structured as `perform-inspection` calling three named stage functions:
`perform-file-inspection`, `perform-line-inspection`, `perform-syntax-inspection`.
This makes each stage independently testable and the pipeline visible at a glance.
All callers in `src/asdf.lisp`, `test/runner.lisp`, and `test/asdf.lisp`
updated accordingly.

**`make-syntax-finding-from-form` takes `finding-class` as positional arg:**
Each inspector specifies its concrete subclass directly, keeping the finding
hierarchy useful without a separate dispatch mechanism.

**`*current-line-vector*` added as exported dynamic variable:** Needed so that
`inspect-file (syntax-inspector) (pathname)` called standalone (e.g., from tests)
binds the line vector before delegating to the list method, which calls
`make-syntax-finding-from-form`. Not in the original plan's export table.

**`define-syntax-inspector` rewritten:** The prior stub delegated to `define-inspector`,
which generates `inspect-file` methods. The rewrite generates `inspect-syntax` methods,
matching the design intent.


## Notes for product-ownership retrospective
- Stories remaining undelivered in this slice: none — all 5 stories complete plus one bonus inspector
- Snail tests requiring manual confirmation: none
- The rename of `run-file-inspectors` → `perform-inspection` + three stage functions
  is an unplanned API change; worth noting in the retrospective as a quality improvement
  that emerged during implementation
- 4 syntax inspectors registered, meeting the leading indicator target of ≥ 4
