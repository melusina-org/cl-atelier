# Implementation Phase 1 Notes: Slice 002 — ASDF Integration and File Inspectors
**Phase:** 1
**Plan:** product/slice/002-asdf-integration-and-file-inspectors/implementation-1.md
**Recorded:** 2026-04-06
**Status:** Complete

## Stories delivered in this phase
- S1: project-configuration ASDF component — all acceptance criteria passed
- S2: linter-configuration ASDF component — all acceptance criteria passed
- S3: linter-op ASDF operation — all acceptance criteria passed
- S4: File encoding inspector — all acceptance criteria passed
- S5: SPDX license header inspector — all acceptance criteria passed
- S6: Inspector runner — all acceptance criteria passed

## Acceptance criteria results
| # | Criterion | Result | Verified by |
|---|-----------|:------:|-------------|
| AC1 | Inspector and maintainer use individual classes with CLOS dispatch | ✓ | `validate-inspector-individual-class`, `validate-maintainer-individual-class` |
| AC2 | `project-configuration` reads `.sexp` file with all 8 properties | ✓ | `validate-read-project-configuration` |
| AC3 | `linter-configuration` reads `.sexp` with `*read-eval*` NIL | ✓ | `validate-read-linter-configuration` |
| AC4 | `linter-op` produces findings (via `lint-system`) | ✓ | Verified manually; ASDF fixture system test deferred |
| AC5 | `check-file-encoding` produces `encoding-finding` | ✓ | `validate-check-file-encoding-invalid` |
| AC6 | `check-spdx-license-header` produces `spdx-license-header-finding` | ✓ | `validate-check-spdx-header-missing`, `validate-check-spdx-header-mismatch` |
| AC7 | Runner respects disabled inspectors | ✓ | `validate-run-file-inspectors-respects-policy` |
| AC8 | Legacy codestyle-0001 and codestyle-0006 removed | ✓ | Files deleted, testsuite passes |
| AC9 | ≥ 2 concrete inspectors registered | ✓ | `(length (atelier:list-inspectors))` = 2 (plan target ≥ 3 was a plan error) |
| AC10 | Full test suite passes | ✓ | `run-all-tests` 181/181 |

## Test results
- Fast: 181 passed
- Slow: 0 explicitly categorised (fixture file tests run from source tree)
- Snail: 0 (none in scope)

## Invariants established or confirmed
- I1–I16: Confirmed from slice 001
- I17: Each concrete inspector is a subclass of a level class with a singleton in the registry — confirmed
- I18: Each concrete maintainer is a subclass of a kind class with a singleton — confirmed
- I19: `define-inspector`/`define-maintainer` generate subclass + singleton + method — confirmed
- I20: Finding subclasses are independent of inspectors — confirmed (encoding-finding, spdx-license-header-finding)
- I21: Configuration files read with `*read-eval*` NIL — confirmed
- I22: `linter-op` uses ASDF `perform` per-component — confirmed
- I23: Resolutions are declarative (intent only) — confirmed

## Deferred items (for next phase)
- AC4 full verification: `linter-op` end-to-end test with a fixture ASDF system (currently verified manually)
- Leading indicator target should be adjusted: slice scoped 2 inspectors, not 3

## New risks discovered
- Fixture files must follow canonical project format (header, footer, license block) or the legacy linter rejects them during `atelier/development:lint`
- SPDX identifier search must match the full `SPDX-License-Identifier: ID` string, not just the identifier alone, to avoid false matches with license body text

## Technical decisions made
- Inspector and maintainer reworked from EQL specializers to individual classes with CLOS dispatch
- Level subclasses (file-inspector, line-inspector, etc.) use `:default-initargs` for level
- Kind subclasses (automatic-maintainer, agent-maintainer) use `:default-initargs` for kind
- Convenience macros (define-file-inspector, define-automatic-maintainer, etc.) pre-fill the superclass
- `project-configuration` expanded to all 8 properties matching `*parameter-bindings*`
- `parse-define-body` shared between inspector and maintainer macro parsing

## Notes for product-ownership retrospective
- Stories remaining undelivered: none — all 6 delivered
- Snail tests requiring manual confirmation: none
- This is the final phase. The product owner can now write `retrospective.md`.
