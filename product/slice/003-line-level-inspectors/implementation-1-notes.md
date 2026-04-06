# Implementation Phase 1 Notes: Slice 003 — Line-Level Inspectors
**Phase:** 1
**Plan:** product/slice/003-line-level-inspectors/implementation-1.md
**Recorded:** 2026-04-07
**Status:** Complete

## Stories delivered in this phase
- S1: Trailing whitespace inspector — all acceptance criteria passed
- S2: Line length inspector — all acceptance criteria passed
- S3: Mixed indentation inspector — all acceptance criteria passed
- S4: Linter-configuration extension for indentation style — all acceptance criteria passed
- S5: Remove legacy codestyle-0002 — all acceptance criteria passed

## Acceptance criteria results
| # | Criterion | Result | Verified by |
|---|-----------|:------:|-------------|
| AC1 | `check-trailing-whitespace` produces `trailing-whitespace-finding` | ✓ | `validate-check-trailing-whitespace-dirty` |
| AC2 | `check-line-length` produces `line-too-long-finding`, skips definitions | ✓ | `validate-check-line-length-long`, `validate-check-line-length-skips-definitions` |
| AC3 | `check-mixed-indentation` produces `mixed-indentation-finding` | ✓ | `validate-check-mixed-indentation-tabs` |
| AC4 | `linter-configuration` `:indentation-style` respected | ✓ | `validate-indentation-style-configuration` |
| AC5 | Legacy codestyle-0002 removed, suite passes | ✓ | Files deleted, 197/197 passing |
| AC6 | `inspect-file` takes 2 args, existing inspectors updated | ✓ | All inspector tests pass with new signature |
| AC7 | Runner includes both file and line inspectors | ✓ | `validate-run-file-inspectors` |
| AC8 | ≥ 3 line-level inspectors registered | ✓ | 3 line inspectors in `(list-inspectors)` |
| AC9 | `atelier/development:lint` completes without error | ✓ | Returns T |

## Test results
- Fast: 197 passed
- Slow: 0 explicitly categorised (fixture tests run from source tree)
- Snail: 0 (none in scope)

## Invariants established or confirmed
- I1–I23: Confirmed from slices 001–002
- I24: `inspect-file` takes 2 arguments. Configuration via special variables. — confirmed
- I25: `define-file-inspector`/`define-line-inspector` declare `inspector` as `ignorable`. — confirmed
- I26: Runner binds `*current-project-configuration*`, `*current-linter-configuration*`, and `*current-pathname*`. — confirmed
- I27: Line inspectors are `line-inspector` subclasses. Runner runs file inspectors first, then line inspectors. — confirmed
- I28: Default indentation style is `:spaces`. — confirmed
- I29: `inspect-line` receives a single line string and line number. `inspect-file` on `line-inspector` iterates and delegates to `inspect-line`. — new
- I30: Inspection pipeline design documented in `references/inspection-pipeline.md`. — new

## Deferred items (for next phase)
- None — all 5 stories delivered

## New risks discovered
- `*current-linter-configuration*` may be NIL when the runner is called with nil config. Inspectors that read config slots must guard against this. The mixed-indentation inspector handles this with an `if` check.

## Technical decisions made
- `inspect-file` is the single entry generic for all inspector levels. Line inspectors specialize on `vector` (line vector) and `pathname` (convenience, reads and delegates). The `inspect-line` generic handles single-line logic.
- `define-line-inspector` generates `inspect-line` methods, not `inspect-file` methods. The base `inspect-file` method on `line-inspector` handles iteration.
- `*current-pathname*` special variable carries the file pathname for line inspectors that receive a vector.
- Inspection pipeline design documented: file → line → syntax stages with reduce structure. Stages 1–2 implemented, stage 3 deferred.

## Notes for product-ownership retrospective
- Stories remaining undelivered: none — all delivered
- Snail tests requiring manual confirmation: none
- This is the final phase. The product owner can now write `retrospective.md`.
