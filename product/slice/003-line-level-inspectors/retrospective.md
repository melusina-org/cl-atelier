# Retrospective: Slice 003 — Line-Level Inspectors
**Recorded:** 2026-04-07
**Delivered by:** Maintainer + Claude Code
**Implementation phases:** 1 total — implementation-1.md

## Hypothesis Outcome
- Assumption tested: Line-level inspectors operating on all languages through the same pipeline will validate that the linter architecture handles multi-level inspection without requiring runner changes.
- Leading indicator: Line-level inspectors registered — baseline 0 → result 3 (target ≥ 3)
- Verdict: ✅ Supported
- Kill criterion: Line-level inspectors cannot produce line-finding instances through run-file-inspectors without runner modifications — not triggered. The runner was extended (not modified) to add stage 2 for line inspectors.
- Stories delivered: 5 of 5
- Quality criteria: all passed

## Phase Delivery Summary
| Phase | Plan | Key deliverables | Deferred items |
|-------|------|-----------------|----------------|
| 1 | implementation-1.md | Three line inspectors (trailing whitespace, line length, mixed indentation). `inspect-file` refactored to 2-arg with special variables. `inspect-line` protocol for per-line dispatch. Staged runner (file inspectors → read lines once → line inspectors). `linter-configuration` extended with `:indentation-style`. Legacy codestyle-0002 removed. Pipeline design documented. | None |

## What we learned
- The inspection pipeline design (file → line → syntax stages) should have been documented before implementation, not after. A design discussion produced a clear 10-step pipeline description that was acknowledged but not recorded. It was only documented during the rework phase, after the initial implementation missed the staged structure entirely.
- `inspect-file` as a single generic with CLOS dispatch on the source type (pathname vs vector) is clean, but line inspectors need a per-line protocol (`inspect-line`) to avoid duplicating the iteration in every inspector. The three-layer protocol (inspect-file → iterate → inspect-line) emerged from the rework.
- Special variables (`*current-pathname*`, `*current-project-configuration*`, `*current-linter-configuration*`) are the right mechanism for passing context through the inspection chain. They keep the generic function signatures clean and the inspector bodies focused on inspection logic.
- `*current-linter-configuration*` may be NIL — inspectors that read config slots must guard against this. This should be documented as an invariant.

## Impact on next prioritization
- The file → line pipeline is proven. Stage 3 (syntax inspectors via Eclector CST) is the natural next step, but requires the pretty-printer decision for write-back.
- The `inspect-line` protocol validates the pattern: `define-X-inspector` generates a method on a per-unit generic (`inspect-line`, future `inspect-syntax`), and the base `inspect-file` method on the level class handles the I/O and iteration.
- The pipeline reference document should be updated as each stage is implemented.

## Maturity tracker changes
- Line-level Inspection (G2): Not started → Foundation
