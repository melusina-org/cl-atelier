# Retrospective: Slice 004 — CST-Level Inspectors
**Recorded:** 2026-04-07
**Delivered by:** Maintainer + Claude Code
**Implementation phases:** 1 total — implementation-1.md

## Hypothesis Outcome
- Assumption tested: CST-level inspectors using Eclector's concrete syntax tree can detect naming convention violations and style issues that are invisible to line-level analysis, proving the syntax stage of the inspection pipeline.
- Leading indicator: CST-level inspectors registered — baseline 0 → result 4 (target ≥ 4)
- Verdict: ✅ Supported
- Kill criterion: CST-level inspectors cannot produce `syntax-finding` instances through the existing runner without architectural changes to the pipeline — not triggered. The runner was extended with a third stage (`perform-syntax-inspection`), following the same pattern established in slice 003.
- Stories delivered: 5 of 5
- Quality criteria: all passed

## Phase Delivery Summary
| Phase | Plan | Key deliverables | Deferred items |
|-------|------|-----------------|----------------|
| 1 | implementation-1.md | Four syntax inspectors (earmuffs, constant naming, bare lambda, bare loop keywords). `inspect-syntax` protocol for per-form dispatch. `parse-lisp-file` with Eclector. `source-position-to-line-column` mapping. `make-syntax-finding-from-form` factory. `define-syntax-inspector` macro rewritten to generate `inspect-syntax` methods. Pipeline decomposed into `perform-inspection` + three named stage functions. `define-finding`/`define-findings` macros for compact finding subclass declarations. ASDF integration refactored: `lint-system` canonical, `linter-op` delegates to it, dynamic vars renamed and simplified, graceful defaults with warning. | None |

## What we learned
- The `define-syntax-inspector` macro initially generated `inspect-file` methods (carried over from a stub), not `inspect-syntax` methods. The mismatch was caught early because the acceptance criteria demanded that inspectors receive individual CST forms — the three-layer protocol (inspect-file → iterate forms → inspect-syntax) would not work otherwise. Having concrete acceptance criteria that name the dispatch mechanism prevented a subtle architectural error from propagating.
- The pipeline decomposition into `perform-inspection` calling three named stage functions (`perform-file-inspection`, `perform-line-inspection`, `perform-syntax-inspection`) was an unplanned rework requested during implementation. The result is cleaner and more testable than the original monolithic `run-file-inspectors`. This validates that the staged pipeline design from slice 003 extends naturally to three levels.
- `perform-inspection` was initially designed to take config parameters and rebind the same dynamic variables already bound by `lint-system`. This redundancy was resolved by making `perform-inspection` read `*project-configuration*` and `*linter-configuration*` directly. The lesson: when a function exists solely within a dynamic binding context, it should read the bindings rather than accept them as parameters.
- The `lint-system` / `linter-op` relationship was inverted: `lint-system` is now canonical (walks source files directly) and `linter-op` delegates to it. This removed the artificial dependency on `asdf:operate` for file traversal and makes `lint-system` usable without ASDF operation machinery.
- `define-findings` with `(PARENT NAME DOCUMENTATION)` spec order enables alphabetical sorting that clusters findings by parent class. This emerged from a design discussion about compacting nine trivial `defclass` forms into a data-like declaration. The macro approach was chosen over alternatives (hash table, alist) because it preserves compile-time class creation.

## Impact on next prioritization
- The three-stage pipeline (file → line → syntax) is now proven end-to-end. All three stages follow the same pattern: `define-X-inspector` generates a method on a per-unit generic, and the base `inspect-file` method on the level class handles I/O and iteration.
- The pretty-printer decision (backlog #23) remains unresolved and blocks syntax-level write-back (SYNTAX-RESOLUTION). The four CST inspectors produce findings but cannot yet offer auto-fixes. This is acceptable for the current slice but becomes critical for any slice that aims to demonstrate the full finding → resolution → write-back cycle at the syntax level.
- The ASDF integration refactoring (`lint-system` canonical, graceful defaults) is a quality improvement that emerged during post-implementation review. It simplifies the contract for third-party systems consuming the linter: they get sensible defaults with a warning when no configuration component is declared.

## Maturity tracker changes
- CST-level Inspection (G2): Not started → Foundation
