# Implementation Phase 1 Notes: Slice 001 — Finding/Resolution Schema
**Phase:** 1
**Plan:** product/slice/001-finding-resolution-schema/implementation-1.md
**Recorded:** 2026-04-06
**Status:** Complete

## Stories delivered in this phase
- S1: Finding class hierarchy — all acceptance criteria passed
- S2: Resolution class hierarchy — all acceptance criteria passed
- S3: Inspector registry with named-instance pattern — all acceptance criteria passed
- S4: Maintainer registry with superseding — all acceptance criteria passed
- S5: prepare-resolution generic protocol — all acceptance criteria passed
- S6: Named-instance pattern infrastructure — all acceptance criteria passed
- S7: Existing linter integration (bridge) — all acceptance criteria passed
- S8: Eclector CST dependency — all acceptance criteria passed

## Acceptance criteria results
| # | Criterion | Result | Verified by |
|---|-----------|:------:|-------------|
| AC1 | All five finding classes instantiate with correct slot types | ✓ | `testsuite-finding` 20/20 |
| AC2 | `syntax-finding` slots accept `concrete-syntax-tree:cst` instances | ✓ | `test-syntax-finding-cst-types` |
| AC3 | `define-inspector` registers, `find-inspector` retrieves, `list-inspectors` lists | ✓ | `test-define-inspector-and-list` |
| AC4 | `define-maintainer` with `:supersedes` produces correct partial order | ✓ | `test-maintainer-superseding` |
| AC5 | `prepare-resolution` returns resolution or NIL | ✓ | `test-prepare-resolution-returns-resolution`, `test-prepare-resolution-returns-nil` |
| AC6 | `hint-to-finding` converts legacy hints to findings | ✓ | `test-hint-at-file-to-finding`, `test-hint-at-file-line-to-finding` |
| AC7 | Existing testsuite passes after legacy move | ✓ | `run-all-tests` 135/135 |
| AC8 | System loads with Eclector dependency | ✓ | `(asdf:load-system "org.melusina.atelier")` succeeds |
| AC9 | Public API exports ≥ 25 symbols | ✓ | 95 exported symbols |
| AC10 | `atelier/development:lint` works via legacy system | ✓ | Returns T |

## Test results
- Fast: 135 passed
- Slow: 0 (none in scope)
- Snail: 0 (none in scope)

## Invariants established or confirmed
- I1–I8: Confirmed (pre-existing conventions)
- I9: `define-named-class` expands to plain `defclass` — confirmed
- I10: Named-instance initargs use `:description` not `:documentation` — confirmed
- I11: Inspector identity is a symbol; legacy keyword codes are valid — confirmed
- I12: `rationale` is a regular instance slot, per-class by convention — confirmed
- I13: `text-resolution` derives location from linked finding — confirmed
- I14: `composite-resolution` ordering documented but not enforced — confirmed
- I15: `atelier/legacy` uses `:import-from`, not `:use` — confirmed
- I16: Superseding cycle detection at `define-maintainer` time — confirmed

## Deferred items (for next phase)
- None — all 8 stories delivered

## New risks discovered
- Template files (`resource/template/`) must be updated whenever the `atelier` public API changes — the templates generate code that references these symbols. Not covered in the risk register.

## Technical decisions made
- Template files (`LISP-DEVELOPMENT.text`, `LISP-DEVELOPMENT-LINT.text`, `LISP-ASDF.text`) updated to reference `atelier/legacy:lint` and depend on `org.melusina.atelier/legacy` — required for generated projects to compile.
- ASDF testsuite module renamed from `"inspector"` to `"legacy-inspector"` (with `:pathname "inspector"`) to avoid name collision with the new `test/inspector.lisp` file.
- `edit-first-line` and `edit-last-line` added to legacy package imports and atelier exports — needed by codestyle-0003 and codestyle-0004 inspectors.
- `test/utilities.lisp` updated: `atelier::anomaly`, `atelier::autocorrect`, `atelier::*hint-pathname*` → `atelier/legacy::` equivalents.

## Notes for product-ownership retrospective
- Stories remaining undelivered in this slice: none — all delivered
- Snail tests requiring manual confirmation before slice closes: none
- This is the final phase. The product owner can now run the hypothesis review and write `retrospective.md`.
