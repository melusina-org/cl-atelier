;;;; implementation-1-notes.md — Phase 1 completion notes for Slice 005

# Implementation Phase 1 Notes: Slice 005 — Autofix Pipeline
**Phase:** 1 of 1
**Plan:** product/slice/005-autofix-pipeline/implementation-1.md
**Recorded:** 2026-04-07
**Status:** Complete

## Stories delivered in this phase

- **S1 Write-back engine** — acceptance criteria: all passed
- **S2 Line-level maintainers** — acceptance criteria: all passed
- **S3 CST text-resolution maintainers** — acceptance criteria: all passed
- **S4 Pretty-printer dispatch table** — acceptance criteria: all passed
- **S5 Bare-lambda maintainer** — acceptance criteria: all passed
- **S6 Batch autofix integration** — acceptance criteria: all passed

## Acceptance criteria results

| # | Criterion | Result | Verified by |
|---|-----------|:------:|-------------|
| 1 | `list-maintainers` returns all six maintainer symbols | ✓ | `validate-line-maintainers-registered`, `validate-syntax-text-maintainers-registered`, `validate-fix-bare-lambda-registered` |
| 2 | `lint-system :autofix t` removes trailing whitespace and writes file back | ✓ | `validate-lint-system-autofix` |
| 3 | Earmuffs maintainer produces replacement `*wrong-name*` | ✓ | `validate-fix-earmuffs` |
| 4 | Bare-lambda maintainer produces FLET-wrapped form | ✓ | `validate-fix-bare-lambda` |
| 5 | `pretty-print-form` returns valid indented Lisp source | ✓ | `validate-pretty-print-fixtures` |
| 6 | All pretty-printer fixture tests pass at each margin width | ✓ | `validate-pretty-print-fixtures` |
| 7 | `lint-system` without `:autofix` does not modify source files | ✓ | `validate-lint-system-no-autofix` |
| 8 | Fast tests < 2 s, slow tests < 10 s | ✓ | timing observed during test runs |
| 9 | `asdf:test-system` passes with zero failures | ✓ | 264/264 tests passing |

## Test results

- Fast: 259 passed
- Slow: 5 passed (`validate-text-resolution-write-back`, `validate-syntax-resolution-write-back`, `validate-lint-system-autofix`, `validate-lint-system-no-autofix`, `validate-lint-system-partial-autofix`)
- Snail: none

## Invariants established or confirmed

- **I11**: Maintainers are defined with `define-automatic-maintainer`, one per file under `src/maintainers/` — confirmed, all six follow this pattern.
- **I12**: Pretty-printer fixtures use the multi-margin YAML format — confirmed.
- **I13**: Maintainer fixtures use the two-document YAML format with AST comparison — confirmed.
- **I14**: `*atelier-pprint-dispatch*` is never modified after load time — confirmed.
- **I15**: Write-back uses `uiop:tmpize-pathname` + `uiop:rename-file-overwriting-target` for atomicity — confirmed.
- **I16**: `lint-system` without `:autofix` preserves current behaviour — confirmed by `validate-lint-system-no-autofix`.
- **I17**: Resolutions applied end-to-start (descending offset order) — confirmed in `apply-resolutions-to-file`.
- Invariants I1–I10: confirmed.

## Deferred items (for next phase)

None — all stories from `slice.md` were delivered in this phase. The slice is complete.

## New risks discovered

**Test maintainer registry pollution.** Test maintainers registered by `test/maintainer.lisp` accumulate in the global `*maintainers*` hash table across test runs. When `resolve-finding` is called in integration tests, it finds all registered maintainers including test ones. This caused two failure modes:

1. Test maintainers dispatch on the base `finding` class and attempted to produce `text-resolution`s for `spdx-license-header-finding` (a pure `file-finding` with no `finding-line`), causing a "no applicable method for `finding-line`" error.
2. Multiple maintainers producing resolutions for the same span triggered the overlap detector.

**Mitigation applied:** The `apply-autofix-to-file` integration test helper shadows `*maintainers*` with a filtered table containing only maintainers in the `atelier` package (excluding `atelier/test`). The production `lint-system` is unaffected because test maintainers are not present in production.

**Suggested follow-up:** Consider a `with-maintainer-registry` macro or a `define-automatic-maintainer` option to scope registration to a dynamic extent, so test maintainers do not persist across testcase boundaries.

## Technical decisions made

- **`resolve-finding` returns a list, not a single resolution.** The callers in `lint-system` and the integration test helper must iterate over the returned list with `dolist`, not treat it as a single value. Both sites were updated. The plan text implied a single resolution; the actual API was already a list.

- **Filter to `line-finding` before resolving.** Both `lint-system` and `apply-autofix-to-file` filter findings to `line-finding` instances before calling `resolve-finding`. This is correct because `text-resolution` calls `finding-line` which only exists on `line-finding`. File-only findings (`file-finding` but not `line-finding`) do not have a line/column position and cannot be targets for `text-resolution`.

- **`validate-lint-system-partial-autofix` assertion corrected.** The original plan's assertion `(not (position #\Space :from-end t :end newline-pos))` checks for any space before the first newline, which fails when the line contains internal spaces (e.g. `() nil)`). The correct assertion checks that the character immediately before the newline is not a space: `(not (char= #\Space (char content (1- newline-pos))))`.

- **`find-enclosing-call` uses `walk` not `search`.** The LABELS function was named `search` in an early draft, which conflicted with `CL:SEARCH`. Renamed to `walk`.

- **SBCL pprint wraps `flet` bindings unconditionally.** SBCL's pretty-printer always places the `flet` binding body on a new line, regardless of available width. The `fix-bare-lambda` fixture was updated to match this behaviour rather than fighting it.

- **`read-pretty-print-fixture` return values declaration.** SBCL type-checks `declare (values ...)` strictly: having 4 types for 5 return values causes a type error at runtime. The declaration was corrected to `(values list t (integer 0) list list)`.

## Notes for product-ownership retrospective

- All six stories delivered; the slice is complete.
- No snail tests; no waivers required.
- The test maintainer pollution issue is a low-priority design debt worth noting in the retrospective. It does not affect production behaviour but will affect any future integration test that calls `resolve-finding` after running the maintainer registry tests.
- Commit: `5573a24` — 34 files changed, 1313 insertions.
