# Retrospective: Slice 005 — Autofix Pipeline with Pretty-Printer
**Recorded:** 2026-04-07
**Verdict:** ✅ Validated

## Hypothesis review

**Assumption:** The full finding → resolution → write-back cycle can be delivered using text-resolution for targeted replacements and syntax-resolution with CL's built-in pretty-printer for structural transforms, without requiring a third-party formatting library.

**Prediction:** A developer running `lint-system` with autofix enabled on a system with style violations receives corrected source files: trailing whitespace removed, indentation normalised, special variable and constant names gain their conventional markers, bare loop keywords replaced with keyword symbols, and bare lambdas in higher-order calls extracted to FLET — all preserving the surrounding code and formatting.

**Outcome:** ✅ Confirmed. All six maintainers deliver correct fixes via `lint-system :autofix t`. CL's built-in pprint (with an Atelier-specific dispatch table) proved sufficient for the FLET wrapping transform. No third-party library was needed.

## Stories delivered

| Story | Verdict |
|-------|:-------:|
| S1 Write-back engine | ✅ |
| S2 Line-level automatic maintainers (trailing whitespace, mixed indentation) | ✅ |
| S3 CST text-resolution maintainers (earmuffs, constant naming, bare loop keywords) | ✅ |
| S4 Pretty-printer for syntax write-back | ✅ |
| S5 Bare-lambda automatic maintainer via syntax-resolution | ✅ |
| S6 Batch autofix integration (`lint-system :autofix t`) | ✅ |

Beyond the six planned stories, a seventh inspector + maintainer pair was added in the same session: `check-labels-for-flet` and `fix-labels-to-flet`, which detect LABELS forms with acyclic call graphs and transform them to nested FLET groups. This was an emergent request — the lint-system autofix pipeline itself contained a spurious LABELS that became the motivating test case.

## Acceptance criteria

| # | Criterion | Result |
|---|-----------|:------:|
| 1 | `list-maintainers` returns all six+ maintainer symbols | ✅ |
| 2 | `lint-system :autofix t` fixes trailing whitespace | ✅ |
| 3 | Earmuffs maintainer produces `*wrong-name*` | ✅ |
| 4 | Bare-lambda maintainer produces FLET-wrapped form | ✅ |
| 5 | `pretty-print-form` returns valid indented Lisp source | ✅ |
| 6 | All pretty-printer fixture tests pass at each margin width | ✅ |
| 7 | `lint-system` without `:autofix` does not modify source files | ✅ |
| 8 | Fast tests < 2 s, slow tests < 10 s | ✅ |

## Quality attributes

- ✅ Write-back uses `uiop:tmpize-pathname` + `uiop:rename-file-overwriting-target` — atomic, recoverable.
- ✅ Each maintainer is exported and documented.
- ✅ Pretty-printer output compiles without error.
- ✅ Autofix is opt-in — default `lint-system` behaviour is unchanged.

## Leading indicator

Automatic maintainers at slice start: 0. At close: 7 (fix-trailing-whitespace, fix-mixed-indentation, fix-earmuffs, fix-constant-naming, fix-bare-loop-keywords, fix-bare-lambda, fix-labels-to-flet). Target was ≥ 6.

## Test results at close

290/290 tests passing (`atelier/testsuite:run-all-tests`).

## Risks that materialised

**Test maintainer registry pollution** — test maintainers registered in `*maintainers*` accumulated across test runs and interfered with integration tests that called `resolve-finding`. Mitigated by the `production-resolution-p` filter in `lint-system` (excluding maintainers from packages whose name contains "TEST") and by the integration test helper filtering the registry to the `atelier` package only. Low-priority design debt; does not affect production behaviour.

**Byte/character offset mismatch in Eclector** — on files with multi-byte UTF-8 characters in their headers (©, –, ë), `file-position` on a UTF-8 stream returns byte offsets while the line-vector infrastructure works with character offsets, causing the write-back engine to land edits in the wrong position. Fixed by changing `parse-lisp-file` to read the file as a string first and use `make-string-input-stream`, so Eclector receives character-position file positions throughout.

## What worked well

- The two-resolution-class design (text-resolution for targeted replacements, syntax-resolution for structural transforms) covered all six original maintainer types cleanly with no architectural changes.
- The Bellman-Ford depth grouping algorithm for `labels-transform-to-flet` was straightforward to implement and produces readable output (depth-0 functions in a flat outer FLET, callers nested inside).
- Using a string-input-stream for Eclector parsing is a small fix with broad correctness impact — worth documenting as a convention for any future CST-level work.

## What was harder than expected

- The `production-resolution-p` filter needed for `lint-system :autofix t` revealed that `FLET` vs `LABELS` matters for `lint-system` itself — `production-resolution-p` was initially outside `resolutions-for-findings` (requiring LABELS), then refactored to a nested FLET (Option A), which then became the motivating test case for the new inspector.
- SBCL's pprint behaviour for `flet` (unconditionally breaking the binding body onto a new line) required updating the `fix-bare-lambda` fixture to match SBCL's output rather than the expected form from the plan.

## Capability maturity transitions

| Capability | From | To |
|-----------|------|----|
| Autofix / Write-back | Not started | Foundation |
| Pretty-printer | Not started | Foundation |
