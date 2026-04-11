# Implementation Phase 1 Notes: Slice 009 — MCP server skeleton

**Phase:** 1
**Plan:** `product/slice/009-mcp-skeleton/implementation-1.md`
**Recorded:** 2026-04-11
**Status:** Complete

## Stories delivered in this phase

All eight stories from `slice.md` delivered:

- **S1** — `org.melusina.atelier/mcp` ASDF system and `#:atelier/mcp` package with the full export list
- **S2** — MCP protocol handshake over a two-way-stream (initialize, `notifications/initialized`, tools/list, tools/call, resources/list, resources/templates/list, resources/read, error envelopes)
- **S3** — Tool registry and **six** tools defined via `define-tool`
- **S4** — Resource registry split into concrete and template buckets, Atelier registry resources
- **S5** — `lisp://systems` resource walking the ASDF source-registry without loading anything
- **S6** — Transcript subsystem with sexp-canonical on-disk format, JSON and Markdown derived views, atomic per-entry writes, torn-tail-tolerant reads
- **S7** — Abstract `image-connection` class with three generics and UIOP-delegating defaults
- **S8** — Testsuite `org.melusina.atelier/testsuite/mcp` with per-subsystem testcases plus a fresh-SBCL subprocess load test

## Acceptance criteria results

| # | Criterion | Result | Verified by |
|---|---|:---:|---|
| 1 | Fresh-SBCL load, zero MCP warnings | ✅ | `validate-mcp-system-loads-cleanly` (spawns a cold `sbcl --non-interactive` subprocess) |
| 2 | Package + exports complete | ✅ | `validate-parse-mcp-message` + the fact that 184 tests reference exported symbols |
| 3 | Handshake — 6 tools, 3 resources, 5 templates | ✅ | `validate-handshake-*` via `serve-two-way-stream` over string streams |
| 4 | Malformed JSON → -32700, unknown method → -32601, no crash | ✅ | `validate-handshake-malformed-json-parse-error`, `validate-dispatch-unknown-method` |
| 5 | 6 tools registered, dispatched via CLOS on generated classes | ✅ | `validate-tool-invocation-tests`, `validate-tool-dispatch-is-specialized` |
| 6 | 3 concrete + 5 templated resources match correctly; unknown URIs → -32002 | ✅ | `validate-resource-read-tests`, `validate-dispatch-resources-read-not-found` |
| 7 | Tool handler signals → in-band `isError:true`; server keeps running | ✅ | `validate-dispatch-tool-handler-signal-in-band` |
| 8 | Transcript writes atomically, tolerates torn tails, renders three views | ✅ | `validate-transcript-torn-write-recovery`, `validate-transcript-write-protocol-flushes-per-entry`, `validate-sexp-to-json-walker`, `validate-sexp-to-markdown-renderer` |
| 9 | `image-connection` abstract, no concrete subclass loaded | ✅ | `validate-image-connection-tests` + Reviewer code review (no subclass anywhere in the tree) |
| 10 | `asdf:test-system` passes in fresh SBCL with zero new regressions | ✅ | 184/184 MCP assertions + 295/295 base atelier assertions in fresh SBCL subprocess = 479/479 |
| 11 | Templates grep for `atelier/mcp` / `mcp` | ✅ | zero matches |
| 12 | R1 audit — every exported symbol traces to an AC | ✅ | every symbol in `src/mcp/package.lisp` is exercised by the testsuite |

## Test results

- **Fast**: 178 assertions passed (all encoding, macro expansion, URI template, dispatcher, tool invocation, resource matching, registry counts, parse tests, image-connection tests)
- **Slow**: 6 assertions passed (3 transcript-filesystem tests + 2 transcript torn-write tests + 1 fresh-SBCL subprocess load test)
- **Snail**: 0
- **Skipped**: 0 — filesystem was available in the test environment

**Totals:**
- MCP testsuite: 184/184 (100%)
- Existing atelier testsuite: 295/295 (100%, no regression)
- Combined: 479/479

The pass-count prediction range in the plan was 50–100 new assertions; actual is 184, significantly above range. The difference reflects two things:

1. Each testcase bundled multiple `assert-*` calls for thoroughness — a single "validate-handshake-initialize" testcase produces 3 assertions in one run.
2. The fresh-SBCL subprocess test is automated (2 assertions: exit code + warning count), adding coverage that wasn't in the original range estimate.

**Calibration note for `product/knowledge/calibration.md`:** plan predicted 50–100, actual 184 — the unit was "testcases × assertions", not "testcases". Range overshoot by a factor of ~2. The lesson (matching slice 008 calibration data) is that assertion counts scale super-linearly with testcase counts for thoroughly-tested code.

## Invariants established or confirmed

**Invariants 1–11: confirmed.** Slice 009 respects all prior-slice invariants. No test touches the production registries (INV-16 proposed below), no template is modified (INV-11), the fresh-SBCL check is baked into the suite (INV-4 *upgraded* from discipline to test).

**Invariants proposed for Reviewer promotion at phase closure:**

- **INV-12** — MCP tool handlers return Lisp data for `:application/json` MIME types and strings for `text/*` MIME types. The dispatcher owns the envelope. Handlers never call jzon directly.
- **INV-13** — Generic function signatures on `image-connection` are stable across slices. Methods may be added; signatures may not be changed. Breaking changes require a new slice and risk review.
- **INV-14** — Every `define-tool` form traces to exactly one exported symbol in at least one acceptance criterion (R1 mitigation made durable).
- **INV-15** — Transcript entries are written atomically per entry via the `prin1` + `terpri` + `finish-output` triple. A torn write at process exit leaves a file readable up to the last complete entry.
- **INV-16** — Test code that calls `define-tool` or `register-tool` must wrap itself in `with-isolated-registries`. No test is allowed to mutate the global `*tool-registry*` or `*resource-registry*` persistently.

## Deferred items (for next phase)

None. Slice 009 is a single-phase slice.

**Deferred to slice 010** (not slice 009's problem, listed so slice 010 Tactician has a ready list):

- Concrete `swank-connection` subclass of `image-connection`, populating `process-info` from `(uiop:launch-program ...)` over a `socketpair(2)` AF_UNIX bidirectional FD.
- Eval tool definition dispatching to `connection-eval`.
- Timeout and interrupt plumbing (see `references/python-prototype-notes.md` §"Useful patterns to revisit in slice 010").
- Package and symbol introspection tools (`lisp://packages/...` resource family) — needs the child image as the target.
- `*debugger-hook*` installation in `serve-two-way-stream` to catch uncaught handler conditions. Slice 009 got by without it because no handler does anything that can signal asynchronously; slice 010's eval will.

## Reworks performed

1. **Plan amendment 1** — `resources/list` vs. `resources/templates/list` split. Discovered during step 2 (writing `references/mcp-protocol.md`) when the spec was fetched. Amendment added to `implementation-1.md` before any code touched. Cost: minor — one additional request class, one additional dispatcher method, one additional fixture, adjusted S2 AC3.

2. **Plist reversal bug in `%parse-define-tool-clauses`** — the first cut of the DEFINE-TOOL macro built its clauses plist by prepending key and value, then `nreverse`'d. `nreverse` on a plist reverses pairs too, producing `(value key value key)` instead of `(key value key value)`. Fixed by switching to a hash-table internally and flattening on exit. Cost: 1 edit, immediately visible when the first test form errored with a parse exception.

3. **Notifications can't accept `:id`** — first cut of `parse-mcp-message` passed `:id nil` to `(make-instance 'mcp-notification ...)`. The notification class has no `id` slot, so `:id` was rejected as an invalid initarg. Fixed by building the initarg list conditionally (notifications get no `:id`). Cost: 1 edit, caught on the first parse test.

4. **`nil` lispified as JSON null instead of `[]`** — the first cut of `%lispify-handler-result` returned `+json-null+` for any `nil`. This made empty lists (e.g., `maintainer-supersedes`) encode as JSON null, which a reasonable consumer would interpret as "this field is missing" rather than "empty list". Fixed by making `nil` become `#()` (empty vector → JSON array) and reserving the explicit symbol `cl:null` for JSON null. Cost: 1 edit + one additional docstring.

5. **`%lispify-handler-result` misclassified list-of-alists as alist** — the first alist-detection heuristic (`(consp (first value))`) would match both `(("k" . "v"))` (alist) and `((("k" . "v")))` (list containing one alist). Fixed by checking that every entry has an atomic car (string, symbol, number), which distinguishes cleanly. Cost: 1 edit, caught during interactive testing before committing.

6. **`uiop:xdg-state-home` not exported in older UIOP** — the transcript file path initially used `uiop:xdg-state-home` which doesn't exist in the UIOP shipped with the running image. Replaced with a local `%xdg-state-home-base` helper that honours `$XDG_STATE_HOME` with the XDG default of `~/.local/state/`. Cost: one helper function.

7. **Subprocess `--eval` reader failed on `asdf:*central-registry*`** — the fresh-SBCL load test passed a single `--eval` form containing `asdf:*central-registry*`, which was read before `(require :asdf)` in the same form executed, so the reader couldn't resolve the `asdf` package. Fixed by splitting into multiple `--eval` forms, each read and evaluated in sequence, using `find-symbol` to avoid package-prefixed reads. Cost: one function refactor.

8. **Confidence `assert-t` is strict** — 16 assertions in the first cut of the testsuite used `assert-t` with forms that return generalised booleans (`search`, `fboundp`, `probe-file`, `hash-table-p`, `stringp`). Confidence's `assert-t` is literally strict. Fixed by importing `assert-t*` and switching the affected assertions. This is the documented pattern from `MEMORY.md` — I should have known. Cost: one import line + ~15 edits.

9. **Keyword case in transcript raw-read test** — the test expected `":marker"` in the raw transcript file, but `prin1` uppercases keywords by default, producing `":MARKER"`. Fixed by updating the assertion. Cost: 1 edit.

**Pattern observation:** 5 of the 9 reworks (items 2, 3, 4, 5, 8) are language-level CL surprises that the Common Lisp skill's reference documents warn about. The ones that bit me most hard were the `nil`/`false`/`null` overloading in jzon, and the `assert-t`/`assert-t*` distinction. Both are documented in `product/knowledge/` memory already; I should have re-read them before starting Phase 2. Candidate for a new pattern entry in `product/knowledge/patterns.md`: *"Skimming the knowledge base is not enough — re-read before starting implementation."*

## New risks discovered

None that weren't already in the risk register. R1 (hallucinated requirements) was an ongoing concern throughout but did not materialize — every exported symbol traces to an AC, and the audit at phase closure confirms it. R13 (uncaught handler condition → debugger hang) did not materialize because the `handler-case` in `%serve-one-line` catches condition escapes from the dispatcher; the more ambitious `*debugger-hook*` mitigation was deferred to slice 010.

## Technical decisions made

1. **`assert-t*` everywhere for non-strict truthy checks** — the CL skill's MEMORY.md documents this, and the test suite now uses it consistently.

2. **`with-isolated-registries` macro wraps every test that calls `define-tool` at runtime** — hard fix for test-registry pollution. Not a "TEST package" heuristic. Proven by the fact that `define-tool-macro` testsuite doesn't pollute the production registries.

3. **Keep the subprocess-load test in the automated suite** — running a cold `sbcl --non-interactive` subprocess inside a test takes ~3 seconds. Upfront cost for CI but cheap insurance; automates INV-4 from discipline to enforced property.

4. **Error handling split: tools use in-band `isError`, resources use protocol `-32002`** — this matches the MCP spec and is codified in `mcp-protocol.md`. Slice 009 dispatcher has distinct `:around` strategies for the two paths.

5. **Transcript JSON view is derived on read, not cached** — slice 009 re-walks the plist file on every `resources/read`. If the file grows large, slice 010+ can add caching; for now simplicity wins.

6. **No `*debugger-hook*` installation in slice 009** — deferred to slice 010 when eval is the path that can actually hang. The per-line `handler-case` around dispatch is sufficient for slice 009's tools.

7. **Package reference sidebar in CLAUDE.md now names 5 systems** — added `/mcp` and `/testsuite/mcp`.

## Notes for Strategist retrospective

- **Stories remaining**: none.
- **Snail tests requiring confirmation**: none — zero snail tests in this slice.
- **Recommended**: promote INV-12 through INV-16 during the Reviewer's knowledge consolidation step. The invariants are independently load-bearing and each has a specific AC verifying it.
- **Calibration data**: the 184-assertion actual vs. 50–100 predicted range is a data point for the "counting the wrong unit" hypothesis in `product/knowledge/calibration.md` — for test-heavy slices that use multi-assertion testcases, predicting per-testcase counts overshoots by ~2×. Suggest future plans count `(count-assertions inside each testcase)` or give a 3–5× wider range.
- **Amendment 1 (resources/templates/list split) is a real finding** worth documenting in the retrospective — a spec fetch during step 2 caught a plan gap before any code was written. The amendment process (append a section, don't rewrite history) worked cleanly.
- **Slice 010 prep**: the `image-connection` abstraction plus the `python-prototype-notes.md` reference are ready for slice 010 to pick up directly. Re-read `mcp-protocol.md` for any spec details affecting eval.

---

**Handoff:** [Maker] Phase 1 notes written. All stories delivered. Strategist can run the retrospective.
