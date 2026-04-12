# Implementation Phase 1 Notes: Slice 010 — Editor Foundation

**Phase:** 1
**Plan:** `product/slice/010-editor-foundation-and-mcp-eval/implementation-1.md`
**Recorded:** 2026-04-12
**Status:** Complete

## Stories delivered in this phase

- **S0** — `atelier:lint-string` in core atelier (`src/runner.lisp`). Runs the lint pipeline on an in-memory string at configurable inspector levels. Returns `(values fixed-string findings)`.
- **S1** — `org.melusina.atelier/editor` system and `#:atelier/editor` package with 15 exports. Loads cleanly in a fresh SBCL without pulling in MCP, jzon, or bordeaux-threads (INV-17).
- **S2** — `toplevel-form` record with 5 slots (kind, name, body, eval-when, source-text). `read-toplevel-form-from-string` parses with a custom Eclector client preserving `#+`/`#-`. `toplevel-form-ast` derived accessor.
- **S3** — `normalize-toplevel-form` composing write → lint-string → read. Applies maintainers (earmuffs, etc.) while preserving feature guards.
- **S9** — `testsuite/editor/` module inside `org.melusina.atelier/testsuite`, package `atelier/testsuite/editor`, wired into `run-all-tests`.
- **S10** — OSS check and prior-art survey. Verdict: build from scratch. No reusable CL projectional editor. Results in `references/prior-art.md`.
- **S11** — Auto-discovered fixture directory `testsuite/fixtures/editor/canonicalize/` with 15 `.text` fixtures. Each produces 3 assertions (normalized match, idempotency, round-trip).

## Acceptance criteria results

| # | Criterion | Result | Verified by |
|---|---|:---:|---|
| AC1 | Fresh-SBCL load, zero warnings | ✅ | T7 |
| AC2 | Editor loads without MCP deps | ✅ | T7 |
| AC3 | 15 symbols exported | ✅ | T6 |
| AC4 | `make-toplevel-form` constructs correctly | ✅ | T8 |
| AC5 | Read peels eval-when, derives kind + name | ✅ | T9, T10, T14 |
| AC6 | CST body preserves `#+`/`#-` | ✅ | T11, T26 |
| AC7 | `toplevel-form-ast` evaluates per `:features` | ✅ | T12, T13 |
| AC8 | PROGN signals with `decompose` restart | ✅ | T15, T16 |
| AC9 | Side-effectful forms signal, no restart | ✅ | T17 |
| AC10 | Write preserves `#+`/`#-`, wraps eval-when | ✅ | T22, T23, T24 |
| AC11 | Round-trip fixed point | ✅ | T25, fixtures |
| AC12 | Normalize applies fix-earmuffs | ✅ | T18 |
| AC13 | Normalize returns findings | ✅ | T19 |
| AC14 | Normalize is idempotent | ✅ | T20, fixtures |
| AC15 | `lint-string` runs syntax inspectors | ✅ | T1, T3 |
| AC16 | Testsuite module runs via `run-all-tests` | ✅ | T41, T42 |
| AC17 | Full regression in fresh SBCL | ✅ | 583/583 |
| AC18 | Fixture auto-discovery ≥ 42 assertions | ✅ | 45 assertions (15 × 3) |

## Test results

- **Fast:** 103 assertions passed
- **Slow:** 1 assertion passed (fresh-SBCL editor-standalone subprocess)
- **Snail:** 0
- **Skipped:** 0

**Totals:**
- Editor testsuite: 104/104 (100%)
- Existing atelier + MCP testsuite: 479/479 (100%, no regression)
- Combined: 583/583

Assertion count prediction: 50–150. Actual: 104. Within range.

## Invariants established or confirmed

**Invariants 1–16: confirmed.** No changes to prior invariants.

**New invariants:**

- **INV-17:** `org.melusina.atelier/editor` loads in a fresh SBCL without `com.inuoe.jzon`, `bordeaux-threads`, or `atelier/mcp`. Enforced by T7.
- **INV-18:** The `toplevel-form` body is an Eclector CST preserving reader conditionals (`#+`/`#-`) as `skipped-cst` / `annotated-cons-cst` nodes. Enforced by T11, T26.
- **INV-19:** `normalize-toplevel-form` is idempotent: applying it twice yields the same text as applying it once. Enforced by T20 and every fixture.
- **INV-20:** The round-trip `write(read(write(read(s)))) = write(read(s))` holds for any string `s` accepted by `read-toplevel-form-from-string`. Enforced by T25 and every fixture.

## Deferred items (for Phase 2)

- **S4** — Concrete `swank-socket-connection` subclass of `image-connection`
- **S5** — `atelier:eval-form` MCP tool with lazy child spawn
- **S6** — Package and symbol introspection tools
- **S7** — Testsuite runner tools (`run-tests-fresh`, `run-tests-in-child`)
- **S8** — `atelier:canonicalize-form` MCP adapter tool

## Reworks performed

1. **Symbol comparison in tests** — `'foo` in the test package is `ATELIER/TESTSUITE/EDITOR::FOO`, but the parsed form's name is `CL-USER::FOO`. Fixed: compare by `symbol-name`, not `equal`. Trigger: the reader interns symbols in the package that is current at parse time, not the test package. Cost: 3 edits.

2. **`assert-condition` argument order** — wrote `(assert-condition TYPE FORM)`, correct order is `(assert-condition FORM TYPE)`. Trigger: documented in `MEMORY.md` but not re-read before test authoring. Cost: 1 edit. Pattern: "skim-then-code" from `product/knowledge/patterns.md`.

3. **eval-when situation serialization** — `wrap-eval-when-if-needed` called `(mapcar #'string-upcase (mapcar #'symbol-name ...))` producing quoted strings instead of keyword symbols. Fixed: emit situations directly via `~S`. Cost: 1 edit.

4. **Fresh-SBCL subprocess test** — two sub-reworks: (a) `uiop:launch-program` doesn't accept `:output :string` (that's `run-program`), switched to `uiop:run-program`. (b) Single `--eval` form with `asdf:*central-registry*` fails at reader time before `(load quicklisp/setup.lisp)` executes. Fixed: split into multiple `--eval` forms per slice 009 rework #7 pattern. Cost: 2 iterations.

5. **Write path rework (from Reviewer feedback)** — the original C2 design (pretty-print matching branches + append skipped regions verbatim) lost the `#+sbcl` prefix on matching branches because Eclector evaluates `#+` transparently. Reviewer's corrected tests exposed this. Fixed: `write-cst-to-string` now copies the body's source range verbatim from the original string when `source-text` is available. Result: 89 lines removed, 32 added. Simpler and correct. Trigger: Reviewer wrote tests asserting `(search "#+sbcl" output)` which the old write path failed.

## New risks discovered

- **SWANK hangs when defining `:around` methods on Eclector's heavily-used generic functions.** The GF recomputation interacts badly with the SWANK thread model (`thread-for-evaluation` on `:spawn` connections). Workaround: never define methods on Eclector GFs via interactive SWANK eval; always load from files via ASDF. Not a code risk — a development-workflow risk.

## Technical decisions made

1. **Source text is the truth, CST is for analysis.** `write-toplevel-form-to-string` copies the original source text verbatim (via the body CST's source range) when `source-text` is available. Pretty-printing from `cst:raw` is the fallback for programmatic forms only. This is the single most important design decision in Phase 1 — it makes `#+`/`#-` preservation fall out naturally.

2. **`lint-string` applies resolutions as character-offset spans on the original string.** Because Eclector's source positions are character offsets into the input string, and the `apply-resolutions` engine replaces by span, feature guards between resolution targets are untouched. No special handling needed.

3. **Custom Eclector client with three classes.** `preserving-cst-client` overrides `make-skipped-input-result` (returns `skipped-cst`) and `make-expression-result :around` (filters skipped nodes, annotates result as `annotated-cons-cst`). These are used only by `read-toplevel-form-from-string`; the rest of Atelier's CST pipeline is unchanged.

4. **Fixture format: YAML front-matter + two documents.** Input `---` Expected. Each fixture produces 3 assertions. Adding a fixture is a single-file operation.

5. **`unexpected-toplevel-form` with `decompose` restart.** PROGN at toplevel is rejected with a restart that decomposes it. Side-effectful forms are rejected with no restart. The allow-list is name-based (`def*`, `define-*`, plus a short explicit list).

## Notes for Strategist retrospective

- **Stories remaining:** S4, S5, S6, S7, S8 — all deferred to Phase 2 (MCP eval layer).
- **Snail tests requiring confirmation:** none.
- **Recommended:** the "source text is the truth" decision should be promoted to a design principle in `product/reference/projectional-editor-design.md` — it governs the entire write path and was the Reviewer's most impactful feedback.
- **Calibration:** 104 assertions actual vs 50–150 predicted. Within range. The fixture format's 3-assertions-per-file multiplier is efficient — 15 files produced 45 assertions. Named tests averaged ~3.7 assertions each.
- **Rework count:** 5 reworks. Patterns: assert-condition order (documented, not re-read — same as slice 009), subprocess reader-time package (same as slice 009 rework #7), write-path architecture (caught by Reviewer). The skim-then-code pattern continues to be the #1 preventable rework source.

---

**Handoff:** [Maker] Phase 1 notes written. Stories remain for Phase 2. Strategist can plan Phase 2 or run the retrospective.
