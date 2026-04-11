# Rework Log

What was reworked, why, and what could have prevented it. The most direct learning signal a project produces.

---

## Slice 001, Phase 1: maintainer protocol evolved three times

**What was reworked:** The maintainer-to-finding coupling mechanism was redesigned twice before landing. First attempt used a `reacts-to` field on the maintainer (registry-level filter by symbol equality). Second attempt used a `resolves` slot (still registry-level, slightly more structured). Third attempt removed the mechanism entirely and let CLOS method dispatch on the finding class do the work: `prepare-resolution` is a generic function, each maintainer is a method specialising on its target finding class.
**Trigger:** The first two designs made the registry responsible for dispatch, which meant every new maintainer required registry surgery, and the registry had to know about finding types it had no business knowing about. The CLOS version gives the compiler the dispatch table for free and keeps registration simple.
**Effort cost:** Significant — three passes over the core protocol — but entirely within the slice.
**Preventable?** Partly. The iteration itself was the design process; the question is whether CLOS-first was considered in the planning interview. Slice 001's interview did not explore the dispatch mechanism deeply because the maintainer role was added late in planning.
**Lesson:** *When designing a protocol between two types in a CLOS codebase, start with method dispatch on one type and ask "why not this?" before introducing any registry-level indirection.* The answer is usually "no reason — we were just thinking procedurally."

## Slice 002, Phase 1: finding subclasses were too generic

**What was reworked:** First implementation had both `check-file-encoding` and `check-spdx-license-header` emitting plain `file-finding` instances. The rework added per-inspector finding subclasses (`encoding-finding`, `spdx-license-header-finding`) and made every maintainer specialise on the specific class, not the parent.
**Trigger:** CLOS dispatch on the parent class `file-finding` cannot distinguish two inspector's outputs, so the dispatch chain (inspector → finding → maintainer) collapsed at the middle link. The first implementation worked in isolation because each test exercised one inspector, but as soon as two maintainers for different findings coexisted, dispatch became ambiguous.
**Effort cost:** Minor — one rework pass, one `define-findings` block added — but the diagnostic cost was high because the symptom was "the wrong maintainer runs," not a compile error.
**Preventable?** Yes. The planning interview had explicitly agreed on the dispatch-chain architecture; the implementation drifted from the agreement. INV-6 encodes the rule as durable text.
**Lesson:** *"Agreed in planning" is not the same as "encoded in a step."* Every architectural agreement must become a concrete acceptance criterion and a concrete step in the plan. See also the pattern "Wrong dispatch mechanism in first implementation" in `patterns.md`.

## Slice 003, Phase 1: pipeline staging added after the fact

**What was reworked:** The initial line-inspector implementation had no staged pipeline — `run-file-inspectors` iterated files, and each line inspector re-opened and re-read the file to do its own line iteration. A design discussion during planning had produced a clear "file inspectors → read lines once → line inspectors" staged design, which was *acknowledged but not recorded* in the plan. The rework added the staged runner, the `inspect-line` generic, and a pipeline reference document.
**Trigger:** The rework was triggered by a performance observation (each line inspector re-reading the same file) and by the absence of any place in the plan where the staged design was captured.
**Effort cost:** Moderate — a core runner refactor in the middle of the slice.
**Preventable?** Yes. The lesson was durable and is now codified in the Tactician role: architectural discussions during planning must be captured in the plan before the interview ends. See slice 004's step-table format for what this looks like when it works.
**Lesson:** *A design decision that lives only in a conversation does not exist for the Maker.* Write it into the plan or throw it away.

## Slice 004, Phase 1: three small reworks around the CST stage

**What was reworked:** (1) `define-syntax-inspector` initially generated `inspect-file` methods instead of `inspect-syntax` methods, copied from a stub. (2) `perform-inspection` initially took configuration as parameters and rebound the same dynamic variables already bound by `lint-system`. (3) `lint-system` and `linter-op` had an awkward dependency: `lint-system` called `asdf:operate` to traverse files. All three reworks happened in one pass.
**Trigger:** (1) The acceptance criterion "`inspect-syntax` must be called with individual CST forms" fired immediately. (2) The redundancy was flagged on review. (3) The dependency was noticed when writing a test that wanted to call `lint-system` without any ASDF operation context.
**Effort cost:** Minor for each; together, a noticeable mid-slice refactor that the retrospective treated as a quality improvement rather than a delay.
**Preventable?** (1) was preventable by taking INV-6 seriously during implementation. (2) and (3) were design improvements that were not visible at planning time — the sort of thing that only becomes obvious when you can read the finished code.
**Lesson:** *Functions that exist entirely inside a dynamic-binding context should read the bindings, not take them as parameters.* And: *if system A delegates to system B only to call back into A, invert the dependency — A is the entry point and B is a convenience wrapper.* Both lessons apply beyond linter code.

## Slice 005, Phase 1: SBCL-pprint mismatch in a fixture

**What was reworked:** The `fix-bare-lambda` maintainer's expected-output fixture did not match SBCL's pretty-printer behaviour for `flet`: SBCL unconditionally breaks the binding body onto a new line, which the plan's expected output did not reflect. The fixture was rewritten to match SBCL's actual output.
**Trigger:** The slice's regression test for `fix-bare-lambda` failed with a character-by-character diff that pointed at the `flet` binding body position.
**Effort cost:** Trivial — one fixture edit.
**Preventable?** Yes, *if* the maintainer author had run the expected output through `pretty-print-form` at planning time. This is the precursor to INV-3 ("the pretty-printer is the single authority on canonical Lisp text") which slice 007 then enforced mechanically for all syntax-level fixtures.
**Lesson:** *When the pretty-printer is in the loop, it is the only valid source of expected output.* Never hand-write a Lisp fixture that a maintainer will produce through pprint — derive it.

## Slice 006, Phase 1: SPDX bulk replacement corrupted fixtures

**What was reworked:** The bulk pass that replaced the verbose 4-line MIT license block with a single `SPDX-License-Identifier: MIT` line across 90 source files also rewrote two test fixtures in `testsuite/fixtures/inspector/check-spdx-license-header/` that *deliberately contained the verbose block as test data*. Fixed by `git checkout`-ing the two fixture files after the bulk pass.
**Trigger:** The post-bulk test run failed on the SPDX license header inspector tests.
**Effort cost:** Trivial — two file restorations — but a real scare.
**Preventable?** Yes. See the pattern "Bulk text replacement corrupts fixtures that deliberately contain the pattern" in `patterns.md`. The mitigation is to grep `testsuite/fixtures/` before any bulk pass and exclude explicitly, or to review `git diff testsuite/` after the pass and selectively restore.
**Lesson:** *Bulk replacements are never safe without a post-pass `git diff` review of the test-fixture paths.* The test suite is the sentinel, but only if it runs against the post-replacement tree; if the replacement breaks the tree so badly that the test system won't load, the sentinel is silent.

---

## Slice 008, Phase 1: two ASDF-file Edit retries

**What was reworked:** Two `Edit` tool calls on `org.melusina.atelier.asd` failed because the `old_string` did not match the file's actual whitespace. First attempt used the Read output's displayed indentation verbatim and failed; second attempt copied the exact tab/space sequence from a narrower `Read` slice and succeeded.
**Trigger:** Indentation in the ASDF file uses tabs, and the two `(:module "inspectors" …)` blocks — one in the main system, one in the testsuite system — nest at different tab depths. The Maker assumed symmetry.
**Effort cost:** Minor — under a minute of friction, one extra `Read` per edit.
**Preventable?** Yes, trivially: when editing a file with mixed or tab-based indentation, read the exact byte sequence before the first edit attempt, not the summarised Read output.
**Lesson:** *Read the bytes, not the render.* The Read tool displays tabs in its own way; for whitespace-sensitive edits, a quick `awk 'NR>=X && NR<=Y' file` or `Grep` on the exact surrounding lines is the safer source of truth.

## Slice 008, Phase 1: acceptance criterion 3 not met literally

**What was reworked:** Nothing in the code — but the acceptance criterion was re-interpreted at review time rather than met as written. The plan said "pass count = 296"; reality was 295.
**Trigger:** Plan counted testcases; reality measured assertions. See `calibration.md` and `patterns.md`.
**Effort cost:** Zero code rework. The friction was in the phase-review phase, where the Reviewer had to mark AC3 as "met in spirit, literal value corrected."
**Preventable?** Yes: acceptance criteria on pass counts should be a *range* or *relative invariant* ("pass count decreases by at most N"), not a tight integer that assumes the plan's counting unit is correct.
**Lesson:** *Tight numeric acceptance criteria are only as good as the unit you counted.* Prefer "no new failures, no new skips, pass count does not decrease by more than X" over "pass count = Y."

## Slice 009, Phase 1: nine reworks, five of them documented CL surprises

**What was reworked:** Nine separate small issues during Phase 2 execution, each caught within one test cycle:
1. `%parse-define-tool-clauses` built a plist and `nreverse`'d it, which reverses pairs too, producing `(value key value key)`. Fixed by building via a hash-table and flattening on exit.
2. `parse-mcp-message` passed `:id nil` to `(make-instance 'initialized-notification ...)`; the notification class has no `id` slot, so the initarg was rejected. Fixed by building the initarg list conditionally.
3. `%lispify-handler-result` first-cut returned `+json-null+` for any `nil`, meaning `maintainer-supersedes` (empty list) encoded as JSON `null`. Fixed by reserving `cl:null` as the explicit null marker and mapping `nil` to `#()` (empty vector → JSON array).
4. `%alist-shape-p` first-cut accepted `(cons-whose-car-is-cons ...)` and misclassified a list-of-alists as an alist. Fixed by checking that every entry has an atomic car.
5. `uiop:xdg-state-home` does not exist in the UIOP shipped with the running image. Fixed with a local `%xdg-state-home-base` shim.
6. Fresh-SBCL subprocess `--eval` form failed at reader time because `asdf:*central-registry*` is read before `(require :asdf)` executes. Fixed by splitting into multiple `--eval` forms and using `find-symbol` to avoid reader-time package resolution.
7. Confidence `assert-t` is strict (expects literal `T`, not generalised boolean). 16 assertions used it with `search`, `fboundp`, `probe-file`, producing false failures on generalised truthy values. Fixed by importing `assert-t*` and switching the affected assertions.
8. Test harness forgot to rebind `*package*` to `#:atelier/mcp` when reading a `define-tool` form from a string, producing tool names like `cl-user:ping` instead of `atelier:ping`. Fixed by the `(let ((*package* ...)) ...)` wrapping pattern.
9. Transcript raw-read test asserted `":marker"` in the written file, but `prin1` prints keywords in uppercase. Fixed by updating the assertion to `":MARKER"`.
**Trigger for each:** Five of the nine (items 2, 3, 4, 7, 9) are CL-language or library-specific surprises that were already documented in `MEMORY.md` or in the Common Lisp skill reference files. The knowledge existed; it was skimmed but not internalised before Phase 2 began.
**Effort cost:** Low per rework (each was one edit + one re-run), significant cumulative cost (9 round-trips through the Maker test cycle).
**Preventable?** Most of them, yes. The append-not-rewrite amendment protocol caught the one real "plan vs. reality" gap (the resources/templates/list spec split) cleanly. But the CL-surprise reworks are a pattern worth watching: the existence of a knowledge file is not enough; the Maker must re-read it before the Phase 2 step it applies to, not skim it during planning and hope to remember.
**Lesson:** See `patterns.md:"Skim-then-code does not work for documented CL surprises"`. The concrete rule is: *before Phase 2 begins, re-read `product/knowledge/MEMORY.md` and every `references/<library>.md` for the slice's dependencies. Not skim — read.* If you cannot name three CL surprises your code is likely to touch, you haven't re-read the knowledge.
