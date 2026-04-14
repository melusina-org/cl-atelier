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

## Slice 010, Phase 1: write-path architecture rework

**What was reworked:** The original write path (`write-toplevel-form-to-string`) reconstructed output by pretty-printing matching CST branches and appending skipped `#+`/`#-` regions. This lost the `#+sbcl` prefix on matching branches because Eclector evaluates `#+` transparently. Reviewer wrote tests asserting `(search "#+sbcl" output)` which failed.
**Trigger:** Reviewer feedback. Tests explicitly checked for feature-guard preservation.
**Effort cost:** Significant — 89 lines removed, 32 added. But the resulting design is simpler: copy the body's source range verbatim from the original string when `source-text` is available.
**Preventable?** Partly. The CST-to-text reconstruction approach assumed Eclector's CST retained the `#+` prefix on matching branches, which it doesn't (it evaluates `#+` at read time). An exploratory test of Eclector's behavior would have caught this before the write-path design was committed.
**Lesson:** *When your design depends on what a library preserves or discards, write a test for that assumption before building on it.*

## Slice 010, Phase 2: interactive-eval does not work for CL clients

**What was reworked:** `swank-eval` initially used `swank:interactive-eval` (exported, 3 args: STRING LINES WIDTH). This entered the debugger because the function depends on Emacs echo-area state. Fixed: use `swank:eval-and-grab-output` (1 arg: STRING, returns `("output" "result")`).
**Trigger:** First SWANK eval test entered the debugger immediately.
**Effort cost:** Minor — 1 edit. But the diagnosis required creating the SWANK exploratory test system.
**Preventable?** Yes, by testing the SWANK function in isolation before wiring it into production code.
**Lesson:** *Never use an API function in production before verifying its behavior in an exploratory test.* Especially true for internal or semi-documented APIs like SWANK.

## Slice 010, Phase 2: pipe deadlock from child stderr

**What was reworked:** `make-child-connection` used `:error-output :stream` (separate pipe). The child's stderr pipe buffer filled with compilation messages, blocking the child before it could print the port to stdout. Three iterations: (1) `:error-output nil` (discard — user rejected), (2) `:error-output :output` (merge — still deadlocks after port is read), (3) `:error-output :output` + background drain thread (final fix).
**Trigger:** Full test suite hung indefinitely. Diagnosed via I/O exploratory tests.
**Effort cost:** 3 iterations. The I/O exploratory test system was created during this rework and immediately proved its value.
**Preventable?** Yes, by understanding pipe buffer behavior upfront. The I/O exploratory tests now encode this knowledge permanently.
**Lesson:** *Any `uiop:launch-program` with `:output :stream` must drain all streams, not just the one you're reading.* Create an I/O exploratory test before designing the pipe architecture.

## Slice 010, Phase 2: SWANK debug auto-abort blocks

**What was reworked:** After sending `invoke-nth-restart-for-emacs` to abort the debugger, the code waited for the original eval's `:return` (continuation ID 1). This never arrives — SWANK only sends `:return` for the abort restart's continuation ID (2). Fixed: track `abort-id` separately; when its `:return` arrives, signal error with captured condition text.
**Trigger:** `validate-swank-eval-error` test timed out. Diagnosed via SWANK exploratory tests.
**Effort cost:** Restructured the `swank-eval` message loop. Medium effort.
**Preventable?** Yes, by testing the debug lifecycle in an exploratory test before building the auto-abort handler.
**Lesson:** *SWANK's debug lifecycle is non-obvious: the original eval's :return does not arrive after abort.* This is now documented in INV-24 and tested in `testsuite/swank/wire-protocol.lisp`.

## Slice 011, Phase 1: CL package lock on invoke-restart

**What was reworked:** Tool `invoke-restart` renamed to `select-restart`.
**Trigger:** SBCL compile error — `define-tool invoke-restart` tried to intern `INVOKE-RESTART-TOOL` in `atelier/mcp` which uses `common-lisp`.
**Effort cost:** Minor — file rename + grep-replace in tests.
**Preventable?** Yes, by checking `(find-symbol "INVOKE-RESTART" :cl)` before naming the tool.
**Lesson:** *Check CL symbol conflicts before naming `define-tool` forms.*

## Slice 011, Phase 1: swank-eval return convention breaks callers

**What was reworked:** `connection-eval` method on `child-connection` updated to unpack the new `(VALUES :ok result output)` return convention.
**Trigger:** Existing tests broke because `connection-eval` returned `:ok` as the "result" string.
**Effort cost:** Minor — 1 method rewrite.
**Preventable?** Yes, by tracing all callers of `swank-eval` before changing its return type.
**Lesson:** *When changing a function's return convention, update the full caller graph in the same step.*

## Slice 011, Phase 1: SWANK debug thread ID required

**What was reworked:** All SWANK debug functions (backtrace, invoke-restart, eval-in-frame) gained a `:thread` keyword parameter, and all callers updated to pass the thread from `debug-state`.
**Trigger:** Backtrace tool test hung. Diagnosed by reading the `:debug` message and noticing the thread ID field.
**Effort cost:** Medium — added `:thread` keyword to 3 SWANK functions + 4 tool callers.
**Preventable?** Partly — the prototype notes mentioned thread dispatch, but didn't make the thread ID requirement explicit for debug-context operations.
**Lesson:** *SWANK dispatches :emacs-rex per-thread. Debug-context requests MUST use the debug thread ID.*

## Slice 011, Phase 1: Post-abort message drain

**What was reworked:** Added `%drain-post-abort-messages` function with `throw-to-toplevel`.
**Trigger:** Repeated debug/abort/eval cycles hung on the second eval.
**Effort cost:** Significant — new function, protocol investigation, throw-to-toplevel signature fix.
**Preventable?** No — this is undocumented SWANK behavior.
**Lesson:** *SWANK re-enters the debugger after abort. Always drain post-abort messages and send throw-to-toplevel.*

## Slice 011, Phase 1: throw-to-toplevel takes 0 arguments

**What was reworked:** Changed `(swank:throw-to-toplevel THREAD)` to `(swank:throw-to-toplevel)`.
**Trigger:** SWANK entered a level-2 debugger with "invalid number of arguments: 1".
**Effort cost:** Trivial — 1 format string edit.
**Preventable?** Yes, by checking the SWANK source for the function signature.
**Lesson:** *Always verify SWANK function signatures via exploratory test or source inspection.*

## Slice 012, Phase 1: Confidence testcase property key

**What was reworked:** `list-testcases-data` initially looked for `confidence::testcase` as the symbol property key. The actual key is `:org.melusina.confidence/testcase` (a keyword).
**Trigger:** `list-testcases-data` returned NIL for packages with known testcases.
**Effort cost:** Trivial — 1 line change.
**Preventable?** Yes, by macroexpanding `define-testcase` before writing the discovery function.
**Lesson:** *Never assume a library's internal conventions — macroexpand and inspect.*

## Slice 012, Phase 1: system-apropos missed *central-registry* systems

**What was reworked:** `system-apropos` initially searched only `asdf/source-registry:*source-registry*`. Systems loaded via `asdf:*central-registry*` (like Atelier itself in the test environment) were not in that hash table.
**Trigger:** `system-apropos "atelier"` returned empty list.
**Effort cost:** Minor — added `asdf:registered-systems` search.
**Preventable?** Yes, by understanding the two ASDF system discovery mechanisms.
**Lesson:** *ASDF has two discovery paths: source-registry (directory scanning) and central-registry (explicit directory list). Search both.*

## Slice 010, Phase 2: sb-ext not portable

**What was reworked:** `swank-protocol.lisp` used `sb-ext:string-to-octets` and `sb-ext:octets-to-string`. User feedback: "avoid SB-EXT, use well-established libraries." Fixed: replaced with `flexi-streams:string-to-octets` / `flexi-streams:octets-to-string`. Added `flexi-streams` to MCP dependencies.
**Trigger:** User review feedback.
**Effort cost:** Mechanical — find-and-replace + dependency addition.
**Preventable?** Yes, by defaulting to portable libraries from the start.
**Lesson:** *Default to portable well-established libraries (flexi-streams, alexandria, bordeaux-threads, closer-mop, usocket) instead of implementation-specific extensions.* SB-EXT is a convenience; portability is a design principle.

## Slice 013, Phase 1: apropos → apropos-search (CL package lock)

**What was reworked:** `define-tool apropos` triggered SBCL package lock violation because `CL:APROPOS` is exported from COMMON-LISP. Interning `APROPOS-TOOL` in `atelier/mcp` (which uses CL) tripped the lock. Renamed to `apropos-search`.
**Trigger:** PAT-11 (CL package lock on define-tool names) — known pattern, but the plan used the bare name anyway.
**Effort cost:** Minor — file rename + 2 test string replacements, ~2 minutes.
**Preventable?** Yes, by checking `(find-symbol "APROPOS" :cl)` during planning. The pattern knowledge file explicitly warns about this.
**Lesson:** *During planning, grep CL exports for every tool name. `(do-external-symbols (s :cl) ...)` or `(find-symbol NAME :cl)` catches conflicts before compile time.*

## Slice 014, Phase 1: *current-server* defvar ordering

**What was reworked:** `(defvar *current-server* nil)` was in `eval-form.lisp` (tools module) but bound by `(let* ((*current-server* server) ...))` in `dispatcher.lisp` which compiles BEFORE the tools module. The `let` created a lexical binding, not dynamic. Live MCP protocol returned "No server context" on every tool call requiring a child.
**Trigger:** Tests passed because they bind `*current-server*` themselves (compiled after the defvar). Only the real MCP protocol path through the dispatcher was broken.
**Effort cost:** Minor — move defvar to `dispatcher.lisp`, remove from `eval-form.lisp`, add regression test.
**Preventable?** Yes, by having a journey test that exercises the real MCP protocol path (tools/call → dispatcher → tool handler), not just direct `handle-tool-call` calls.
**Lesson:** *Defvar for special variables used in dynamic bindings must be compiled BEFORE the binding site. Tests that bypass the production code path (calling handle-tool-call directly instead of through the dispatcher) can't catch binding bugs.*

## Slice 014, Phase 1: sb-introspect:who-* returns conses

**What was reworked:** Code treated `sb-introspect:who-calls` results as bare `definition-source` objects. They are actually `(SYMBOL . DEFINITION-SOURCE)` conses. Crashed with TYPE-ERROR at runtime.
**Trigger:** PAT-12 violated — no exploratory test verifying SBCL API shape before production use.
**Effort cost:** Minor — destructure `(car entry)` and `(cdr entry)`.
**Preventable?** Yes, by writing an exploratory test for `sb-introspect:who-calls` before implementing the child-worker function. Pattern PAT-12 explicitly warns about this.
**Lesson:** *Always verify external API return shapes with an exploratory test. `sb-introspect` documentation doesn't specify the return type clearly enough to trust.*

## Slice 014, Phase 1: connection-alive-p doesn't detect broken SWANK

**What was reworked:** Not yet reworked — documented as INV-38 for slice 015. `connection-alive-p` checks `uiop:process-alive-p` but not SWANK socket health. When SWANK dies but the process lives, `ensure-child-connection` reuses the dead connection. Every subsequent eval-form returns "broken pipe" until MCP client reconnects.
**Trigger:** Nested child spawning during MCP tests crashed the SWANK socket inside the child.
**Effort cost:** Significant — required MCP reconnection every time the bug triggered during development, ~5 reconnections in this session.
**Preventable?** Yes, by catching `broken-pipe` errors in `connection-eval` and treating them as dead-connection signals.
**Lesson:** *Health checks must verify the protocol channel, not just the process. A live process with a dead socket is worse than a dead process — the dead process triggers respawn, the dead socket causes persistent errors.*
