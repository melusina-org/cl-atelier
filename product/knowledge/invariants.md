# Project Invariants

Project-wide constraints that are not in any single plan but apply across the project. Discovered through experience; violations have historically caused rework or incidents.

Numbering is global and continuous across all slices.

---

## INV-1: Discoverable fixtures are a convenience, not a goal

**Discovered:** slice 007, phase 1
**Invariant:** Autofix-cycle fixtures and inspector fixtures under `testsuite/fixtures/` are a testing convenience. If a maintainer's expected output does not fit the fixture format (e.g., semantically meaningful whitespace, or content the pretty-printer would canonicalise differently), the right answer is an ad-hoc `define-testcase` in `testsuite/maintainers/`, **not** a special case in the fixture loader.
**Rationale:** During slice 007, `fix-mixed-indentation` was force-fit into the discoverable format and produced a test that could never round-trip through the pretty-printer. Removing the fixture and moving the maintainer to an ad-hoc testcase was the clean answer. The principle is now in `CLAUDE.md`.

## INV-2: Per-maintainer self-idempotency at N=1 is required

**Discovered:** slice 007, phase 1
**Invariant:** Every registered maintainer must satisfy the property that re-running the `(inspector, maintainer)` pair on the result of a single fix yields the same result. This is enforced by `validate-one-autofix-cycle-fixture` at N=1. Pipeline idempotency (whole-file `lint-system :autofix t` reaching a fixed point) is strictly stronger and is a separate goal on the roadmap.
**Rationale:** Without N=1 self-idempotency, any maintainer can trivially diverge when applied in a loop. Ruff and ESLint both use hard iteration caps (`MAX_ITERATIONS` / `MAX_AUTOFIX_PASSES = 10`) rather than break cycles — Atelier's position is that per-maintainer N=1 is enforced first, pipeline idempotency is built on top. See `product/slice/007-maintainer-and-inspector-expansion/references/linter-convergence.md`.

## INV-3: The pretty-printer is the single authority on canonical Lisp text

**Discovered:** slice 007, phase 1
**Invariant:** For any syntax-level maintainer that emits text, the emitted text must be a `read ⟫ pretty-print-form` fixed point. The pretty-printer owns the one canonical form of any AST, and text-resolution maintainers at the syntax level must match it.
**Rationale:** Slice 007 enforced this as a cross-population test on all syntax-inspector fixtures. It keeps the inspector/maintainer pair from drifting away from the pretty-printer's output, which would make subsequent formatter passes undo the maintainer's work. Line-level fixtures are excluded because their expected whitespace is semantic.

## INV-4: Regression verification must use a fresh SBCL subprocess

**Discovered:** slice 007, phase 1 (root-cause of the `*current-line-vector*` bug)
**Invariant:** Any claim of "tests passing" that supports a slice closure, a release, or a merge must come from a fresh `sbcl --non-interactive` subprocess, **not** from the development REPL image. Live-image reloads mask load-order bugs because the order in which files happen to have been recompiled across editing sessions rarely matches the cold ASDF `:serial` order.
**Rationale:** Slice 007 surfaced the `*current-line-vector*` defvar-in-wrong-file bug that had been present since slices 003–004 but was masked for *months* because every development session had `write-back.lisp` (which held the defvar) already loaded before `runner.lisp` was recompiled. At cold start, `runner.lisp` saw the variable as lexical, not special, and `*current-line-vector*` was NIL inside `let` bindings that should have been dynamic. One fresh subprocess run would have caught it on day one. The discipline is now applied at every slice-level checkpoint.

## INV-5: Atelier does not police line length

**Discovered:** slice 008, phase 1
**Invariant:** Atelier does not report, warn about, or attempt to fix lines that exceed any nominal maximum length. The pretty-printer's `*print-right-margin*` is the only mechanism that influences line length, and its effect is advisory — a form that remains long after pretty-printing is accepted as-is. No `check-line-length`-style inspector and no `fix-line-too-long`-style maintainer should be reintroduced.
**Rationale:** The research in `product/reference/line-length-research.md` surveyed ESLint, Ruff, Black, Prettier, gofmt, rustfmt, and clang-format. The decisive finding was Black's explicit escape hatch — *"in those rare cases, auto-formatted code will exceed your allotted limit"* — which showed that even the most principled structural formatter cannot guarantee line-length satisfaction. gofmt refuses the rule entirely (*"Go has no line length limit"*). Atelier's pretty-printer is Oppen-style and already sophisticated enough to handle structural wrapping via `*print-right-margin*`; adding a separate reporter on top adds noise without adding correctness, and slice 007 had already established that the pretty-printer is the single authority on canonical Lisp text (INV-3). Slice 008 deleted the inspector, the maintainer, the finding class, three testcases, and twenty fixtures. This invariant is a candidate for promotion to a design principle in `product/definition.md` at the next Steward revision, parallel to how slice 007's idempotency invariant was promoted to design principle #7.

## INV-6: One finding subclass per inspector category; CLOS dispatch chain is the architecture

**Discovered:** slice 001 (maintainer protocol evolution); reinforced slice 002 (file-level inspectors)
**Invariant:** Every inspector emits findings of a subclass specific to that inspector's concern — not a generic `file-finding`, `line-finding`, or `syntax-finding`. Maintainers specialise on that specific finding subclass via CLOS methods. The dispatch chain *inspector → specific finding class → maintainer specialised on that class* is the core architectural pattern; any attempt to route by registry lookup, symbol equality, or tag fields is a regression toward something we already tried and rejected.
**Rationale:** Slice 001 iterated through three designs — `reacts-to` field, `resolves` slot, and finally CLOS method dispatch on the finding class — before settling on the current protocol. Slice 002 immediately hit the "but the first implementation used `file-finding` as the direct parent for both file inspectors" rework: without per-inspector subclasses, the CLOS dispatch chain collapses and maintainers cannot discriminate between findings that share a parent category. The `define-finding` / `define-findings` macros exist precisely so that adding a new finding subclass is one data row, not a defclass ceremony, so there is no "it was easier to reuse the parent" excuse.

## INV-7: Inspection context flows through special variables, and `*current-linter-configuration*` may be NIL

**Discovered:** slice 003 (line-level inspectors)
**Invariant:** The inspection pipeline passes file and configuration context through a small set of special variables (`*current-pathname*`, `*current-project-configuration*`, `*current-linter-configuration*`), bound by the runner before it calls any inspector. Inspector bodies read these variables; they must not be added to inspector method signatures. **`*current-linter-configuration*` may legitimately be NIL** (the "no configuration component declared" graceful-default path from slice 004), so any inspector that reads a slot on it must guard against NIL.
**Rationale:** Slice 003 introduced the special-variable mechanism because adding configuration to every `inspect-line` signature would have forced every downstream inspector to accept and ignore parameters it did not use. Slice 004 then made the no-configuration path graceful (warn and default), which means the variable can be bound to NIL at any inspector entry point. Inspectors that assumed non-NIL were rewritten during 004; the lesson is durable.

## INV-8: Write-back to source files is atomic via tmpize + rename-overwriting

**Discovered:** slice 005 (autofix pipeline)
**Invariant:** Any on-disk source file modification made by the autofix write-back engine, or by any future maintainer-adjacent tool, must use `uiop:tmpize-pathname` to build the new contents in a sibling temporary file, then `uiop:rename-file-overwriting-target` to install it. Never open the target file for `:direction :output` and overwrite it in place. Never construct the temporary file outside the target's directory (crossing filesystem boundaries breaks atomic rename).
**Rationale:** Slice 005 explicitly chose atomic write-back as a quality attribute because a partial write mid-pass would corrupt the user's source. The slice verified the atomicity property and documented it. Any future I/O path that bypasses this is a regression.

## INV-9: Eclector CST parsing must read the file as a string first

**Discovered:** slice 005 (autofix pipeline)
**Invariant:** When parsing a Common Lisp source file through Eclector for CST-level inspection or write-back, `parse-lisp-file` (and any successor) must first read the file contents into a string via `alexandria:read-file-into-string` (or equivalent), then parse from a `make-string-input-stream`. **Do not** open the file directly and hand the stream to Eclector. The reason is source-position fidelity: `file-position` on a UTF-8 file stream returns **byte** offsets, while the line-vector infrastructure and the write-back engine work in **character** offsets. Any file containing a multi-byte character in a position before a finding will cause the write-back engine to land its edit at the wrong column.
**Rationale:** Slice 005 hit this bug on files containing `©`, `–`, and `ë` in their copyright headers — the fix-earmuffs maintainer landed its replacement several bytes too early. The fix was one-line (switch to string-input-stream) but the diagnosis took time because the symptom was a silent corruption, not a signalled error. This invariant is the reason Atelier's entire CST pipeline is character-indexed end-to-end.

## INV-10: Autofix is opt-in; default `lint-system` never modifies files

**Discovered:** slice 005 (autofix pipeline)
**Invariant:** `(atelier:lint-system "...")` without `:autofix t` must never modify any file on disk. It walks the source tree, runs inspectors, emits findings to `*standard-output*`, and returns. `:autofix t` is the only switch that enables write-back, and even then the `linter-configuration`'s per-maintainer disposition (`:auto`, `:interactive`, `:skip`) and the `resolution-proposed` signalling protocol give the caller a chance to veto each resolution.
**Rationale:** Slice 005 made autofix opt-in as a safety contract: a developer running `lint-system` to inspect an unfamiliar project must be able to do so without risking changes. A future convenience that silently enables autofix would break this contract and is disallowed.

## INV-12: MCP handler return convention is MIME-type driven

**Discovered:** slice 009, phase 1
**Invariant:** MCP tool and resource handlers return Lisp data structures (alists, plists, lists, numbers, strings) for `:application/json` MIME types, and strings directly for `text/*` MIME types. The dispatcher owns the encoding envelope; handlers never call `jzon:stringify` themselves. The distinction is driven by the `:mime-type` clause in the `define-tool` form, not by the handler's return type.
**Rationale:** Slice 009 first tried the asymmetric design where tool handlers returned Lisp data and resource handlers returned pre-encoded strings; the unification via the `(:resource ...)` dual-expose option collapsed this asymmetry into a single return convention. The macro's expansion is simpler, the handler bodies are shorter, and swapping JSON libraries becomes a one-file change.

## INV-13: `image-connection` generic signatures are stable across slices

**Discovered:** slice 009, phase 1
**Invariant:** The generic function signatures on `image-connection` (`connection-eval`, `connection-shutdown`, `connection-alive-p`) are stable. Subsequent slices may *add* generics (e.g., `connection-interrupt`, `connection-backtrace`) but must not *change* the existing ones. Breaking changes require a new slice with its own risk review and an entry in this file marking the old invariant as superseded.
**Rationale:** Slice 009 ships only the abstract class with no concrete subclass. Slice 010 will add `swank-connection` as the first concrete subclass. If the three signatures drift between slice 009 and slice 010 the protocol breaks before it is used by any real consumer. Pinning them as an invariant makes the drift risk visible and forces future slices to extend rather than mutate.

## INV-14: Every `define-tool` form traces to an exported symbol with an acceptance criterion

**Discovered:** slice 009, phase 1
**Invariant:** Every concrete `define-tool` form in the source tree corresponds to exactly one exported symbol in `#:atelier/mcp` (or a downstream `<name>/mcp` package), and that symbol is referenced by at least one acceptance criterion in some slice. No tool is exported "just in case." The Reviewer audits this at the phase closure of any slice that adds or removes tools.
**Rationale:** Slice 009 started with 85 exports in `#:atelier/mcp`, which is a large surface for a foundation slice and the #1 risk (R1, "hallucinated requirements"). The audit confirmed zero orphans only because every slot and every generic was introduced with a named story. Making this durable is how Atelier avoids the anti-pattern that turns foundation slices into architecture astronauts' playgrounds.

## INV-15: Transcript entries are written atomically per entry

**Discovered:** slice 009, phase 1
**Invariant:** The MCP transcript write protocol is `(prin1 entry stream) (terpri stream) (finish-output stream)`, in that order, per entry. Each entry is a plist serialisable by `read`. A process killed mid-write leaves a file readable up to the last complete entry; a torn tail on the last line is silently skipped by the reader. Any future transcript-adjacent code (trace log, audit stream, replay file) must follow the same per-entry flush discipline.
**Rationale:** Slice 009's acceptance criterion S6.6 ("kill -9 mid-session leaves the file readable up to the last fully-written entry") is the user-visible contract. The three-step write protocol is what makes it true: without the per-entry `finish-output`, buffered output means a killed process can leave a plausible-looking but truncated entry.

## INV-16: Tests that call `define-tool` rebind the registries via `with-isolated-registries`

**Discovered:** slice 009, phase 1
**Invariant:** Any test that invokes `define-tool` or `register-tool` at runtime must wrap its body in `(atelier/testsuite/mcp:with-isolated-registries ...)`, which dynamically rebinds `*tool-registry*`, `*concrete-resource-registry*`, `*template-resource-registry*`, `*input-schema-cache*`, and `*input-schema-source*` to fresh empty hash-tables. No test is allowed to mutate the global production registries persistently. This is the hard fix for the test-registry pollution pattern documented in `patterns.md`; slice 009 does not inherit the "TEST" package heuristic used for inspectors/maintainers.
**Rationale:** Slice 005's retrospective noted that `*inspectors*` and `*maintainers*` were polluted by tests that registered entries then didn't clean them up. The mitigation was a soft filter (`production-resolution-p` excludes maintainers from packages containing `"TEST"`). Slice 009 took the hard fix as a given from day one, and it costs 5 lines of macro plus one import line in every test file. The pattern is portable — when slice 010+ extends the inspector/maintainer side of things, the same approach should replace the `"TEST"` heuristic there too.

## INV-11: Templates under `resource/template/` are API consumers

**Discovered:** slice 001 (discovered late); reinforced by the `#:atelier` nickname bug in slice 006
**Invariant:** The files under `resource/template/*.text` generate new projects that reference Atelier symbols, ASDF system names, and package nicknames. They are a hidden but real consumer of the public API. Any slice that renames an exported symbol, changes an ASDF system name, or adjusts the `#:atelier` nickname must update the templates in the same commit. A grep over `resource/template/` is a mandatory step in any API-rename slice.
**Rationale:** Slice 001 discovered the coupling late — a rename to a template-referenced symbol broke the generator. Slice 006 hit a variant: `resource/template/LISP-ASDF.text` used `#:atelier` (the package nickname) as an ASDF dependency name, which is a distinct namespace from CL package names. The fix was one character per template, but only because grep found the references. Every future API-touching slice must assume templates are in scope until proven otherwise.

## INV-17: Editor loads without MCP dependencies

**Discovered:** slice 010, phase 1
**Invariant:** `org.melusina.atelier/editor` loads in a fresh SBCL without pulling in `com.inuoe.jzon`, `bordeaux-threads`, `closer-mop`, or the `atelier/mcp` package. Enforced by a subprocess test.
**Rationale:** The editor is a standalone library usable by any adapter (MCP, CLI, Emacs integration). If it depends on MCP infrastructure, every consumer inherits those dependencies.

## INV-18: toplevel-form body is Eclector CST preserving reader conditionals

**Discovered:** slice 010, phase 1
**Invariant:** The `toplevel-form` body slot holds an Eclector CST with `#+`/`#-` preserved as `skipped-cst` and `annotated-cons-cst` nodes. Converting to s-expression is a lossy operation available via `toplevel-form-ast`.
**Rationale:** The write path copies non-matching `#+`/`#-` branches verbatim from the original source text using the CST's source ranges. Without structural preservation, cross-platform code would lose branches for non-matching features.

## INV-19: normalize-toplevel-form is idempotent

**Discovered:** slice 010, phase 1
**Invariant:** Applying `normalize-toplevel-form` twice yields the same text as applying it once. Enforced by every canonicalize fixture.
**Rationale:** If normalization is not idempotent, re-saving a file produces spurious diffs. This is the editor's fixed-point contract.

## INV-20: Round-trip write(read(write(read(s)))) = write(read(s))

**Discovered:** slice 010, phase 1
**Invariant:** For any string `s` accepted by `read-toplevel-form-from-string`, the round-trip `write(read(write(read(s)))) = write(read(s))` holds. Enforced by fixtures.
**Rationale:** The write path must be a fixed point — reading its own output and writing again must produce the same text.

## INV-21: child-connection spawns, connects via SWANK, shuts down cleanly

**Discovered:** slice 010, phase 2
**Invariant:** `child-connection` spawns a child SBCL via `uiop:launch-program`, reads a port from its stdout, connects via SWANK over TCP, and shuts down cleanly via `connection-shutdown`. A background drain thread prevents stdout pipe deadlock. No orphan SBCL processes after shutdown. Enforced by child-dependent tests and orphan check.
**Rationale:** Orphan processes waste resources and confuse process-level tests. The drain thread is essential because the child writes compilation messages to stdout (merged with stderr via `:error-output :output`) after the port is read.

## INV-22: canonicalize-form MCP tool runs in parent, no child needed

**Discovered:** slice 010, phase 2
**Invariant:** The `canonicalize-form` MCP tool calls `normalize-toplevel-form` entirely in the parent MCP image. No child connection is created or used. Enforced by a test that runs with `*current-server*` nil.
**Rationale:** Canonicalization is a pure editor operation. It should not require a running child SBCL.

## INV-23: eval-form captures stdout via SWANK :write-string

**Discovered:** slice 010, phase 2
**Invariant:** `eval-form` returns captured stdout from the child eval via SWANK `:write-string` messages accumulated during the eval cycle. Enforced by output-capture test.
**Rationale:** The agent needs to see output from `(format t ...)` calls in evaluated forms.

## INV-24: SWANK debug auto-abort lifecycle

**Discovered:** slice 010, phase 2
**Invariant:** When SWANK enters the debugger during eval: (1) the server sends `:debug` (with condition text), (2) then `:debug-activate` (debugger ready), (3) the client sends `invoke-nth-restart-for-emacs` to abort, (4) the server sends `:return` for the abort's continuation ID, (5) the original eval's `:return` **never arrives**. The client must track the abort-id separately and signal error when its `:return` arrives. Encoded in SWANK exploratory tests.
**Rationale:** This non-obvious lifecycle caused a blocking hang in slice 010. The original eval's `:return` does not arrive after abort — this is SWANK's behavior, not a bug.

## INV-25: run-tests-fresh uses separate SBCL, not session child

**Discovered:** slice 010, phase 2
**Invariant:** `run-tests-fresh` spawns a separate SBCL subprocess via `uiop:run-program` (synchronous, captures output). The session child is untouched. Enforced by test.
**Rationale:** Fresh-start testing must be hermetic — no compiled state from prior evals.

## INV-26: eval-form returns debug state, not isError, on debugger entry

**Discovered:** slice 011, phase 1
**Invariant:** When a child eval enters the SWANK debugger, `eval-form` returns a JSON object with `in_debugger: true`, condition, restarts, backtrace, and level. The `isError` field is false. The agent can act on the debug state without the MCP client treating it as a failure.
**Rationale:** Auto-aborting on error (slice 010 behavior) hides the restart options from the agent. Returning debug state lets the agent choose how to proceed.

## INV-27: eval-form rejects calls while debugger is active

**Discovered:** slice 011, phase 1
**Invariant:** When `connection-debug-state` is non-nil, `eval-form` signals `debugger-active` immediately. One eval at a time per session.
**Rationale:** Overlapping evals while the SWANK debugger is active would deadlock.

## INV-28: swank-eval returns status keyword as first value

**Discovered:** slice 011, phase 1
**Invariant:** `swank-eval` returns `(VALUES :ok result output)` on success or `(VALUES :debug debug-state output)` on debugger entry. `connection-eval` auto-aborts on `:debug` for backward compatibility.
**Rationale:** The status keyword lets callers choose between auto-abort (connection-eval) and debug-state exposure (eval-form tool).

## INV-29: SWANK debug requests require the debug thread ID

**Discovered:** slice 011, phase 1
**Invariant:** All SWANK requests during an active debug session (`backtrace`, `invoke-nth-restart-for-emacs`, `eval-string-in-frame`) must use the thread ID from the `:debug` message, not `t`. Using `t` causes the request to hang.
**Rationale:** SWANK dispatches `:emacs-rex` per-thread. The debug thread is blocked in the debugger loop; only requests addressed to it reach the debug context.

## INV-30: Post-abort drain with throw-to-toplevel

**Discovered:** slice 011, phase 1
**Invariant:** After a SWANK abort restart, `%drain-post-abort-messages` must consume `:debug-return`, `:debug`, and `:debug-activate` messages and send `(swank:throw-to-toplevel)` (0 args, on the debug thread) to force return to the REPL loop. Without this, the next `swank-eval` reads stale messages.
**Rationale:** SWANK re-enters the debugger after abort. The post-abort drain is essential for connection reuse.

## INV-31: Confidence testcase property key is :ORG.MELUSINA.CONFIDENCE/TESTCASE

**Discovered:** slice 012, phase 1
**Invariant:** Confidence's `define-testcase` marks symbols with the property `:org.melusina.confidence/testcase` (a keyword, not an internal symbol). Testcase discovery scans `symbol-plist` for this key.
**Rationale:** Initial assumption was `confidence::testcase` (internal symbol). The actual key was found by macroexpanding `define-testcase` → `set-testcase-properties` → `(setf (get sym :org.melusina.confidence/testcase) t)`.

## INV-32: system-apropos must search both *source-registry* and registered-systems

**Discovered:** slice 012, phase 1
**Invariant:** ASDF system search must cover `asdf/source-registry:*source-registry*` (disk-scanned systems) AND `asdf:registered-systems` (already-loaded systems). Systems loaded via `*central-registry*` do not appear in `*source-registry*`.
**Rationale:** The Atelier project directory is pushed to `*central-registry*`, not to the source registry configuration. Searching only `*source-registry*` missed all Atelier systems.

## INV-33: HyperSpec tools read only from the local filesystem

**Discovered:** slice 013, phase 1
**Invariant:** All HyperSpec tools (`hyperspec-lookup`, `hyperspec-issue`, `hyperspec-issues`) read from the local MacPorts installation at `/opt/local/share/doc/lisp/HyperSpec-7-0/HyperSpec/`. They never make network requests.
**Rationale:** Network requests would add latency, require error handling for connectivity, and introduce a dependency on an external service. The local installation is complete and authoritative.

## INV-34: HyperSpec tools gracefully unavailable when not installed

**Discovered:** slice 013, phase 1
**Invariant:** When the local HyperSpec installation is absent, `hyperspec-available-p` returns NIL and HyperSpec tests skip gracefully. Tools return an `mcp-error` with an installation instruction.
**Rationale:** Not every developer machine has MacPorts lisp-hyperspec installed. The MCP server must start and function for all other tools even when HyperSpec is unavailable.

## INV-35: Tool names never shadow CL exports

**Discovered:** slice 013, phase 1
**Invariant:** Every `define-tool` name must not be an external symbol of `COMMON-LISP`. The `define-tool` macro creates `NAME-TOOL` which requires interning `NAME` in the current package; if that package uses CL and `NAME` is a CL export, SBCL raises a package lock violation.
**Rationale:** `define-tool apropos` failed because `CL:APROPOS` is exported. Renamed to `apropos-search`. See PAT-11.
