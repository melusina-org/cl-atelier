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

## INV-11: Templates under `resource/template/` are API consumers

**Discovered:** slice 001 (discovered late); reinforced by the `#:atelier` nickname bug in slice 006
**Invariant:** The files under `resource/template/*.text` generate new projects that reference Atelier symbols, ASDF system names, and package nicknames. They are a hidden but real consumer of the public API. Any slice that renames an exported symbol, changes an ASDF system name, or adjusts the `#:atelier` nickname must update the templates in the same commit. A grep over `resource/template/` is a mandatory step in any API-rename slice.
**Rationale:** Slice 001 discovered the coupling late — a rename to a template-referenced symbol broke the generator. Slice 006 hit a variant: `resource/template/LISP-ASDF.text` used `#:atelier` (the package nickname) as an ASDF dependency name, which is a distinct namespace from CL package names. The fix was one character per template, but only because grep found the references. Every future API-touching slice must assume templates are in scope until proven otherwise.
