# Project Invariants

Project-wide constraints that are not in any single plan but apply across the project. Discovered through experience; violations have historically caused rework or incidents.

Numbering is global and continuous across all slices.

---

## INV-1: Discoverable fixtures are a convenience, not a goal

**Discovered:** slice 007, phase 1
**Invariant:** Autofix-cycle fixtures and inspector fixtures under `test/fixtures/` are a testing convenience. If a maintainer's expected output does not fit the fixture format (e.g., semantically meaningful whitespace, or content the pretty-printer would canonicalise differently), the right answer is an ad-hoc `define-testcase` in `test/maintainers/`, **not** a special case in the fixture loader.
**Rationale:** During slice 007, `fix-mixed-indentation` was force-fit into the discoverable format and produced a test that could never round-trip through the pretty-printer. Removing the fixture and moving the maintainer to an ad-hoc testcase was the clean answer. The principle is now in `CLAUDE.md`.

## INV-2: Per-maintainer self-idempotency at N=1 is required

**Discovered:** slice 007, phase 1
**Invariant:** Every registered maintainer must satisfy the property that re-running the `(inspector, maintainer)` pair on the result of a single fix yields the same result. This is enforced by `validate-one-autofix-cycle-fixture` at N=1. Pipeline idempotency (whole-file `lint-system :autofix t` reaching a fixed point) is strictly stronger and is a separate goal on the roadmap.
**Rationale:** Without N=1 self-idempotency, any maintainer can trivially diverge when applied in a loop. See `product/slice/007-maintainer-and-inspector-expansion/references/linter-convergence.md`.

## INV-3: The pretty-printer is the single authority on canonical Lisp text

**Discovered:** slice 007, phase 1
**Invariant:** For any syntax-level maintainer that emits text, the emitted text must be a `read > pretty-print-form` fixed point. The pretty-printer owns the one canonical form of any AST, and text-resolution maintainers at the syntax level must match it.
**Rationale:** Slice 007 enforced this as a cross-population test on all syntax-inspector fixtures. Line-level fixtures are excluded because their expected whitespace is semantic.

## INV-4: Regression verification must use a fresh SBCL subprocess

**Discovered:** slice 007, phase 1 (root-cause of the `*current-line-vector*` bug)
**Invariant:** Any claim of "tests passing" that supports a slice closure, a release, or a merge must come from a fresh `sbcl --non-interactive` subprocess, **not** from the development REPL image. Live-image reloads mask load-order bugs because the order in which files happen to have been recompiled across editing sessions rarely matches the cold ASDF `:serial` order.
**Rationale:** Slice 007 surfaced the `*current-line-vector*` defvar-in-wrong-file bug that had been present since slices 003-004 but was masked in the development REPL.

## INV-5: Atelier does not police line length

**Discovered:** slice 008, phase 1
**Invariant:** Atelier does not report, warn about, or attempt to fix lines that exceed any nominal maximum length. The pretty-printer's `*print-right-margin*` is the only mechanism that influences line length, and its effect is advisory. No `check-line-length`-style inspector and no `fix-line-too-long`-style maintainer should be reintroduced.
**Rationale:** The research in `product/reference/line-length-research.md` surveyed ESLint, Ruff, Black, Prettier, gofmt, rustfmt, and clang-format. The pretty-printer is the single authority on canonical Lisp text (INV-3).

## INV-6: One finding subclass per inspector category; CLOS dispatch chain is the architecture

**Discovered:** slice 001 (maintainer protocol evolution); reinforced slice 002 (file-level inspectors)
**Invariant:** Every inspector emits findings of a subclass specific to that inspector's concern — not a generic `file-finding`, `line-finding`, or `syntax-finding`. Maintainers specialise on that specific finding subclass via CLOS methods.
**Rationale:** Slice 001 iterated through three designs before settling on CLOS method dispatch on the finding class.

## INV-7: Inspection context flows through special variables, and `*current-linter-configuration*` may be NIL

**Discovered:** slice 003 (line-level inspectors)
**Invariant:** The inspection pipeline passes file and configuration context through special variables (`*current-pathname*`, `*current-project-configuration*`, `*current-linter-configuration*`), bound by the runner before it calls any inspector. **`*current-linter-configuration*` may legitimately be NIL**.
**Rationale:** Slice 003 introduced the special-variable mechanism. Slice 004 made the no-configuration path graceful (warn and default).

## INV-8: Write-back to source files is atomic via tmpize + rename-overwriting

**Discovered:** slice 005 (autofix pipeline)
**Invariant:** Any on-disk source file modification made by the autofix write-back engine must use `uiop:tmpize-pathname` + `uiop:rename-file-overwriting-target`. Never overwrite in place. Never construct the temporary file outside the target's directory.
**Rationale:** Slice 005 explicitly chose atomic write-back as a quality attribute.

## INV-9: Eclector CST parsing must read the file as a string first

**Discovered:** slice 005 (autofix pipeline)
**Invariant:** When parsing a Common Lisp source file through Eclector for CST-level inspection, first read the file contents into a string, then parse from a `make-string-input-stream`. **Do not** open the file directly. The reason is source-position fidelity: `file-position` on a UTF-8 file stream returns **byte** offsets, while the write-back engine works in **character** offsets.
**Rationale:** Slice 005 hit this bug on files containing multi-byte characters in copyright headers.

## INV-10: Autofix is opt-in; default `lint-system` never modifies files

**Discovered:** slice 005 (autofix pipeline)
**Superseded by INV-15:** slice 011 (linter API cleanup) replaced `lint-system` with `lint` and made `:action :fix` the DWIM default. The "no side effects" contract is now expressed by `:action :inspect` (findings only) or `:action :preview` (planned resolutions, no write-back); the old boolean opt-in design has been retired.
**Original invariant (historical):** `(atelier:lint-system "...")` without `:autofix t` must never modify any file on disk. `:autofix t` is the only switch that enables write-back.
**Original rationale:** Slice 005 made autofix opt-in as a safety contract.

## INV-11: Templates under `resource/template/` are API consumers

**Discovered:** slice 001 (discovered late); reinforced by the `#:atelier` nickname bug in slice 006
**Invariant:** The files under `resource/template/*.text` generate new projects that reference Atelier symbols, ASDF system names, and package nicknames. Any slice that renames an exported symbol, changes an ASDF system name, or adjusts a nickname must update the templates in the same commit.
**Rationale:** Slice 006 hit a variant where a template used a package nickname as an ASDF dependency name.

## INV-12: System naming inspector operates on .asd files only

**Discovered:** slice 009, phase 1
**Invariant:** `check-system-naming` and `check-test-mirror` dispatch on `(string-equal "asd" (pathname-type pathname))`. They must not run on `.lisp` files.
**Rationale:** ASDF system definitions live exclusively in `.asd` files. Running the system naming inspector on `.lisp` files would find no `defsystem` forms and waste time.

## INV-13: Findings that need text-resolution must be line-finding subclasses

**Discovered:** slice 009, phase 1
**Invariant:** The write-back engine's `resolution-text-span` for `text-resolution` derives character offsets from `(finding-line, finding-column)` to `(finding-end-line, finding-end-column)`. A `file-finding` (which lacks these slots) cannot produce a `text-resolution` — it will signal `no-applicable-method` at write-back time.
**Rationale:** The original plan used `file-finding` for deprecated name findings. The first test run crashed because `text-resolution` called `finding-line` on a `file-finding`. Changed to `line-finding` subclasses.

## INV-14: Pre-commit hook is a self-contained POSIX shell script

**Discovered:** slice 009, phase 1
**Invariant:** The pre-commit hook installed by `install-pre-commit-hook` uses `#!/bin/sh`, `set -eu`, and calls `sbcl --non-interactive --load <asd-file>`. It does not depend on Quicklisp being configured or on any specific ASDF source registry.
**Rationale:** Pre-commit hooks must work in CI environments and fresh clones where Quicklisp may not be configured.

## INV-15: `LINT` decomposes into four composable primitives

**Discovered:** slice 011, phase 1
**Invariant:** `(atelier:lint system :action :fix :scope :system)` is semantically equivalent to the ordered composition

    (apply-lint-resolutions
     (plan-resolutions
      (inspect-lint-files
       (collect-lint-files system :scope :system))))

iterated to convergence (with the interactive-acceptance layer that `lint :action :fix` adds on top). Any future refactor must preserve this equivalence so callers who need the pipeline without the orchestrator (CI dry-run reporters, MCP tools, LSP endpoints, custom batch scripts) can reach for the primitives without losing DWIM semantics. The invariant is enforced by `validate-lint-composes-primitives` in `test/lint.lisp`.
**Rationale:** Slice 011 retired the monolithic `lint-system` because its two boolean flags (`:autofix`, `:sibling-systems`) could not be composed and the function body locked a three-stage pipeline behind an opaque surface. Splitting the pipeline without preserving the equivalence would make the DWIM default drift from the primitive chain — the precise failure mode that motivated the split.
