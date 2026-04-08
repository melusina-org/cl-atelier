# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

`org.melusina.atelier` is a Common Lisp developer toolbox providing:
- **Project scaffolding** via a template system (`new-lisp-project`, `new-lisp-file`)
- **A linter** with 13 inspectors, 10 automatic maintainers, autofix, and pretty-printer
- **License management** (MIT, GPL, CeCILL, Proprietary) with SPDX identifiers

## Common Commands (REPL)

Load the system with Quicklisp:
```lisp
(ql:quickload "org.melusina.atelier")
```

Reload everything (development workflow):
```lisp
(ql:quickload "org.melusina.atelier/development")
(atelier/development:reload)
```

Run all tests:
```lisp
(ql:quickload "org.melusina.atelier/testsuite")
(atelier/testsuite:run-all-tests)
```

Run the linter on this project:
```lisp
(ql:quickload "org.melusina.atelier/development")
(atelier/development:lint)
```

Lint with autofix:
```lisp
(atelier:lint-system "org.melusina.atelier" :autofix t)
```

## ASDF Systems

| System | Purpose |
|--------|---------|
| `org.melusina.atelier` | Main system: scaffolding, linter, inspectors, maintainers, pretty-printer |
| `org.melusina.atelier/testsuite` | Test suite (uses `org.melusina.confidence`) |
| `org.melusina.atelier/development` | Dev helpers: `lint`, `reload` |

## Architecture

### Project Configuration

Each project has two `.sexp` configuration files:
- **`project-configuration.sexp`** — project name, homepage, copyright, license. Declared as `asdf-project-configuration` component, or read directly from system source directory for Atelier's own system.
- **`linter-configuration.sexp`** — disabled inspectors, severity overrides, indentation style, maintainer overrides. Declared as `asdf-linter-configuration` component.

`project-configuration-parameter-bindings` bridges project configuration to the template system's `*parameter-bindings*`.

### Initialization

`(atelier:initialize)` must be called before using templates or licenses. It loads both repositories from `resource/`. The `lint` function and `find-template` call it automatically if needed.

### Parameter System (`src/parameter.lisp`)

Templates use `${PARAMETER_NAME}` placeholders. Parameter names are case- and separator-insensitive (`copyright-holder` = `COPYRIGHT_HOLDER`). `*parameter-bindings*` is the dynamic alist of replacements. `parameter-replace` does topological sorting of bindings to handle inter-parameter references.

### License Repository (`src/license.lisp`, `resource/license/`)

License files are `.text` files with YAML front matter (`name:`, `spdx:`) followed by two documents (header, full text), separated by `---`. Loaded into `*license-repository*` hash table keyed by keyword (e.g., `:mit`). Each license has a `spdx-identifier` slot (e.g., `"MIT"`, `"LicenseRef-Proprietary"`).

### Template Repository (`src/template.lisp`, `resource/template/`)

Two template kinds:
- **`file-template`** — wraps a single `.text` file from `resource/template/`; loaded at `initialize` time
- **`composite-template`** — declared in source with `define-composite-template`; composes other templates into a directory tree

`write-template` dispatches on template type and destination (pathname, stream, or `t` for stdout). `new-lisp-project` is the top-level entry point.

### Linter

**Finding hierarchy:** `finding` > `file-finding` > `line-finding` > `syntax-finding`, plus `region-finding`. Concrete subclasses for each inspector (e.g., `trailing-whitespace-finding`, `bare-lambda-finding`).

**Resolution hierarchy:** `resolution` > `text-resolution` (replacement string) | `syntax-resolution` (CST transform function + optional `cst-node` for transform target) | `agent-resolution` (LLM prompt) | `composite-resolution` (ordered list).

**Inspector registry:** `*inspectors*` hash table. `define-file-inspector`, `define-line-inspector`, `define-syntax-inspector` macros. 13 inspectors:
- File: `check-file-encoding`, `check-spdx-license-header`, `check-header-line`, `check-footer-line`, `check-project-identification`
- Line: `check-trailing-whitespace`, `check-line-length`, `check-mixed-indentation`
- Syntax (CST): `check-earmuffs`, `check-constant-naming`, `check-bare-lambda`, `check-loop-keywords`, `check-labels-for-flet`

**Maintainer registry:** `*maintainers*` hash table with superseding partial order. `define-automatic-maintainer` macro. 10 maintainers:
- `fix-trailing-whitespace`, `fix-mixed-indentation`, `fix-earmuffs`, `fix-constant-naming`, `fix-bare-loop-keywords`, `fix-bare-lambda`, `fix-labels-to-flet`, `fix-header-line`, `fix-footer-line`, `fix-project-identification`

**Maintainer maturity:** Each maintainer has a `:maturity` slot (`:stable` or `:experimental`). Experimental maintainers signal a warning instead of auto-applying in batch mode.

**Autofix signalling:** During `lint-system :autofix t`, each resolution is signalled as `resolution-proposed` with `apply-resolution` and `skip-resolution` restarts. The `linter-configuration` can override per-maintainer disposition (`:auto`, `:interactive`, `:skip`).

**Comment style:** `file-comment-prefix` maps file extensions to comment prefixes: Lisp (`;;;; `), Shell/Make/Docker/Terraform (`# `), C/C++ (`// `), TeX (`% `), Autoconf (`dnl `).

**Write-back engine:** `apply-resolutions` is a generic function on STRING (pure in-memory transform) and PATHNAME (atomic file write). `parse-common-lisp` is a generic on STRING and PATHNAME. `string-to-line-vector` splits content for in-memory use. `resolution-text-span` converts each resolution to `(start end replacement)`.

**Pretty-printer:** `*atelier-pprint-dispatch*` is an isolated dispatch table. `pretty-print-form` emits indented Lisp at a given column.

### Writing Maintainers — Design Guidance

**Maintainers must not reparse files.** The inspection pipeline already parses each file once. Syntax findings carry `cst-node` (the diagnostic node) and `cst-root` (the full file CST). Maintainers should use these — not call `parse-common-lisp` or `read-file-into-line-vector` on the finding's file.

**Use `syntax-resolution` with `cst-node` when the transform target differs from the finding.** A finding's `cst-node` identifies where the problem was detected (for diagnostic display). The resolution's `cst-node` identifies what source span to replace (for the write-back engine). When a maintainer transforms a larger enclosing form (e.g., `fix-bare-lambda` detects the lambda but transforms the enclosing call), set the resolution's `cst-node` to the enclosing form's CST node. The write-back engine uses `(or (resolution-cst-node resolution) (finding-cst-node finding))` for the source span.

**Inspection is fast and frequent; resolution is slow and rare.** Inspectors run on every `lint-system` call. Maintainers run only during `:autofix t`. Design inspectors to be lightweight (no file I/O, no parsing). Maintainers can do more work (walk the CST root, compute call graphs) because they run only on the small number of findings that actually need fixing.

**Prefer `syntax-resolution` over `text-resolution` for CST-level maintainers.** Syntax resolutions let the write-back engine handle position conversion and pretty-printing. Text resolutions require the maintainer to compute line/column positions and generate formatted text — which means re-reading the line vector.

**Refactoring (rename, move) is a separate protocol from linting.** Cross-file operations (renaming a package, renaming a file) are not expressible as inspector/finding/maintainer. Implement them as standalone functions that walk the project, not as inspectors.

### Resource Files (`resource/`)

- `resource/license/*.text` — license definitions with SPDX identifiers
- `resource/template/*.text` — file templates with YAML front matter
- `*resourcedir*` is set at compile/load time from the system source directory, or overridden via `ATELIER_RESOURCEDIR` env var

## Code Conventions

- Every source file begins with `;;;; filename — Description` and ends with `;;;; End of file 'filename'`.
- Every source file has `;;;; SPDX-License-Identifier: MIT` after the copyright block.
- LOOP clause keywords are always keyword symbols (`:for`, `:in`, `:collect`).
- `MAPCAR`/`MAPCAN`/`REMOVE-IF` receive a named `FLET` function, never a bare `LAMBDA`.
- `LABELS` is used only when local functions are mutually or self-recursive; otherwise use `FLET` (with nesting if needed).
- Tests use `define-testcase` from `org.melusina.confidence`.
- Test fixtures live in `testsuite/fixtures/{inspector,maintainer,pretty-print}/` with accessor functions `inspector-fixture`, `maintainer-fixture`, `pretty-printer-fixture`. Maintainer and pretty-printer fixtures are auto-discovered: adding a `.text` file creates a test automatically.
- Maintainer and syntax tests use in-memory parsing (`parse-common-lisp` on string) and in-memory resolution (`apply-resolutions` on string). Only file round-trip tests (UTF-8, atomicity) touch the filesystem.
- No SBCL-specific extensions without an explicit `#+sbcl` guard.
