# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

`org.melusina.atelier` is a Common Lisp developer toolbox providing:
- **Project scaffolding** via a template system (`new-lisp-project`, `new-lisp-file`)
- **A linter** with 17 inspectors, 15 automatic maintainers, autofix, and pretty-printer
- **License management** (MIT, GPL, CeCILL, Proprietary) with SPDX identifiers
- **MCP server** (slices 009–014) — `org.melusina.atelier/mcp` exposes Atelier state over the Model Context Protocol via stdio with 41 tools, 4 concrete resources, and 7 URI-templated resources. Entry point: `(atelier/mcp:serve-two-way-stream)`. Slice 010 added child SBCL management via SWANK (`child-connection`), eval-form, canonicalize-form, package/symbol introspection, and testsuite runner tools. Child-side code lives in `org.melusina.atelier/child-worker`. Slice 011 added live debugger access: when `eval-form` encounters an error, it returns the debug state (condition, restarts, backtrace) instead of auto-aborting. Four new tools: `select-restart` (choose a restart), `abort-debug` (abort current debug), `backtrace-frames` (get full backtrace), `eval-in-frame` (evaluate in a frame's context). Slice 012 added ASDF/Quicklisp/Confidence integration: `quickload` (load systems via Quicklisp), `system-info` (system metadata), `system-apropos` (search systems), `list-testcases` (discover Confidence testcases), `run-testcase` (run individual testcases). Slice 013 added documentation tools: `apropos` (symbol search across packages), `hyperspec-lookup` (local CLHS dictionary entry), `hyperspec-issue` (X3J13 issue writeup), `hyperspec-issues` (list all X3J13 issues), `macroexpand-form` (macroexpand in child), `disassemble-symbol` (disassemble in child), `compile-form` (compile with diagnostics in child). HyperSpec tools read from local MacPorts installation only (INV-33). Slice 014 added xref/introspection tools: `who-calls`, `who-references`, `who-binds`, `who-specializes`, `who-macroexpands` (SBCL xref queries), `inspect-class` (CLOS class inspector via closer-mop), `trace-function`/`untrace-function` (runtime tracing), `who-tests` (find Confidence testcases calling a function), `run-impacted` (discover and run impacted tests).
- **Projectional editor** (slice 010) — `org.melusina.atelier/editor` (package `atelier/editor`) represents toplevel CL forms as `toplevel-form` records (4 slots: kind, name, body, eval-when) with an Eclector CST body preserving `#+`/`#-` as structure. Entry point: `(atelier/editor:normalize-toplevel-form form)` runs the lint + maintainer pipeline and returns `(values normalized-form findings)`. Read: `read-toplevel-form-from-string`. Write: `write-toplevel-form-to-string`. Also adds `atelier:lint-string` to core for in-memory lint pipeline execution.

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
| `org.melusina.atelier/editor` | Projectional editor: `toplevel-form` record, `read-toplevel-form-from-string`, `write-toplevel-form-to-string`, `normalize-toplevel-form`. Depends only on core atelier (no MCP, no jzon, no bordeaux-threads). |
| `org.melusina.atelier/child-worker` | Child SBCL worker: SWANK startup, introspection helpers (`list-packages-data`, `describe-symbol-data`, etc.). Loaded in the child image. Depends on `closer-mop`. |
| `org.melusina.atelier/mcp` | MCP server: stdio JSON-RPC, 14 tools + 3 concrete resources + 5 templates. SWANK wire protocol client, `child-connection` for child SBCL management. Depends on `com.inuoe.jzon`, `bordeaux-threads`, `usocket`, `flexi-streams`, and `org.melusina.atelier/editor`. |
| `org.melusina.atelier/testsuite/mcp` | MCP test suite |
| `org.melusina.atelier/testsuite/input-output` | Exploratory tests for UNIX pipe I/O behavior (not in main suite) |
| `org.melusina.atelier/testsuite/swank` | Exploratory tests for SWANK wire protocol (not in main suite) |

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

**Inspector registry:** `*inspectors*` hash table. `define-file-inspector`, `define-line-inspector`, `define-syntax-inspector` macros. 15 inspectors:
- File: `check-file-encoding`, `check-spdx-license-header`, `check-header-line`, `check-footer-line`, `check-project-identification`
- Line: `check-trailing-whitespace`, `check-mixed-indentation`
- Syntax (CST): `check-earmuffs`, `check-constant-naming`, `check-bare-lambda`, `check-loop-keywords`, `check-labels-for-flet`, `check-single-branch-if`, `check-single-form-progn`, `check-when-not`

**Maintainer registry:** `*maintainers*` hash table with superseding partial order. `define-automatic-maintainer` macro. 10 maintainers:
- `fix-trailing-whitespace`, `fix-mixed-indentation`, `fix-earmuffs`, `fix-constant-naming`, `fix-bare-loop-keywords`, `fix-bare-lambda`, `fix-labels-to-flet`, `fix-header-line`, `fix-footer-line`, `fix-project-identification`

**Maintainer maturity:** Each maintainer has a `:maturity` slot (`:stable` or `:experimental`). Experimental maintainers signal a warning instead of auto-applying in batch mode.

**Autofix signalling:** During `lint-system :autofix t`, each resolution is signalled as `resolution-proposed` with `apply-resolution` and `skip-resolution` restarts. The `linter-configuration` can override per-maintainer disposition (`:auto`, `:interactive`, `:skip`).

**Comment style:** `file-comment-prefix` maps file extensions to comment prefixes: Lisp (`;;;; `), Shell/Make/Docker/Terraform (`# `), C/C++ (`// `), TeX (`% `), Autoconf (`dnl `).

**Write-back engine:** `apply-resolutions` is a generic function on STRING (pure in-memory transform) and PATHNAME (atomic file write). `parse-common-lisp` is a generic on STRING and PATHNAME. `string-to-line-vector` splits content for in-memory use. `resolution-text-span` converts each resolution to `(start end replacement)`.

**Pretty-printer:** `*atelier-pprint-dispatch*` is an isolated dispatch table with Atelier-specific overrides (e.g. `when`/`unless` body indentation). `pretty-print-form` emits indented Lisp at a given column. Pprint dispatch functions must use explicit `pprint-logical-block`, `pprint-newline`, `pprint-indent`, and `write` calls — never `FORMAT` control strings (`~:<`, `~:@_`, `formatter`, etc.) which are unreadable and hard to debug.

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
- Test fixtures live in `testsuite/fixtures/{inspector,autofix,pretty-print}/` with accessor functions `inspector-fixture`, `autofix-cycle-fixture`, `pretty-printer-fixture`. Autofix-cycle and pretty-printer fixtures are auto-discovered: adding a `.text` file creates a test automatically.
- **Autofix-cycle fixtures** (`testsuite/fixtures/autofix/<maintainer>/<case>.text`) exercise the full diagnostic cycle `(inspector, finding, maintainer, resolution)`. The YAML front-matter declares all four symbol fields as mandatory; the body contains exactly three `---`-separated documents: input source, expected finding slot values as a plist, and expected fixed code. Validated by `validate-one-autofix-cycle-fixture` which asserts the primary outcome plus **self-idempotency at N=1** — re-running the `(inspector, maintainer)` pair on the result must yield the same result. See `testsuite/autofix.lisp` and `product/slice/007-*/references/linter-convergence.md` for the rationale.
- **Pretty-printer cross-population**: every autofix-cycle fixture whose inspector operates at the `:syntax` level has its expected fixed code document asserted as a `read ⟫ pretty-print-form` fixed point. **The fixture is normative**: when the fixture's expected output disagrees with the pretty-printer, the pretty-printer must be fixed — not the fixture. Fixtures encode the project's canonical formatting decisions; the pretty-printer implements them. Line-inspector fixtures are excluded from this cross-population.
- Maintainers that do not fit the discoverable-fixture format (e.g. `fix-mixed-indentation`, whose expected output has semantically meaningful leading whitespace) are tested via ad-hoc `define-testcase` under `testsuite/maintainers/`. Discoverable fixtures are a convenience, not a goal.
- Maintainer and syntax tests use in-memory parsing (`parse-common-lisp` on string) and in-memory resolution (`apply-resolutions` on string). Only file round-trip tests (UTF-8, atomicity) touch the filesystem.
- No SBCL-specific extensions without an explicit `#+sbcl` guard.
