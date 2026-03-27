# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

`org.melusina.atelier` is a Common Lisp developer toolbox providing:
- **Project scaffolding** via a template system (`new-lisp-project`, `new-lisp-file`)
- **A linter** for Common Lisp, Shell Script, and other source file types, extensible by companion ASDF systems
- **License management** (MIT, GPL, CeCILL, Proprietary)

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

Run a single test group (from `testsuite/entrypoint.lisp`):
```lisp
(atelier/testsuite:testsuite-linter)
(atelier/testsuite:testsuite-template)
```

Run the linter on this project:
```lisp
(ql:quickload "org.melusina.atelier/development")
(atelier/development:lint)
```

## ASDF Systems

| System | Purpose |
|--------|---------|
| `org.melusina.atelier` | Main system (scaffolding, current linter) |
| `org.melusina.atelier/testsuite` | Test suite (uses `org.melusina.confidence`) |
| `org.melusina.atelier/development` | Dev helpers: `lint`, `reload` |
| `org.melusina.atelier/linter` | Redesigned linter engine (in progress — see `agent/`) |
| `org.melusina.atelier/asdf` | ASDF `lint-op` and `linter-parameters` component (in progress) |

## Architecture

### Initialization

`(atelier:initialize)` must be called before using templates or licenses. It loads both repositories from `resource/`. The `lint` function and `find-template` call it automatically if needed.

### Parameter System (`src/parameter.lisp`)

Templates use `${PARAMETER_NAME}` placeholders. Parameter names are case- and separator-insensitive (`copyright-holder` = `COPYRIGHT_HOLDER`). `*parameter-bindings*` is the dynamic alist of replacements. `parameter-replace` does topological sorting of bindings to handle inter-parameter references.

Block parameters (`:license-text`, `:license-header`) are handled differently: the replacement is indented to match the surrounding comment style.

### License Repository (`src/license.lisp`, `resource/license/`)

License files are `.text` files with YAML front matter followed by two documents (header, full text), separated by `---`. Loaded into `*license-repository*` hash table keyed by keyword (e.g., `:mit`).

### Template Repository (`src/template.lisp`, `resource/template/`)

Two template kinds:
- **`file-template`** — wraps a single `.text` file from `resource/template/`; loaded at `initialize` time
- **`composite-template`** — declared in source with `define-composite-template`; composes other templates into a directory tree

`write-template` dispatches on template type and destination (pathname, stream, or `t` for stdout). `new-lisp-project` is the top-level entry point.

### Current Linter (`src/lint.lisp`, `src/inspector/`)

**Linters** (registered in `*linter-table*` by file-type keyword) match files via a predicate DSL (`:has-suffix`, `:has-name`, `:has-shebang`, `:or`, `:and`) and carry three inspector lists: file, content, line.

**Inspectors** are functions registered with `define-inspector` (keyed by keyword code in `*inspector-table*`). They return `nil` (no hint), a `hint` instance, a list of hints, or a modified string (for content/line inspectors that auto-fix).

`*linter-interactive-p*` defaults to `t` when SWANK is connected. In batch mode, anomaly conditions are caught and the process exits with status 1 on any hint.

Built-in linter types: `plain-line-comment-linter` (Lisp, shell, make, Docker, TeX) and `plain-block-comment-linter`.

### Redesigned Linter (in progress — `agent/SLICE-*.md`)

The SLICE plans in `agent/` implement the linter architecture described in the README. Key concepts:

**Three analysis levels:** File-level (encoding, copyright), line-level (trailing whitespace, line length), and code-level (parsed structure via Eclector CST reader, SBCL compiler backend, or LLM backend via llama-server).

**Inspector identity via packages:** Each inspector is a Lisp symbol (e.g., `org.melusina.atelier.line:trailing-whitespace`). No separate string naming scheme.

**Findings → Advice:** Inspectors produce `finding` objects (pure observations). The runner consults the `policy` and advice providers to produce `advice` objects carrying an optional fix callable and recovery options.

**Four dispositions:** `allow` (silent) < `warn` (report) < `deny` (report + fail) < `forbid` (deny + immune to suppression). Configured per inspector symbol, per package group, or project-wide default.

**Condition/restart protocol:** Each advice is signalled as `atelier:inspector-warning` or `atelier:inspector-error` with four standard restarts: `accept-finding`, `apply-fix`, `suppress-inspector`, `skip-file`. In batch mode the runner invokes `apply-fix` automatically; in interactive mode (SLIME/SLY) the debugger presents restarts.

**ASDF integration:** `atelier:lint-op` is a first-class operation. Policy is declared as a `atelier:linter-parameters` component in the system definition, pointing to a `.sexp` data file read with `*read-eval*` bound to `nil`.

**Extensibility:** Any project can publish a companion ASDF system that calls `atelier:define-inspector` to register project-specific inspectors. Loading it is sufficient to activate them in subsequent `lint-op` runs.

**Runtime dependencies:** The SBCL compiler backend and file-permissions inspector are `#+sbcl` guarded. `shellcheck`/`terraform` inspectors require the tools on `PATH`. The LLM backend requires a running llama-server instance and is disabled by default.

### Resource Files (`resource/`)

- `resource/license/*.text` — license definitions
- `resource/template/*.text` — file templates with YAML front matter
- `*resourcedir*` is set at compile/load time from the system source directory, or overridden via `ATELIER_RESOURCEDIR` env var

## Code Conventions

- Every source file begins with `;;;; filename — Description` and ends with `;;;; End of file 'filename'`.
- LOOP clause keywords are always keyword symbols (`:for`, `:in`, `:collect`).
- `MAPCAR`/`MAPCAN`/`REMOVE-IF` receive a named `FLET` function, never a bare `LAMBDA`.
- Tests use `define-testcase` from `org.melusina.confidence`.
- Current inspectors: `define-inspector :keyword-code function-name (args)`.
- Redesigned inspectors (SLICE plans): `define-inspector package:symbol :level … :languages …`.
- No SBCL-specific extensions without an explicit `#+sbcl` guard.

## Implementation Status

The `agent/` directory contains SLICE plans (`SLICE-0.md` through `SLICE-4.md`) implementing the linter architecture described in the README:
- `SLICE-0` — core protocol (`inspector`/`finding`/`advice`/`policy`), condition/restart protocol, ASDF `lint-op`
- `SLICE-1` — file and line inspectors
- `SLICE-2` — code inspectors (Eclector CST backend)
- `SLICE-3` — SBCL compiler backend
- `SLICE-4` — LLM backend (llama-server)

The existing `src/lint.lisp` and `src/inspector/` contain the current (pre-redesign) linter, which remains functional.
