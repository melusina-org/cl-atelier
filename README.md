# Atelier

An atelier for Common Lisp developers.

Atelier provides project scaffolding and a linter for Common Lisp projects.
The linter checks file structure, coding style, and naming conventions at
the file, line, and CST (concrete syntax tree) levels. It supports autofix
for most findings and is extensible via ASDF companion systems.

This software is Copyright © 2017–2026 Michaël Le Barbier and is distributed
under the terms of the MIT licence.


## Project Scaffolding

Atelier can generate a new Common Lisp project with a standard layout,
copyright header, licence, and ASDF system definition:

```lisp
(atelier:new-lisp-project #p"~/Lisp/myproject/"
  :copyright-holder "Jane Smith"
  :copyright-year "2025"
  :project-filename "org.example.myproject"
  :project-name "My Project"
  :project-description "A short description."
  :homepage "https://github.com/example/myproject"
  :license :mit)
```


## Linter

### Inspectors

Inspectors examine source files and produce findings. There are
13 built-in inspectors at three levels:

**File-level** — examine the file as a whole:
- `check-file-encoding` — UTF-8 validity
- `check-spdx-license-header` — SPDX licence identifier presence and correctness
- `check-header-line` — canonical header line (`;;;; filename — description`)
- `check-footer-line` — canonical footer line (`;;;; End of file 'filename'`)
- `check-project-identification` — project name, homepage, copyright block

**Line-level** — work over lines as strings:
- `check-trailing-whitespace` — trailing spaces or tabs
- `check-mixed-indentation` — tabs vs. spaces consistency

**Syntax-level** — analyse parsed structure via Eclector's CST reader:
- `check-earmuffs` — `DEFVAR`/`DEFPARAMETER` names lack `*earmuffs*`
- `check-constant-naming` — `DEFCONSTANT` names lack `+plus-convention+`
- `check-bare-lambda` — bare `LAMBDA` in higher-order calls instead of named `FLET`
- `check-loop-keywords` — bare `LOOP` clause keywords instead of keyword symbols
- `check-labels-for-flet` — `LABELS` with no mutual/self-recursion (should be nested `FLET`)

File and line inspectors support multiple file types via `file-comment-prefix`:
Lisp, Shell, Makefile, Dockerfile, Terraform, C/C++, TeX/Metapost, and Autoconf.
Syntax inspectors are Lisp-specific.


### Automatic Maintainers

Each inspector has a paired maintainer that produces a resolution — a
concrete fix that can be applied automatically:

| Maintainer | Fix |
|------------|-----|
| `fix-trailing-whitespace` | Strip trailing whitespace |
| `fix-mixed-indentation` | Normalise to spaces or tabs |
| `fix-earmuffs` | Add `*earmuffs*` to special variable names |
| `fix-constant-naming` | Add `+plus+` to constant names |
| `fix-bare-loop-keywords` | Replace bare keywords with keyword symbols |
| `fix-bare-lambda` | Extract lambda to named `FLET` function |
| `fix-labels-to-flet` | Rewrite spurious `LABELS` as nested `FLET` |
| `fix-header-line` | Set canonical header line |
| `fix-footer-line` | Set canonical footer line |
| `fix-project-identification` | Set canonical project identification block |


### Running the Linter

Lint a system and return findings:

```lisp
(atelier:lint-system "org.my.project")
```

Lint with autofix — applies all automatic resolutions and returns remaining findings:

```lisp
(atelier:lint-system "org.my.project" :autofix t)
```

As an ASDF operation:

```lisp
(asdf:operate 'atelier:linter-op "org.my.project")
```


### ASDF Integration

Linter configuration is declared as ASDF components in the system definition:

```lisp
(asdf:defsystem "org.my.project"
  :depends-on ("org.melusina.atelier")
  :components
  ((:module "src" :components ((:file "package") (:file "core")))
   (atelier:asdf-project-configuration "project-configuration")
   (atelier:asdf-linter-configuration "linter-configuration")))
```

**`project-configuration.sexp`** — project metadata used by header/footer inspectors:

```lisp
(:project-name "My Project"
 :homepage "https://github.com/example/myproject"
 :copyright-year "2025"
 :copyright-holder "Jane Smith"
 :project-filename "org.example.myproject"
 :license "MIT")
```

**`linter-configuration.sexp`** — linter policy:

```lisp
(:indentation-style :spaces
 :disabled-inspectors (check-earmuffs)
 :severity-overrides ((check-trailing-whitespace . :error))
 :maintainer-overrides ((fix-labels-to-flet . :interactive)))
```

When no configuration components are present, `lint-system` applies
sensible defaults and emits a warning.


### Autofix Signalling

During `lint-system :autofix t`, each proposed resolution is signalled
as a `resolution-proposed` condition with two restarts:
- `apply-resolution` — apply the fix
- `skip-resolution` — skip this fix

Maintainers have a maturity level (`:stable` or `:experimental`).
Stable maintainers auto-apply in batch mode. Experimental maintainers
signal a warning and are skipped unless the operator invokes
`apply-resolution` from the debugger.

Per-maintainer disposition can be overridden in the linter configuration:

```lisp
:maintainer-overrides ((fix-labels-to-flet . :interactive)
                        (fix-trailing-whitespace . :auto))
```

Dispositions: `:auto` (apply silently), `:interactive` (signal warning),
`:skip` (do not apply).


### Extensibility

Any project can register additional inspectors by defining and registering
them in a companion ASDF system:

```lisp
(atelier:define-syntax-inspector my-project:check-handler-naming (form)
  "Check that HTTP handler functions follow naming conventions."
  ;; Walk CST and produce findings...
  )
```

Loading the system is sufficient to activate the inspector in subsequent
`lint-system` calls.


## Installation

```lisp
(ql:quickload "org.melusina.atelier")
```

The linter uses Eclector for CST parsing. All inspectors are portable
ANSI Common Lisp. No SBCL-specific extensions are used without an
explicit `#+sbcl` guard.
