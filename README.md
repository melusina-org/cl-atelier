# Atelier

An atelier for Common Lisp developers.

Atelier provides project scaffolding and a linter for Common Lisp projects.
The linter is designed to work across Common Lisp, Shell Script, and Terraform,
and to be extended by any project or library that wants to publish its own
quality checks alongside its code.

This software is Copyright © 2017–2026 Michaël Le Barbier and is distributed
under the terms of the MIT licence.


## Project Scaffolding

Atelier can generate a new Common Lisp project with a standard layout,
copyright header, licence, and ASDF system definition:

```lisp
(org.melusina.atelier:new-lisp-project #p"~/Lisp/myproject/"
  :copyright-holder "Jane Smith"
  :copyright-year "2025"
  :project-filename "org.example.myproject"
  :project-name "My Project"
  :project-description "A short description."
  :homepage "https://github.com/example/myproject"
  :license :mit)
```


## Linter

The Atelier linter is built around a small number of design ideas that
distinguish it from existing Common Lisp linting tools.


### Three Levels of Analysis

Inspectors operate at three distinct levels of abstraction, each implemented
as a separate class:

**File-level** inspectors examine the file as a whole: encoding (UTF-8
validity, absence of a byte-order mark), line-ending consistency, file
permissions, copyright header presence and format, and whether the file
is non-empty.

**Line-level** inspectors work over the file's lines as a sequence of
strings: trailing whitespace, lines exceeding a configured maximum length,
runs of consecutive blank lines, and tab characters in Lisp files.

**Code-level** inspectors analyse parsed structure. For Common Lisp,
two backends are available: one drives SBCL as a subprocess to surface
compiler warnings and type errors; the other uses Eclector's concrete
syntax tree reader to perform structural checks such as detecting
`LET`-bound lambdas that should be `FLET` or `LABELS`, and `IF` forms
that should be `WHEN` or `UNLESS`. A third backend submits source
fragments to a local LLM (llama-server) for open-ended quality
assessment of docstrings and procedure complexity.

File and line inspectors are language-agnostic. They run on any text file
and specialise per language through a registration mechanism. Code
inspectors are language-specific by design.


### ASDF Integration

The linter is a first-class ASDF operation, on equal footing with
`asdf:test-op`. Running the linter on a system requires no separate
configuration file or command-line tool:

```lisp
(asdf:operate 'atelier:lint-op "org.my.project")
```

Linter policy is declared inside the system definition as a sibling
component, using a dedicated component type:

```lisp
(asdf:defsystem "org.my.project"
  :depends-on ("org.melusina.atelier/asdf")
  :components
  ((:module "src" :components ((:file "package") (:file "core")))
   (atelier:linter-parameters "linter-parameters")))
```

The file `linter-parameters.sexp` is pure data — read with `READ` and
`*read-eval*` bound to `nil`, never `LOAD`ed — containing the policy
for this system:

```lisp
(:groups ((:security . :forbid)
          (:style    . :warn)
          (:line     . :deny))
 :rules  ((org.melusina.atelier.lisp:let-to-flet . :allow))
 :thresholds ((org.melusina.atelier.line:max-line-length . 120))
 :default :warn)
```

When no `linter-parameters` component is present, `lint-op` applies
sensible defaults and emits a warning about the missing policy rather
than failing.


### Policy with Four Dispositions

Every inspector is associated with a disposition drawn from four levels,
borrowed from Rust's Clippy: `allow` (silent), `warn` (report only),
`deny` (report and fail), and `forbid` (report, fail, and immune to
inline suppression). Dispositions are configured per inspector symbol,
per package group, or as a project-wide default, and are resolved from
most specific to least specific.

The `forbid` level is the key safety-gate mechanism. A project can
declare that no hardcoded credential will ever pass the linter regardless
of what inline suppression comments appear in the source:

```lisp
:groups ((:security . :forbid))
```

This cannot be overridden by any per-file or per-line suppression.


### Findings, Advice, and Interactive Recovery

An inspector produces **findings** — pure observations (this inspector,
at this location, observed this condition). The runner then consults
the active policy and the registered **advice providers** to assemble
**advice** objects for each finding. An advice object carries the finding,
a severity, an optional fix callable, and a list of named recovery paths.

This separation means that the same finding can attract advice from
multiple sources. The base Atelier system contributes one advice; a
companion system loaded alongside it can contribute additional advice
specific to the project's conventions, without either system knowing
about the other.

The condition/restart protocol is the delivery mechanism. Each advice
is signalled as an `atelier:inspector-warning` or
`atelier:inspector-error` condition, and four standard restarts are
established around every finding:

- `accept-finding` — acknowledge and continue.
- `apply-fix` — invoke the fix callable if present, then continue.
- `suppress-inspector` — add the inspector to the session allow-list.
- `skip-file` — skip remaining findings for this file.

In a live SLIME or SLY session the debugger presents these restarts
interactively. In batch mode (CI, pre-commit hooks) the runner invokes
`apply-fix` automatically for fixable findings and exits with a non-zero
code when `deny`- or `forbid`-level findings remain.

Fix callables are pure functions: they accept a pathname, perform no
I/O, and return `(values modified-p new-content-string)`. The runner is
responsible for writing modifications back to disk.


### Inspector Identity via the Package System

Each inspector is identified by a Lisp symbol in a package. There is no
separate string-based naming scheme. The package system provides
namespacing, stability, and uniqueness for free:

```
atelier/line:trailing-whitespace
atelier/lisp:let-to-flet
my-project/atelier:handler-naming
```

Policy configuration, inline suppression, and CI output all refer to
inspectors by their fully qualified symbol names.


### Extensibility via Companion Systems

Any project or library can publish a companion ASDF system that registers
inspectors specific to its conventions. The three steps required are:

```lisp
;; 1. Define the system.
(asdf:defsystem "my.project/atelier"
  :depends-on ("org.melusina.atelier/linter" "my.project")
  :components ((:file "package") (:file "inspectors")))

;; 2. Define the package.
(defpackage #:my.project.atelier
  (:use #:cl))

;; 3. Register inspectors.
(atelier:define-inspector my.project.atelier:my-convention
  :level :line
  :languages (:common-lisp)
  :documentation "Enforce my.project naming conventions.")
```

Loading the companion system is sufficient to activate its inspectors
in any subsequent `lint-op` run. No changes to Atelier or to the
target project are required.

Atelier publishes companion systems for common Lisp libraries. The first
is `org.melusina.atelier/hunchentoot`, covering handler naming, missing
error handlers, and route definition style. Others are planned for
Alexandria, Drakma, and cl-webmachine.


## Running the Linter

**From the REPL** (interactive mode — findings are signalled as conditions
with restarts presented by the debugger):

```lisp
(ql:quickload "org.melusina.atelier/linter")
(asdf:operate 'atelier:lint-op "org.my.project")
```

**From the command line** (batch mode — findings are printed as
`file:line:col: severity: message`; exit code reflects the worst
disposition encountered):

```sh
sbcl --script lint.lisp -- org.my.project
```

**As a pre-commit hook** or GitHub Actions step, the command-line entry
point integrates directly with standard CI tooling.


## Installation

Atelier is available from Quicklisp:

```lisp
(ql:quickload "org.melusina.atelier")
(ql:quickload "org.melusina.atelier/linter")
(ql:quickload "org.melusina.atelier/asdf")
```

The linter requires SBCL. The SBCL compiler backend and the
`file-permissions` inspector use SBCL-specific interfaces and are
guarded with `#+sbcl`; on other implementations they degrade gracefully
to no-ops with a load-time warning. All other inspectors are portable
ANSI Common Lisp.

The `shellcheck` and `terraform` code-level inspectors require the
respective external tools to be installed and on `PATH`. Their absence
is detected at run time and results in a warning, not an error.

The LLM backend requires a running llama-server instance (llama.cpp).
It is disabled by default (`allow` disposition) and must be explicitly
enabled in the `linter-parameters` file.
