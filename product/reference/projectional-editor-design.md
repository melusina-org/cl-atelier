# Projectional Editor — Design Direction

**Status:** Durable design reference. Read by slice 010 and every subsequent slice that touches the projectional editor or its MCP adapter.
**Recorded:** 2026-04-11
**Author:** Strategist, during slice 010 design session
**Scope of authority:** Contract for how Atelier-managed Lisp files are read, represented, and written by the `org.melusina.atelier/editor` system, and for how the MCP server exposes editor capabilities to agents.

---

## Purpose

Atelier's projectional editor does not let callers edit files as text. It
lets them manipulate **forms**, and it owns the file layout, package
declarations, header/footer, compile-order, and canonical formatting. The
caller sends semantic objects; the editor produces valid, lintable,
pretty-printed Lisp.

The projectional editor is the central differentiator of Atelier's
agent-facing surface over generic Common Lisp MCP servers and generic
structural editors. It is a standalone capability with its own ASDF system,
and the MCP server is **one adapter among possible future consumers** (a
CLI, an Emacs integration, a web UI, a CI tool).

### Why

1. **Token efficiency.** `update-form foo with <body>` costs a few hundred
   tokens per edit. An `Edit(old_string, new_string)` against a 400-line file
   costs thousands. Over a long session this compounds into real savings in
   both cost and context window.
2. **Canonical output by construction.** Every write flows through Atelier's
   pretty-printer and automatic maintainers. Earmuffs, constant naming, LOOP
   keyword style, bare-lambda rewriting, header/footer, SPDX line — none of
   these are the agent's concern. It cannot produce output that violates
   them; the server would not write such output.
3. **Topological invariants hold by construction.** DEFUN-before-use,
   DEFMACRO-before-expansion, eval-when for helper functions called from
   macros, DEFCONSTANT referenced from DEFVAR initializers — these are all
   compile-order gotchas that the server resolves by free-variable analysis
   and topological sort. The agent writes the natural form; the server makes
   it compile.
4. **Dependency discipline is enforced, not requested.** `(:depends-on …)` in
   `.asd` files and `(:use …)`/`(:import-from …)` in `defpackage` forms are
   derived from the free variables of the forms in each component, not
   written by the agent. "It loads on my machine" fails as a class.
5. **One protocol, not two.** The agent already knows the
   finding/resolution schema (slices 001–009). The projectional editor reuses
   the same pipeline: every write is a lint pass, every compile warning is a
   finding, every automatic maintainer runs on every write.
6. **Files remain readable by any editor.** The on-disk artefact is always a
   valid `.lisp` file. If the MCP server is unavailable, the user edits the
   file by hand. No projectional lock-in.

---

## System layering

The projectional editor is a standalone system. Its ASDF and package shape
mirrors the MCP server's slice-009 organisation:

```
org.melusina.atelier                    ← core: linter, maintainers, pretty-printer
        │
        ├─── depends-on ──────────┐
        │                          │
        ▼                          ▼
org.melusina.atelier/editor    org.melusina.atelier/mcp
  package: atelier/editor        package: atelier/mcp
        │                          │
        └─── depended-on-by ──────→┘
                                   │
                                   ▼
                         MCP tools wrap editor API
```

- **`org.melusina.atelier/editor`** (package `atelier/editor`) — the
  projectional editor proper. Owns the `form` record, the read/write
  pipeline, topological sort, `eval-when` dependency analysis, header and
  footer templating, and the `defpackage` / `defsystem` structural APIs.
  Depends on core `org.melusina.atelier` for the pretty-printer, the
  linter, the maintainer registry, and Eclector integration. **Does not
  depend on `/mcp`.** Has no awareness of JSON-RPC, stdio framing, or any
  agent protocol.

- **`org.melusina.atelier/mcp`** — the MCP server. Depends on
  `org.melusina.atelier/editor` so that MCP tools can call
  `atelier/editor:normalize-toplevel-form`,
  `atelier/editor:add-form`, and the rest of the editor API. MCP tools
  are thin wrappers: parse arguments, delegate to the editor API, wrap
  the result in a JSON-RPC response.

- **`org.melusina.atelier/testsuite`** — single consolidated test system,
  per the slice 009 pattern. Tests for the editor live under the module
  `testsuite/editor/` in package **`atelier/testsuite/editor`**, reachable
  through the canonical `(asdf:test-system "org.melusina.atelier")` entry
  point. There is **no** independent `org.melusina.atelier/test/editor`
  system. Same discipline as `testsuite/mcp/`.

- **Other future consumers.** A CLI (`atelier form update foo`), an Emacs
  integration, or a web UI all depend on `org.melusina.atelier/editor`
  directly and never touch `/mcp`. The editor is not an MCP library; the
  MCP server is an editor adapter.

**Invariant:** `org.melusina.atelier/editor` compiles and loads cleanly with
no MCP dependency in the image. A fresh SBCL can
`(asdf:load-system "org.melusina.atelier/editor")` without pulling in
`com.inuoe.jzon`, `bordeaux-threads`, or any MCP code. This invariant is
enforceable by a subprocess-load test in slice 010, analogous to the
fresh-SBCL subprocess test slice 009 added for the main suite.

---

## The constrained file model

An Atelier-managed Lisp file has exactly the following structure:

```
<header banner>            ← server-owned, template-driven
                             (filename + description + SPDX + copyright)
<in-package form>          ← server-owned, derived from component.package
<form 1>                   ← agent-addressable
<form 2>
…
<form N>
<footer banner>            ← server-owned, template-driven
                             (;;;; End of file 'filename')
```

The file is the unit of persistence. The **form list** is the unit of agent
interaction.

### The `toplevel-form` record

Every addressable toplevel item is a `toplevel-form` instance (CLOS class)
with four slots:

| Slot | Type | Meaning |
|---|---|---|
| `kind` | symbol | The operator symbol from the form's CAR. Examples: `common-lisp:defun`, `common-lisp:defclass`, `confidence:define-testcase`, `atelier:define-maintainer`. Third-party toplevel macros are first-class citizens. |
| `name` | symbol or nil | The defined symbol, derived from position 1 (second element) of the form for both known CL definition forms and unknown third-party macros. NIL for expression forms that do not define a named entity. |
| `body` | Eclector CST | The complete form as an Eclector Concrete Syntax Tree. Reader conditionals (`#+`/`#-`) inside the body are preserved as CST structure, not evaluated. The docstring (if any) remains in its natural position within the body — it is not extracted. Use `toplevel-form-ast` for a plain s-expression view that evaluates reader conditionals against a given feature set. |
| `eval-when` | list | The EVAL-WHEN situation list wrapping this form. Default `(:load-toplevel :execute)`, elided on write. `:compile-toplevel` is added automatically by the dependency analyzer (slice 015) when required. |

**Slots deliberately omitted:**

- **`docstring`** — remains in the body CST. Extracting it requires
  kind-specific knowledge that breaks down for unknown third-party macros.
  The body is the truth; callers who want the docstring inspect the AST.
- **`feature-expression`** — dropped. Platform-specific whole-form guards
  are handled at the ASDF level (feature-guarded `:components` entries),
  which is established CL practice. The editor never wraps a single form
  in `#+` at the toplevel level.
- **`source-position`** — dropped. The editor's ordering algorithm
  (topological sort by free-variable dependencies, alphabetical tie-break
  by symbol name) is the single authority on form order. No slot
  influences it. Dependencies are computed on demand from the body,
  never stored.

**Derived accessor:**

```lisp
(toplevel-form-ast form &key features)
```

Returns the body as a plain s-expression. Reader conditionals are evaluated
against FEATURES (default: `cl:*features*`). The result is a lossy view:
cross-platform branches for non-matching features are dropped. This is for
convenient examination; the canonical representation is the Eclector CST
in the `body` slot.

**Constructor:**

```lisp
(make-toplevel-form &key kind name body eval-when)
```

**Read/Write functions:**

```lisp
(read-toplevel-form-from-string string)   ; → toplevel-form (or list via DECOMPOSE restart)
(write-toplevel-form-to-string form)      ; → string
(normalize-toplevel-form form)            ; → (values toplevel-form findings)
```

When the editor **writes** a file (slice 015), it:

1. Renders the header banner from the template.
2. Writes `(in-package #:<component.package>)`.
3. Computes dependencies by analyzing free variables in each form's body.
4. Topologically sorts the form list. Tie-break: alphabetical by symbol
   name. Mutually recursive DEFUNs can be broken by `(declaim (ftype ...))`
   forms that provide forward type declarations.
5. For each form, wraps it in the minimal `eval-when` envelope required by
   its metadata and pretty-prints the CST to text, preserving reader
   conditionals.
6. Renders the footer banner from the template.
7. Runs every automatic maintainer on the output and applies every
   auto-applicable resolution.
8. Writes the file atomically.

When the editor **reads** a file, it inverts this process: strips the banners,
parses `in-package`, reads each toplevel form via a custom Eclector client
that preserves `#+`/`#-` as CST structure, and hydrates `toplevel-form`
records by peeling `eval-when` wrappers into the `eval-when` slot. This
read-back is required to be a fixed point:
`write(read(write(read(s)))) = write(read(s))` for any string `s` the
editor accepts.

### Forbidden at toplevel

The projectional editor refuses to represent or emit the following:

- **Mid-file comments.** `;;`, `;;;`, `;;;;`, and `#|…|#` between forms.
  Section headings, FIXMEs, and narrative commentary are not allowed. Header
  and footer banners are the only comment-bearing structures in the file,
  and the server owns them.
- **Agent-supplied `in-package` forms.** The server writes `in-package` from
  the component's package slot. The agent never sends an `in-package`.
- **Side-effectful toplevel forms.** `(format t …)`, `(setf <global> …)`
  outside of initializers, `(load …)`, `(require …)`. Tests register via
  `define-testcase`, not by running at load time.
- **The `#.` reader macro.** Read-time evaluation is unsound under the
  round-trip read/write contract.
- **Toplevel `progn`.** `progn` at toplevel signals
  `unexpected-toplevel-form` with a `decompose` restart that splits it
  into individual `toplevel-form` records. The agent may submit a
  sequence directly, or submit a `progn` and handle the restart.
- **Toplevel feature conditionals** (`#+sbcl (defun ...)`). Platform-
  specific whole-form guards are handled at the ASDF level: feature-
  guarded `:components` entries in the `defsystem` form, which is
  established CL practice. The editor does not wrap individual forms
  in `#+` at the toplevel level.

**Allowed inside form bodies:**

- **`#+`/`#-` reader conditionals.** Following established CL practice,
  reader conditionals are allowed inside DEFUN bodies, DEFCLASS slot
  lists, DEFMETHOD bodies, etc. The body is an Eclector CST that
  preserves these as structure, not evaluated away.
  Example: `(defun connect () #+sbcl (sb-bsd-sockets:...) #-sbcl (error ...))`
  is a single `toplevel-form` with `kind = common-lisp:defun` and the
  `#+`/`#-` branches preserved in the body CST.

### `defpackage` and `defsystem` are structural, not formal (W1)

Two forms are **not** addressable as ordinary forms in the form list:

- **`defpackage`** — owned by the **package** of the ASDF component. The
  agent manipulates it via high-level APIs: `add-export`, `remove-export`,
  `add-import-from`, `remove-import-from`, `add-use`, `remove-use`,
  `rename-package`. The server rewrites the single `defpackage` form in the
  package's source file in place. The agent never sends a `defpackage` form.
- **`defsystem`** — owned by the `.asd` file. The agent manipulates it via
  high-level APIs: `create-system`, `rename-system`, `add-component`,
  `remove-component`, `rename-component`, `move-component`. The server
  rewrites the `defsystem` form in place. The agent never sends a
  `defsystem` form.

This keeps the form list conceptually pure (forms are semantic units inside
a single file, inside a single package, inside a single component) while
giving `defpackage` and `defsystem` the structural API they need.

### Compile-order responsibilities are the server's (W2)

The server owns, and the agent does not:

- **Ordering within a file.** Forms are topologically sorted before emission.
- **`eval-when` wrapping.** A DEFUN used as a helper by a same-file DEFMACRO
  automatically gets `(:compile-toplevel :load-toplevel :execute)`. The
  agent writes `defun`; the server writes `eval-when`.
- **Cross-file dependencies within an ASDF system.** The server updates
  the `:components` ordering (for `:serial t`) or declares explicit
  `:depends-on` edges so that every file compiles after its dependencies.
- **`defpackage` export set.** When a form is added that defines an exported
  symbol, the server updates the package's `:export` list. When the last
  reference to an exported symbol is removed, the server flags it (it does
  not remove the export automatically; that is a refactoring operation).
- **ASDF `:depends-on` graph.** When a form is added that references a
  symbol from a package that is not yet a dependency of the owning system,
  the server proposes adding the dependency — either automatically (if
  `unambiguous`) or as a finding for the agent to confirm.

### Fallback

Because the on-disk representation is always a valid Lisp file, the user may
edit any managed file directly with any editor at any time. When the MCP
server next reads the file, it reconstructs the form list from the text, and
any manual edits survive as long as they respect the constrained file model.
If a manual edit introduces a mid-file comment or a toplevel `progn`, the
next read canonicalises it: the comment is dropped on the next write, the
`progn` is decomposed. **There is no projectional lock-in.**

---

## Wrinkles recorded, not solved

These are open issues captured now so that the slice that encounters them
has a place to start.

**W3 — Mutually recursive DEFUN ordering is ambiguous.** When `a` calls `b`
and `b` calls `a`, topological sort has no preferred order. Resolution:
declaration order (`source-position`) as written by the agent. The fixed
point is `(read (write form-list)) ≡ form-list` for any form list the server
itself produced; for form lists assembled by the agent from scratch, the
server's ordering choice is authoritative after the first write.

**W4 — CLOS `defmethod` without the primary `defgeneric`.** A `defmethod`
implicitly creates a `defgeneric` if none exists. The projectional editor
requires the `defgeneric` to be explicit: a method without a matching
generic is rejected, and the server proposes adding the generic. This
makes method dispatch discoverable by symbol lookup and keeps the form list
complete.

**W5 — Reader-macro-defined forms outside CL.** Macros that expand into
code using non-standard readtables (`cl-interpol`, `cl-ppcre` character
classes, `str:concat` extensions) round-trip only if the readtable is
registered with the server. Out of scope until a concrete story requires
it.

**W6 — Forms that reference themselves via `#n=`/`#n#`.** Circular sharing
at read time is rare outside data literals but not impossible. Out of scope
until a concrete story requires it; reject during read-back with a clear
error.

---

## Slice dependency map

The projectional editor is a **track**, not a slice. Its implementation is
spread across multiple slices, each adding one capability to
`org.melusina.atelier/editor`, with the MCP server gaining a matching
adapter tool.

| Slice | `org.melusina.atelier/editor` delivery | `org.melusina.atelier/mcp` delivery |
|---|---|---|
| **010** — child image + eval + package introspection | Create the system. Ship the `form` record type with its seven slots. Ship `canonicalize-form` (form in → pretty-printed, maintainer-applied form out) built on the existing pretty-printer. Ship `read-form` / `write-form` helpers. Export the API. **No file I/O.** Fresh-SBCL subprocess-load test proves the system loads without MCP. | MCP tools wrap `atelier/editor:canonicalize-form`. A new `atelier:canonicalize-form` tool returns the canonicalized form. The `atelier:eval-form` tool returns both the evaluation result and the canonicalized form it evaluated. |
| **011** — debugger and restarts | No new editor API. | Consumes `form` records when presenting eval-in-frame and restart selection over the slice-010 child-image transport. |
| **012** — ASDF + Quicklisp + Confidence | Read-only ASDF structural API: `list-systems`, `list-components`, `where-is-system`. No mutation yet. | MCP tools wrapping the read-only ASDF API. |
| **013** — documentation (describe, apropos, hyperspec, macroexpand) | `macroexpand-form` on the `form` record. | MCP tools wrapping documentation helpers. |
| **014** — xref + inspector + `who-tests` | Free-variable analysis first needed for real: cross-reference queries feed the dependency analyzer that slice 015 will use for topological sort. | MCP tools wrapping xref and inspector APIs. |
| **015** — refactorings + **projectional editor proper** | File-level form CRUD: `add-form`, `update-form`, `remove-form`, `rename-form`, `move-form`. `defpackage` structural API: `add-export`, `remove-export`, `add-import-from`, `add-use`, `rename-package`. `defsystem` structural API: `create-system`, `add-component`, `remove-component`, `rename-component`, `move-component`. Header/footer template application. Topological sort. **First real write-back to managed files.** | MCP tools wrapping the editor's file-level CRUD and the `defpackage` / `defsystem` structural APIs. Lint-passthrough via the existing maintainer pipeline. |
| **016** — domain diagnostics | Consumes the `form` record when reporting per-form CFFI or threading diagnostics. No new editor capabilities. | MCP tools wrapping diagnostics helpers. |

**Key invariant of this split:** every slice before 015 may **read** project
files but **never writes** them through the projectional pipeline. The
first projectional write is slice 015's defining deliverable. Slices
010–014 exercise the `form` record against eval'd code and introspection
output, so that by the time 015 commits a file, the form model has already
been round-tripped thousands of times in tests.

**Second key invariant:** `atelier/editor` never depends on `atelier/mcp`.
The dependency arrow points one way: adapters depend on the domain, never
the other way around. A third consumer (CLI, Emacs, web) can be added in
any future slice without touching `/mcp`.

---

## Non-goals

- **Not a replacement for text editing.** Users who want to write comments,
  maintain non-canonical formatting, or keep forms in a specific order the
  projectional editor would rewrite should edit the file directly. Managed
  files are not a prison; they are a contract for agent edits.
- **Not a syntax database.** The form record is not persisted to SQLite or
  any index (see backlog #33). It is reconstructed on every read.
- **Not a package manager.** The projectional editor updates `:depends-on`
  clauses but does not resolve or install dependencies. Quicklisp / OCICL /
  ASDF remain the package managers.
- **Not a compiler.** The server invokes SBCL's compiler to validate each
  write; it does not implement type inference or code generation.
- **Not scoped to CL.** Elisp, Shell, and Terraform/HCL files are **not**
  projectional-editor managed. They remain text-edited through the linter
  pipeline. The projectional editor is a Common Lisp capability first; if
  a compelling story arrives for Elisp, it is a separate track.

---

## Design principles this reference adds

These are stated here and should be promoted to `definition.md` when the
Steward next revises the principles list:

- **P8 — Projectional editing for managed Lisp files.** Agents write
  semantic forms; the server writes files. The file on disk is always a
  byte-identical fixed point of `(write (read f))`. Topological ordering,
  compile-order wrapping, `in-package`, header, footer, and
  dependency-graph updates are server responsibilities, not agent
  responsibilities.
- **P9 — One protocol, not two.** Every write through the projectional
  editor runs the full linter and maintainer pipeline. Findings surfaced
  during a write are returned to the agent as part of the tool response.
  The agent that consumes MCP and the CI script that consumes `atelier
  lint` read the same finding class hierarchy.

---

## Revision History

| Date | Change |
|---|---|
| 2026-04-11 | Initial draft during slice 010 design session. Constrained file model, `form` record, W1–W6 wrinkles, slice dependency map, non-goals, P8 and P9 principles. |
| 2026-04-11 | Added the System Layering section. The projectional editor is extracted as a standalone system `org.melusina.atelier/editor` (package `atelier/editor`) from the start, with the MCP server depending on it as an adapter rather than hosting it. Tests live under `testsuite/editor/` (package `atelier/testsuite/editor`) inside the consolidated `org.melusina.atelier/testsuite`, mirroring slice 009's organisation for MCP tests — no independent test system. Slice dependency map updated to split each slice's delivery into an editor column and an MCP-adapter column. Two new invariants recorded: fresh-SBCL loads `/editor` without MCP, and `/editor` never depends on `/mcp`. |
| 2026-04-12 | **Major revision from slice 010 Tactician planning interview.** The `form` record is now `toplevel-form` (CLOS class, four slots: kind, name, body, eval-when). `kind` is a symbol (`common-lisp:defun`, not a keyword), supporting third-party macros as first-class citizens. `body` is an Eclector CST preserving `#+`/`#-` inside form bodies as structure (following established CL practice). Three slots dropped: `docstring` (stays in body — extraction is kind-specific and breaks for unknown macros), `feature-expression` (whole-form guards handled at ASDF `:components` level), `source-position` (editor owns ordering via topological sort + alphabetical tie-break; no slot influences it). Dependencies computed on demand, never stored. `toplevel-form-ast` is a derived accessor returning a lossy s-expression view with reader conditionals evaluated against a caller-supplied feature set. API names follow CL conventions: `read-toplevel-form-from-string`, `write-toplevel-form-to-string`, `normalize-toplevel-form`. The `forbidden-toplevel` condition renamed to `unexpected-toplevel-form` with a `decompose` restart for PROGN. Mutual recursion handled via `(declaim (ftype ...))` forward declarations. |
