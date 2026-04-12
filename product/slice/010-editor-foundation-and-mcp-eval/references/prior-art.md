# Prior Art — Projectional Editors for Common Lisp

**Recorded:** 2026-04-12, Tactician Stage 2 (OSS and Prior Art Check)

## Survey scope

Searched Quicklisp/Quickdocs, GitHub, and Codeberg for:
1. Standalone projectional editors for CL
2. Form-as-record libraries (kind/name/body/metadata)
3. Libraries that extract eval-when/#+/docstring as metadata
4. File-level form CRUD over ASDF systems
5. `closer-mop` status

## Candidates examined

### ProjecturEd (github.com/projectured/projectured)

Generic projectional editor framework with bidirectional printer/reader and
change propagation. ~20k LoC, GUI-oriented, experimental, marked
work-in-progress. **Not usable.** Wrong shape (generic document editor, not
CL-file-specific) and wrong weight (~20k LoC dependency for ~500 LoC of our
own focused code).

### trivial-formatter (github.com/hyotang666/trivial-formatter)

Reads a file into a list of forms via an intermediate-object readtable,
pretty-prints them back. Has `fmt :system :supersede` for whole-file rewriting.
The intermediate objects are layout/comment markers, not semantic form records.
**Not a dependency.** Useful as a stylistic reference for the read/format cycle.
Caveat: canonicalises `::` to `:` on keywords, which would break our round-trip
contract.

### Eclector (s-expressionists/Eclector)

Already a core dependency of Atelier. CSTs are lower-level than what we need
(they represent s-expression syntax, not toplevel-form semantics). The custom
client protocol (`make-skipped-input-result`, `make-expression-result`) is
the foundation our `preserving-cst-client` builds on. **Used as the substrate,
not reused as a form-record library.**

### SLIME/SLY xref

Read-only cross-reference queries. No add/update/remove/rename-form. Not
applicable.

### Redshank (emacsattic/redshank)

Interactive Emacs buffer commands for enclosing/extracting bindings. Not
programmatic file-level CRUD. Unmaintained (in emacsattic). Not applicable.

### cl-project, cl-refactor, lisp-format

No credible Quicklisp hit matching file-level form CRUD. Not applicable.

### closer-mop

MIT-style license, tagged 1.0.0 mid-2025, maintained by Pascal Costanza,
active on current SBCL/CCL/ECL/CLISP/ABCL/LispWorks, shipped via Quicklisp.
Remains the portable MOP layer. No serious alternative besides
implementation-specific `sb-mop`. **Phase 2 concern** — Phase 1 needs no MOP.

## Verdict

**Build from scratch.** No existing CL library implements a projectional editor
in the sense Atelier needs. Eclector CSTs are the substrate; the form record,
the read/write pipeline, the `#+`/`#-` preservation via custom client, and the
C2 CST-to-text writer are ours to build. No new dependency for Phase 1.
