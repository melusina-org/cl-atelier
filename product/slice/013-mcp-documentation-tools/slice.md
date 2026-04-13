# Slice 013: MCP Documentation Tools

**Status:** Complete
**Type:** New capability
**Goal addressed:** G5
**Backlog items:** #9
**Planned start:** 2026-04-13
**Actual end:** 2026-04-13
**Implementation phases:**
  - Phase 1: product/slice/013-mcp-documentation-tools/implementation-1.md — Complete

---

## What changes for users

An AI agent (or human via MCP client) can now look up any Common Lisp
symbol in the locally-installed HyperSpec, read X3J13 issue writeups,
search symbols across all packages with `apropos`, macroexpand forms,
disassemble functions, and compile forms with full compiler diagnostic
capture. These are the documentation and introspection tools that make
the child SBCL a complete reference environment, not just an evaluator.

## Specification references

- [MCP specification](https://spec.modelcontextprotocol.io/specification/) — tool and resource registration
- [CLHS](http://www.lispworks.com/documentation/HyperSpec/Front/) — installed locally at `/opt/local/share/doc/lisp/HyperSpec-7-0/HyperSpec/`
- `Map_Sym.txt` — symbol-to-page mapping (978 entries)
- `Map_IssX.txt` — X3J13 issue-to-page mapping (365 entries)

## Stories

### S1: Apropos search across all packages
**In order to** find symbols by partial name without knowing which
package they belong to, **a** developer **can** call `apropos` with a
search string and optional package filter.
**Acceptance criteria:**
- Given search string `"MAP"` and no package filter, when `apropos` is called, then results include symbols from multiple packages (at minimum `MAPCAR`, `MAPHASH`)
- Given search string `"MAP"` and package filter `"COMMON-LISP"`, when `apropos` is called, then results contain only CL-package symbols
- Each result includes: symbol name, package, kind (function/variable/class/etc.), and whether it is external

### S2: HyperSpec symbol lookup
**In order to** read the authoritative CL specification entry for a
symbol without leaving the MCP session, **a** developer **can** call
`hyperspec-lookup` with a symbol name and receive the CLHS page
content.
**Acceptance criteria:**
- Given symbol name `"MAPCAR"`, when `hyperspec-lookup` is called, then the result contains the CLHS dictionary entry text (extracted from the local HTML file)
- Given a symbol name not in the HyperSpec (e.g. `"MY-PRIVATE-FN"`), when `hyperspec-lookup` is called, then the result indicates no entry found
- The tool reads from the local filesystem (`/opt/local/share/doc/lisp/HyperSpec-7-0/HyperSpec/`), never from the network
- The tool also returns the relative CLHS URL path for reference

### S3: HyperSpec symbol lookup as resource
**In order to** browse HyperSpec entries via the MCP resource
protocol, **a** developer **can** read `lisp://hyperspec/symbol/{name}`
resources.
**Acceptance criteria:**
- Given URI `lisp://hyperspec/symbol/mapcar`, when the resource is read, then it returns the same content as the `hyperspec-lookup` tool
- The resource MIME type is `text/html`

### S4: X3J13 issue lookup
**In order to** understand the rationale behind CL design decisions,
**a** developer **can** call `hyperspec-issue` with an issue name and
receive the X3J13 issue writeup.
**Acceptance criteria:**
- Given issue name `"ADJUST-ARRAY-DISPLACEMENT"`, when `hyperspec-issue` is called, then the result contains the issue writeup text from the local HyperSpec Issues directory
- Given a nonexistent issue name, when `hyperspec-issue` is called, then the result indicates no entry found
- The tool reads from the local filesystem, never from the network

### S5: X3J13 issues as resources
**In order to** browse X3J13 issues via the MCP resource protocol,
**a** developer **can** read `lisp://hyperspec/issues` (list) and
`lisp://hyperspec/issues/{name}` (individual issue) resources.
**Acceptance criteria:**
- Given URI `lisp://hyperspec/issues`, when the resource is read, then it returns a list of all X3J13 issue names (365 entries from `Map_IssX.txt`)
- Given URI `lisp://hyperspec/issues/ADJUST-ARRAY-DISPLACEMENT`, when the resource is read, then it returns the issue writeup
- The list resource MIME type is `application/json`; the individual issue MIME type is `text/html`

### S6: Macroexpand
**In order to** understand what a macro call expands to, **a**
developer **can** call `macroexpand` with a form string and receive
the expanded form.
**Acceptance criteria:**
- Given form `"(defun foo (x) x)"`, when `macroexpand` is called, then the result shows the macroexpansion (which for `defun` is implementation-specific but non-trivial)
- The tool supports both `macroexpand-1` (one step) and `macroexpand` (full) via a `fully` parameter (default: false = macroexpand-1)
- The expanded form is pretty-printed for readability
- The tool runs in the child SBCL via the child-worker pattern

### S7: Disassemble
**In order to** inspect the compiled machine code of a function, **a**
developer **can** call `disassemble` with a function designator and
receive the disassembly output.
**Acceptance criteria:**
- Given function designator `"CL:CAR"`, when `disassemble` is called, then the result contains assembly instructions
- Given a non-existent function, when `disassemble` is called, then the result indicates the function was not found
- The tool runs in the child SBCL via the child-worker pattern

### S8: Compile with notes
**In order to** check a form for compiler diagnostics without
side-effects in the child image, **a** developer **can** call
`compile-form` with a form string and receive compiler notes,
warnings, and errors.
**Acceptance criteria:**
- Given a form with a type warning (e.g. `"(defun bar () (+ 1 \"x\"))"` ), when `compile-form` is called, then the result includes the compiler diagnostic
- Given a clean form, when `compile-form` is called, then the result shows no diagnostics
- The tool captures SBCL's `sb-ext:compiler-note`, `warning`, and `error` conditions during compilation
- The tool runs in the child SBCL via the child-worker pattern

## Quality Criteria

- [x] All new tools follow the child-worker pattern (child-worker function + thin MCP wrapper) except HyperSpec tools which run in the parent (local file I/O only)
- [x] HyperSpec tools never make network requests — all reads are from the local filesystem
- [x] All new resources are registered in both concrete and template registries as appropriate
- [x] Full test suite passes (base atelier + MCP + editor) in a fresh SBCL subprocess
- [x] CLAUDE.md tool and resource counts updated

## Definition of Ready

- [x] Stories traceable to backlog items (#9)
- [x] Stories sized <= 2 days each
- [x] Acceptance criteria written
- [x] Quality criterion defined
- [x] Spec references identified

## Definition of Done

- [x] All stories complete with acceptance criteria passing
- [x] Quality criteria passing
- [x] Full test suite passes (695/695)
- [x] All implementation phases have completion notes
- [x] `product/slice/013-mcp-documentation-tools/retrospective.md` created
- [x] `product/backlog.md` updated
- [x] `product/roadmap.md` updated
