# Slice 010: Editor foundation and MCP eval

**Status:** Planned
**Type:** New capability
**Goal addressed:** G5 — MCP server exposes REPL evaluation, debugger and image lifecycle, CLOS introspection, and rename refactorings
**Backlog items:** #6 (MCP child image lifecycle, eval, package/symbol introspection); partial down-payment on the projectional editor track (see `product/reference/projectional-editor-design.md`).
**Planned start / end:** 2026-04-11 / TBD (Tactician predicts at the planning interview).
**Actual end:** _(filled on completion)_
**Implementation phases:**
  - Phase 1: `product/slice/010-editor-foundation-and-mcp-eval/implementation-1.md` — Planned
  - _(Tactician may split into phase 2 during the planning interview — see Notes for the Tactician.)_

---

## What changes for users

After this slice, an MCP-aware client (Claude Desktop, Cursor, or any MCP host) connected to `atelier-mcp` can:

- **Evaluate forms in a child SBCL image.** The MCP server spawns and manages a child SBCL process, connects to it over `socketpair(2)` / SWANK, sends forms to evaluate, and streams back results and structured stdout/stderr. The parent MCP image is the *manager*; the child is the *workbench*. The child is created lazily on the first eval call, reused across calls within a session, and shut down when the MCP server exits.
- **Canonicalize a Lisp form to its pretty-printed, maintainer-applied equivalent.** A new tool `atelier:canonicalize-form` accepts a form as a string (or as a JSON-encoded s-expression, Tactician to pin the wire shape) and returns the form as Atelier would write it: pretty-printed, with earmuffs applied, LOOP keywords normalized, bare lambdas replaced, and so on. This is the first exposure of the projectional editor's round-trip contract. Every subsequent projectional-editor slice builds on this primitive.
- **Evaluate a form and see it canonicalized in the same response.** The `atelier:eval-form` tool returns *both* the evaluation result (printed, or structured if the form returns multiple values) *and* the canonicalized form that the editor would have written for that input. This means every single eval during slice-010 testing exercises the round-trip on real input, at no additional cost.
- **Introspect packages and symbols in the child image.** Four tools: `atelier:list-packages` (all packages in the child), `atelier:list-package-symbols` (all symbols in a package, with metadata rich enough for slice 015's `add-export` and `remove-export` to not need a retrofit), `atelier:describe-symbol` (function signature, slot list for classes, CLOS method list, documentation), `atelier:find-definition` (source location for a symbol, via the child's `sb-introspect`).
- **Run a testsuite in isolation.** Two tools covering the two dominant patterns: `atelier:run-tests-fresh` spawns a *fresh* SBCL child, loads a system, runs `(asdf:test-system …)`, and returns the result (cold start, stateless, equivalent to the pre-commit discipline). `atelier:run-tests-in-child` runs `(asdf:test-system …)` in the existing per-session child (faster, iterative, carries compiled state from prior evals). Agents choose based on what they need.

**What this slice does not ship:**

- Interactive debugger, restarts, or sldb-equivalent UI. Those are slice 011.
- ASDF structural operations beyond the read-only surface needed for `atelier:run-tests-*`. Slice 012 covers `asdf-operate`, Quicklisp, and the Confidence integration.
- Documentation tools (`describe`, `apropos`, hyperspec, macroexpand). Slice 013.
- Xref, CLOS inspector, trace, `who-tests`. Slice 014.
- **File-level form CRUD: `add-form`, `update-form`, `remove-form`, `rename-form`, `move-form`.** These are slice 015. Slice 010 ships the `form` record and `canonicalize-form` as a pure in-memory operation; no managed Lisp file is ever read or written through the projectional pipeline in this slice. See `product/reference/projectional-editor-design.md` § "Slice dependency map" for the rationale.
- `defpackage` and `defsystem` structural APIs. Slice 015.
- Any MCP tool that writes to a project file. Slice 010 is read + eval only.
- Lint-passthrough as an explicit tool. Slice 015 (but the canonicalize-form primitive exercises the same pipeline).

The slice's headline deliverable is the creation of `org.melusina.atelier/editor` as a standalone ASDF system, with the `form` record and `canonicalize-form` as its first public API. Everything else — child image, eval, introspection, testsuite runner — is the MCP server growing the first real adapter layer over the editor.

## Specification references

- **Projectional editor design contract** — `product/reference/projectional-editor-design.md`. The system layering section (`atelier/editor` never depends on `atelier/mcp`), the `form` record slot list, the forbidden-at-toplevel rules, and the slice dependency map are all normative for this slice.
- **MCP specification** — Model Context Protocol, `2024-11-05` as pinned by slice 009. Slice 010 adds new tool handlers and (if needed) one or two new tool-result content types; no protocol-level changes.
- **Python prototype** — `/Users/michael/Workshop/lisp_mcp/sbcl_eval.py`. Slice 009 already extracted the naming and envelope conventions into `product/slice/009-mcp-skeleton/references/python-prototype-notes.md`. The Tactician re-reads §"Useful patterns to revisit in slice 010" before planning: this is where the child-image lifecycle, `socketpair(2)` transport, and eval streaming patterns are recorded. **Do not transliterate.** The Python implementation is a starting point, not a contract.
- **SWANK** — the in-image backend used by the child. Tactician pins the exact version during planning (SLIME vs. SLY divergence matters). The three `image-connection` generics (`connection-eval`, `connection-shutdown`, `connection-alive-p`) declared in slice 009 are the integration points. Slice 010 writes the first concrete subclass of `image-connection`.
- **`closer-mop`** — new dependency for slice 010, used by the child-image package and symbol introspection tools. The Tactician validates during the OSS check that `closer-mop` covers SBCL cleanly and does not pull in secondary dependencies. If MOP access can be done with SBCL-native APIs (`sb-mop`, `sb-introspect`) without sacrificing portability-of-intent, the Tactician may drop `closer-mop` in favor of an internal abstraction layer — record the decision either way in `implementation-1.md`.
- **Eclector** — already a core dependency. The `form` record uses Eclector's CST output as its canonical input representation. Slice 010 reuses the existing `parse-common-lisp` and pretty-printer entry points; it does not add new Eclector integration.
- **ASDF source registry** — used by `atelier:run-tests-fresh` to find systems without loading them. Same API slice 009 already uses for `atelier:list-systems`.

## Stories

### S1 — `org.melusina.atelier/editor` system and `#:atelier/editor` package

**In order to** depend on the projectional editor from any adapter (MCP server, future CLI, future Emacs integration), **a** developer **can** declare `(:depends-on "org.melusina.atelier/editor")` in their `.asd` file and `(:use #:atelier/editor)` in their package definition.

**Acceptance criteria:**

- Given `org.melusina.atelier.asd` declares the secondary system `org.melusina.atelier/editor`, when `(asdf:load-system "org.melusina.atelier/editor")` is called from a fresh SBCL, then it loads cleanly with zero warnings, zero style-warnings, zero notes.
- Given the system is loaded, when `(find-package "ATELIER/EDITOR")` is called, then it returns a package whose name is `"ATELIER/EDITOR"`.
- Given a fresh SBCL, when `(asdf:load-system "org.melusina.atelier/editor")` is called **in a subprocess**, then `com.inuoe.jzon`, `bordeaux-threads`, `closer-mop`, and the `atelier/mcp` package are **not** present in the image. This is the "editor is standalone" invariant, enforced by a test in `testsuite/editor/` that shells out to a fresh SBCL and greps the list of loaded systems and packages.
- Given `org.melusina.atelier/editor` is loaded in a fresh SBCL, when its exported symbols are listed, then they include at minimum: the class `form`, its constructor `make-form` and accessors (`form-kind`, `form-name`, `form-body`, `form-docstring`, `form-eval-when`, `form-features`, `form-source-position`), `canonicalize-form`, `read-form`, `write-form`, and the condition types `invalid-form` and `forbidden-toplevel`.
- Given the package is loaded, when `(documentation 'atelier/editor:form 'type)` is called, then it returns a docstring pointing at `product/reference/projectional-editor-design.md` as the normative design contract.

### S2 — The `form` record and its metadata fields

**In order to** represent a toplevel Lisp form as a semantic object rather than as text, **a** developer **can** construct a `form` instance with kind, name, body, docstring, eval-when situations, feature expression, and source position, and expect the record to round-trip cleanly through the reader and writer.

**Acceptance criteria:**

- Given `(make-form :kind :defun :name 'foo :body '(defun foo (x) (1+ x)))`, when constructed, then the resulting `form` has `form-kind = :defun`, `form-name = 'foo`, `form-body = '(defun foo (x) (1+ x))`, `form-docstring = nil`, `form-eval-when = '(:load-toplevel :execute)`, `form-features = nil`, and `form-source-position = nil`.
- Given a `form` with a docstring-carrying body like `(defun foo (x) "Add one." (1+ x))`, when constructed via `read-form` from that string, then `form-docstring` is `"Add one."` and `form-body` is `(defun foo (x) (1+ x))` — the docstring is extracted as metadata, not left in the body.
- Given a `form` wrapped in `(eval-when (:compile-toplevel :load-toplevel :execute) (defun helper () ...))`, when constructed via `read-form`, then `form-eval-when = '(:compile-toplevel :load-toplevel :execute)` and `form-body = '(defun helper () ...)` with the `eval-when` envelope removed.
- Given a `form` wrapped in `#+sbcl (defun foo () ...)`, when constructed via `read-form`, then `form-features = :sbcl` and `form-body = '(defun foo () ...)` with the reader conditional removed. Similarly for `#+(or sbcl ecl)`.
- Given a `form` with `form-features = :sbcl`, when `write-form` emits it, then the output starts with `#+sbcl` before the pretty-printed form.
- Given a `form` whose `form-eval-when` is the default `'(:load-toplevel :execute)`, when `write-form` emits it, then the output does **not** wrap the form in an explicit `eval-when` (default is elided).
- Given a `form` whose `form-eval-when` is any non-default situation list, when `write-form` emits it, then the output wraps the body in `(eval-when (...) ...)` with the declared situations.
- Given an attempt to construct a `form` with a forbidden toplevel shape (a `progn`, an `in-package`, a side-effectful `(format t ...)`, a `#.` reader macro, a mid-body feature conditional), when `read-form` is called on that input, then a `forbidden-toplevel` condition is signaled carrying the offending shape and a pointer at the reference document's "Forbidden at toplevel" section.
- Given `read-form` is called on a toplevel `progn` of definitions, when handled, then the condition's `continue` restart decomposes the `progn` into a list of `form` records, one per child form — honoring the reference document's "Toplevel `progn`" decomposition rule.

### S3 — `canonicalize-form` round-trip primitive

**In order to** validate that the projectional editor's read-write pipeline is a fixed point over the form record, **a** developer **can** call `(atelier/editor:canonicalize-form input)` and receive back a string that is the pretty-printed, maintainer-applied form as Atelier would write it; and calling `canonicalize-form` on its own output returns the same output (idempotency).

**Acceptance criteria:**

- Given `(canonicalize-form "(defun foo(x)(1+ x))")`, when called, then the result is a pretty-printed string matching Atelier's canonical indentation, with spaces between tokens, on the same formatting the existing pretty-printer produces for the identical form.
- Given `(canonicalize-form "(defun *foo* (x) x)")`, when called, then the result is the DEFUN with the maintainer `fix-earmuffs` not applied to function names (earmuffs apply only to DEFVAR / DEFPARAMETER / DEFCONSTANT — the existing behaviour is preserved). This is a regression guard against the canonicalize pipeline treating DEFUN like DEFVAR.
- Given `(canonicalize-form "(defvar foo 42)")`, when called, then the result applies `fix-earmuffs` and returns `"(defvar *foo* 42)"` — pretty-printed, maintainer-applied, using the existing maintainer pipeline.
- Given a form that produces any `finding` during canonicalization, when returned, then `canonicalize-form` returns two values: the canonicalized string as the primary value, and a list of findings (as finding instances) as the secondary value. Adapters decide how to surface findings to their caller.
- Given `(canonicalize-form s)` returns `s'`, when `(canonicalize-form s')` is called, then the result is `s'` byte-identical — **the canonicalize operation is idempotent**. This is a fast test asserted on every fixture in S11.
- Given the canonicalize pipeline encounters a forbidden-at-toplevel form, when called, then a `forbidden-toplevel` condition is signaled with the `continue` restart offering to drop the offending form (not silently — the adapter must opt in).

### S4 — Concrete `image-connection` subclass: `swank-socket-connection`

**In order to** actually run eval and introspection in a child SBCL image, **a** developer **can** instantiate `atelier/mcp:swank-socket-connection` and expect a child SBCL to be spawned, an in-image SWANK to be started, a `socketpair(2)` to connect parent and child, and the three `image-connection` generics (`connection-eval`, `connection-shutdown`, `connection-alive-p`) to route through the socket.

**Acceptance criteria:**

- Given `org.melusina.atelier/mcp` is loaded, when `(find-class 'atelier/mcp:swank-socket-connection)` is called, then it returns a `standard-class` that inherits from `atelier/mcp:image-connection`.
- Given `(make-instance 'atelier/mcp:swank-socket-connection)`, when constructed, then the SBCL subprocess is spawned via `uiop:launch-program` (or equivalent), a `socketpair(2)` is allocated, the child loads SWANK (version pinned by Tactician), the child starts the SWANK server on the child end of the socketpair, and the parent connects to the parent end. The whole sequence completes in under 5 seconds on an unloaded machine.
- Given a live `swank-socket-connection`, when `(connection-eval conn '(+ 1 2))` is called, then the result is `3` (or the implementation's canonical printed form for `3`), **not** a `not-implemented` signal.
- Given a live `swank-socket-connection`, when the parent process dies unexpectedly, then the child SBCL terminates within a bounded time (Tactician chooses: SIGTERM, socket closure detection, or a watchdog — whatever the Python prototype notes recommend).
- Given a `swank-socket-connection`, when `connection-shutdown` is called, then the child SBCL is terminated cleanly, the socket is closed, and `connection-alive-p` returns NIL.
- Given a fresh `swank-socket-connection`, when multiple `connection-eval` calls are made in sequence, then each call sees the state left by the previous call (e.g. `(defvar *x* 1)` followed by `*x*` returns `1`). The connection is stateful within its lifetime.
- Given the user's machine does not have SBCL on `PATH`, when `make-instance 'swank-socket-connection` is called, then a clear condition `child-image-spawn-failed` is signaled with a message naming the missing binary and pointing at the Atelier prerequisites section of CLAUDE.md. **No silent fallback.**

### S5 — `atelier:eval-form` tool and child lifecycle management

**In order to** evaluate a form in an isolated workbench image from an MCP client, **an** agent **can** call the `atelier:eval-form` tool with a form string and receive the evaluation result, any structured stdout/stderr, and the canonicalized version of the form.

**Acceptance criteria:**

- Given the MCP server has no active child image, when the first `tools/call atelier:eval-form` arrives, then a `swank-socket-connection` is created lazily, stored on the session, and used for the call. Session lifetime and child lifetime are identical: when the session ends, the child is shut down.
- Given an active child image, when subsequent `atelier:eval-form` calls arrive in the same session, then the same child is reused — no fresh SBCL spawn per call.
- Given a form that evaluates to a value, when `atelier:eval-form` is called, then the tool response is a JSON object with fields `value` (printed representation), `canonicalized-form` (output of `canonicalize-form` on the input), `stdout` (captured during eval, possibly empty), `stderr` (captured during eval, possibly empty), and `duration-ms`.
- Given a form that signals an uncaught condition in the child, when `atelier:eval-form` is called, then the tool response is a `tools/call` result with `isError: true` carrying the condition's printed representation and the child's backtrace (first 20 frames). The MCP server continues serving; the child continues running in its toplevel repl.
- Given a form with multiple return values, when `atelier:eval-form` is called, then the `value` field is a JSON array of printed representations, preserving order.
- Given a form that takes longer than a configurable timeout (default 30 seconds), when `atelier:eval-form` is called, then the call returns an `isError: true` result with a "timed out" message, and the child is interrupted (not killed — the child survives for subsequent calls). The timeout is configurable per session and per call.
- Given a connection-alive check before dispatch, when `connection-alive-p` returns NIL (child died), then the MCP server automatically spawns a fresh child on the next `eval-form` call and surfaces a `child-restarted` notice in the response. The agent never sees a dead-child error; it sees a restart notice.

### S6 — Package and symbol introspection tools

**In order to** explore the child image's packages and symbols without loading any project file, **an** agent **can** call `atelier:list-packages`, `atelier:list-package-symbols`, `atelier:describe-symbol`, and `atelier:find-definition`.

**Acceptance criteria:**

- Given the tool `atelier:list-packages` is registered, when invoked with no arguments, then the result is a JSON array of package descriptors: `name`, `nicknames`, `use-list`, `used-by-list`, `external-symbols-count`, `internal-symbols-count`. Computed in the child image via `do-all-symbols` or an equivalent that does not load any system.
- Given the tool `atelier:list-package-symbols` is registered, when invoked with `package-name` and optional `status` (one of `:external`, `:internal`, `:inherited`, `:any` — default `:external`), then the result is a JSON array of symbol descriptors, each including at minimum: `symbol-name`, `package-name`, `status`, `home-package-name`, `symbol-kind` (one of `:function`, `:generic-function`, `:macro`, `:special-form`, `:variable`, `:constant`, `:class`, `:condition`, `:type`, `:unbound`), and a truncated `documentation` string. This is the field set slice 015's `add-export` / `remove-export` will consume.
- Given the tool `atelier:describe-symbol` is registered, when invoked with a `symbol-designator` (string like `"cl:car"` or `"atelier/linter:lint-system"`), then the result is a JSON object carrying: function signature (if a function), slot list (if a class), method list with specializers (if a generic function), condition slots (if a condition), `documentation`, and the symbol's home package.
- Given the tool `atelier:find-definition` is registered, when invoked with a `symbol-designator`, then the result is a JSON object `{"source-file": "/path/to/file.lisp", "line": N, "column": M}` or `null` if source location is not available. Uses `sb-introspect:find-definition-sources-by-name` in the child.
- Given a symbol that does not exist in the child image, when any introspection tool is called with its name, then the response is a `tools/call` result with `isError: true` carrying a "no such symbol" message; the server continues serving.
- Given the Tactician's OSS Check decision on `closer-mop`, when introspection is implemented, then the implementation either uses `closer-mop` (portable across CL implementations) or uses SBCL-native APIs (`sb-mop`, `sb-introspect`) behind a thin abstraction. The decision is recorded in `implementation-1.md` §"OSS Components".

### S7 — Testsuite runner tools

**In order to** validate a system's testsuite from an MCP client without contaminating the dev image, **an** agent **can** call `atelier:run-tests-fresh` (cold, stateless, equivalent to the pre-commit pattern) or `atelier:run-tests-in-child` (warm, iterative, uses the session's existing child).

**Acceptance criteria:**

- Given the tool `atelier:run-tests-fresh` is registered, when invoked with `system-name`, then a **fresh** SBCL subprocess is spawned (not the session's existing child), the system is loaded via `(asdf:load-system ...)`, the test system is run via `(asdf:test-system ...)`, and the result captured. The subprocess is shut down after the call; the session's existing child is untouched.
- Given the tool result, when returned, then the JSON object carries: `system`, `passed` (integer), `failed` (integer), `errored` (integer), `skipped` (integer), `duration-ms`, and `output` (captured stdout/stderr, truncated at a configurable limit). Parsed from Confidence's test output format using the existing `define-testcase` convention; Tactician pins the exact parser during planning.
- Given the tool `atelier:run-tests-in-child` is registered, when invoked with `system-name`, then the same sequence runs but in the session's existing child image, reusing any compiled state. Faster (no cold SBCL start) but non-hermetic.
- Given a system that fails to load in the fresh subprocess, when `atelier:run-tests-fresh` is called, then the result has `isError: true` with the load error captured; the subprocess is cleaned up.
- Given a system with no test system declared (no `org.melusina.atelier/testsuite` equivalent), when either runner is called, then the result has `isError: true` with a clear "no test system" message.
- Given Atelier itself as the target system, when `atelier:run-tests-fresh :system "org.melusina.atelier"` is called, then the result reports at minimum the same pass count the project's current suite reports (as of slice 009 closure: 479/479). This is the "dogfood" sanity check.

### S8 — `atelier:canonicalize-form` MCP adapter tool

**In order to** canonicalize a form from an MCP client without running it in a child image, **an** agent **can** call `atelier:canonicalize-form` with a form string and receive the pretty-printed, maintainer-applied form and any findings raised during canonicalization.

**Acceptance criteria:**

- Given the tool `atelier:canonicalize-form` is registered, when invoked with a `form` string argument, then the tool body is effectively `(multiple-value-bind (s findings) (atelier/editor:canonicalize-form form) ...)` and returns a JSON object `{"canonicalized": s, "findings": [...]}`.
- Given the input is a forbidden-at-toplevel form (e.g. a toplevel `format` call), when the tool is invoked, then the response is a `tools/call` result with `isError: true` carrying the `forbidden-toplevel` condition's printed representation and a reference-doc pointer.
- Given the input is a decomposable toplevel `progn`, when the tool is invoked with an optional `:decompose-progn t` argument, then the response is a JSON array of canonicalized forms (one per child) and a merged `findings` array. Without the flag, the response is an error with a "toplevel progn must be decomposed explicitly" message.
- Given the tool runs **entirely in the parent MCP image**, when invoked, then no child-image call is made. This is the bright-line check: `canonicalize-form` is a pure editor operation, not an eval operation. Verified by a test that stubs out the child-image path and confirms the tool still returns the expected output.

### S9 — Testsuite module `testsuite/editor/` inside the consolidated testsuite

**In order to** regression-test the editor without creating a new ASDF test system, **a** developer **can** add editor tests to `testsuite/editor/` under package `atelier/testsuite/editor`, and reach them via `(asdf:test-system "org.melusina.atelier")`.

**Acceptance criteria:**

- Given `org.melusina.atelier.asd` is updated, when `(asdf:test-system "org.melusina.atelier")` is called, then it runs `atelier/testsuite:run-all-tests` which now includes the editor test module alongside the existing `testsuite/mcp/` module. No new test system is introduced.
- Given `(find-package "ATELIER/TESTSUITE/EDITOR")` is called after loading `org.melusina.atelier/testsuite`, then it returns a package whose name is `"ATELIER/TESTSUITE/EDITOR"`.
- Given the editor tests are run, when they complete, then the pass count delta from the slice-009 baseline (479/479) is the number of new assertions added in this slice — Tactician predicts a *wide range* per the slice-008 calibration lesson (3–5× the initial estimate).
- Given the full suite is run in a fresh SBCL subprocess (the slice-009 subprocess-load test), then all editor, MCP, and base atelier tests pass in one `(atelier/testsuite:run-all-tests)` call.
- Given the editor-loads-without-mcp invariant (S1's subprocess check), when run, then the assertion is in `testsuite/editor/` (not `testsuite/mcp/`), because the invariant is about the editor, not about MCP.

### S10 — `closer-mop` dependency decision and OSS check

**In order to** decide whether `org.melusina.atelier/editor` or `org.melusina.atelier/mcp` takes a `closer-mop` dependency, **the** Tactician **can** run the OSS Check stage of the planning interview, examine SBCL-native alternatives, and pin the decision in `implementation-1.md`.

**Acceptance criteria:**

- Given the Tactician planning interview, when Stage 2 (OSS and Prior Art Check) runs, then `closer-mop` is evaluated against: (a) `sb-mop` + `sb-introspect` for SBCL-only, (b) no MOP (plain `do-symbols` + `find-class` + `symbol-function`), and (c) `closer-mop`. The decision is recorded in `implementation-1.md` §"OSS Components".
- Given the Tactician planning interview, when Stage 2 runs, then a Quicklisp / GitHub search for existing **projectional editors for Common Lisp** is performed and summarized in `references/prior-art.md` (this slice directory). The search is bounded — not an exhaustive survey — but it must check for: standalone projectional editors, Common Lisp refactoring tools that operate on forms rather than text, Eclector-based tooling that already does form round-tripping, and SLIME/SLY refactoring packages. If something close to what we are building exists, the plan adjusts. If nothing does, the fact is recorded.
- Given the Tactician planning interview, when Stage 2 closes, then either zero new library dependencies are introduced beyond `closer-mop` (if that is the final decision) or any other chosen library is justified in `implementation-1.md` with license, version, and rationale. No transitive dependency creep.

### S11 — Fixture directory for `canonicalize-form` round-trip tests

**In order to** exercise the `canonicalize-form` primitive across the full range of form kinds, metadata combinations, and edge cases, **a** developer **can** add a `.text` fixture to `testsuite/fixtures/editor/canonicalize/` and have a test automatically generated.

**Acceptance criteria:**

- Given the directory `testsuite/fixtures/editor/canonicalize/` exists, when the editor test module loads, then every `.text` file in it is auto-discovered and turned into a `canonicalize-form` round-trip test, mirroring the slice-007 auto-discovery pattern for autofix-cycle fixtures.
- Given a fixture file with the two-document format `input --- expected-canonical`, when the test runs, then `(canonicalize-form input)` must match `expected-canonical` byte-for-byte.
- Given a fixture file, when the first round-trip passes, then `(canonicalize-form expected-canonical)` must also equal `expected-canonical` (idempotency — the fixed-point property for `canonicalize-form`).
- Given the initial fixture set, when the slice ships, then it includes at minimum: one fixture per `form-kind` (`:defun`, `:defclass`, `:defgeneric`, `:defmethod`, `:defmacro`, `:defvar`, `:defparameter`, `:defconstant`, `:deftype`, `:define-condition`), one fixture exercising docstring extraction, one fixture exercising `eval-when` wrapper extraction, one fixture exercising `#+sbcl` wrapper extraction, one fixture exercising `fix-earmuffs` application, and one fixture exercising the forbidden-toplevel rejection (with an expected condition class). Tactician may add more based on the `form` record test plan.
- Given the fixture format, when the Tactician writes the plan, then the format is pinned in `implementation-1.md` so that "adding a fixture" is a single-file operation for the Maker and for future maintainers.

## Quality Criteria

- [ ] `(asdf:load-system "org.melusina.atelier/editor")` in a fresh SBCL produces zero warnings, zero style-warnings, zero notes.
- [ ] `(asdf:load-system "org.melusina.atelier/mcp")` in a fresh SBCL (which will transitively load `/editor`) produces zero warnings, zero style-warnings, zero notes.
- [ ] **Editor-standalone invariant:** `(asdf:load-system "org.melusina.atelier/editor")` in a fresh SBCL subprocess does **not** load `com.inuoe.jzon`, `bordeaux-threads`, or the `atelier/mcp` package. Enforced by a test.
- [ ] **Editor never depends on MCP:** grepping `src/editor/` for any reference to `atelier/mcp` or `org.melusina.atelier/mcp` returns zero hits. Enforced as a slice-closure check.
- [ ] Full regression `(asdf:test-system "org.melusina.atelier")` passes in a fresh SBCL subprocess (per INV-4, now automated by slice 009).
- [ ] No tool handler can crash the MCP server: every handler is wrapped in a `handler-case` that converts conditions to JSON-RPC error results (same discipline as slice 009).
- [ ] No new MCP dependency beyond what slice 009 already pulled in, **except** `closer-mop` if the Tactician decides in its favor. The decision is recorded in `implementation-1.md` §"OSS Components".
- [ ] No managed project file is written through the projectional pipeline in this slice. Verified by code review during phase closure: `src/editor/` contains no `with-open-file` for a `:direction :output` on any `.lisp` file in a project directory. (Writing to fixture files in `testsuite/` is fine; writing to template files is fine; writing to project source is not.)
- [ ] `canonicalize-form` is a fixed point on its own output (asserted by every fixture in S11).
- [ ] The `form` record round-trip `(read-form (write-form f))` is byte-equivalent to `f` for every fixture.
- [ ] Child image lifecycle: a session that spawns a child always shuts the child down when the session ends. No orphan SBCL processes after test suite completes — verified by a slow test that counts SBCL processes before and after.
- [ ] Timeouts on `atelier:eval-form` are honored: a form that would run forever is interrupted within `timeout + 1s`.
- [ ] Prior-art search for projectional editors is performed and summarized in `references/prior-art.md` before `implementation-1.md` is finalized.
- [ ] **INV-11 audit:** templates under `resource/template/` are grepped for any reference to symbols this slice introduces. None expected; the grep is a slice-closure step.

## Definition of Ready

- [x] Stories traceable to backlog items — slice maps to backlog item #6 and is a partial down-payment on the projectional editor track
- [x] Stories sized ≤ 2 days each — eleven stories, each scoped to one subsystem; Tactician may split the slice into two implementation phases if the aggregate warrants it
- [x] Acceptance criteria written for all stories
- [x] Quality criteria defined
- [x] Spec references identified — projectional editor design contract, MCP spec, Python prototype, SWANK, `closer-mop`, Eclector, ASDF source registry

## Definition of Done

- [ ] All eleven stories complete with acceptance criteria passing
- [ ] Quality criteria passing
- [ ] Full test suite passes in fresh SBCL subprocess (per INV-4, automated since slice 009)
- [ ] Manual MCP-client smoke test recorded: launch `atelier-mcp` from an MCP client, invoke `atelier:eval-form` with `(+ 1 2)` and observe `3`, invoke `atelier:canonicalize-form` with `"(defvar foo 42)"` and observe `"(defvar *foo* 42)"`, invoke `atelier:list-packages`, invoke `atelier:run-tests-fresh :system "org.melusina.atelier"` and observe the pass count
- [ ] All implementation phases have completion notes
- [ ] `product/slice/010-editor-foundation-and-mcp-eval/retrospective.md` created
- [ ] `product/backlog.md` updated (item #6 marked delivered for the editor foundation + MCP eval half; any deferred work moved back to the queued section)
- [ ] `product/roadmap.md` updated (010 moved from Next to Now on slice start; 010 moved to Completed on slice closure; 011 moved to Next)
- [ ] `CLAUDE.md` updated with a brief editor section pointing at `atelier/editor:canonicalize-form` as the editor's first public API and naming the child-image entry point in `/mcp`
- [ ] No new template under `resource/template/` references any new package (verified by grep)
- [ ] **Projectional editor design contract updates:** if slice execution surfaces new wrinkles (W7+), they are recorded in `product/reference/projectional-editor-design.md` at slice closure — not before

---

## Notes for the Tactician

This is a **dual-deliverable slice** — an editor foundation plus an MCP eval layer that depends on it. My working mental model is two implementation phases:

- **Phase 1** — editor foundation. Create `org.melusina.atelier/editor`, ship the `form` record, ship `canonicalize-form` + `read-form` + `write-form`, ship the `testsuite/editor/` module, ship the fixture directory and the auto-discovered tests. **No MCP code, no child image.** The phase-1 deliverable loads cleanly in a fresh SBCL and passes all editor tests without any MCP dependency. This is the load-bearing foundation; getting it right means phase 2 is mostly plumbing.
- **Phase 2** — MCP eval layer. The concrete `swank-socket-connection` subclass, the child-image lifecycle management, the six new MCP tools (`eval-form`, `canonicalize-form` adapter, `list-packages`, `list-package-symbols`, `describe-symbol`, `find-definition`, `run-tests-fresh`, `run-tests-in-child`), and the MCP-side tests. This phase imports `atelier/editor` as a library and never touches `src/editor/`.

But I'm not pinning this split. The Tactician may decide during the planning interview that one phase is enough, or that Phase 2 itself needs to be split (e.g., child-image + eval in Phase 2, introspection + testsuite runner in Phase 3). The split is a Tactician call based on step-table size, risk concentration, and what the Maker can coherently execute in one push. The only non-negotiable is that Phase 1's deliverable passes tests **before** any child-image code is written. The editor must exist and work before the MCP server starts calling into it.

### Questions to resolve during the planning interview

1. **SWANK vs. SLY backend.** The child-image side of `swank-socket-connection` needs a SWANK server (or SLY's `slynk`). Pick one, pin the version, record the rationale. Both work; the choice is between the two communities' maintenance posture and whatever the Python prototype used.
2. **`socketpair(2)` vs. TCP on localhost.** Slice 009 committed to `socketpair(2)`. Verify that SWANK/SLY supports AF_UNIX socket pair input as a transport in the child image. If it does not out of the box, the options are: (a) write a thin adapter that reads from the socket and calls SWANK's in-process functions, (b) fall back to TCP on 127.0.0.1 with a random port, (c) pre-emptively rewrite SWANK's transport layer. My instinct is (a) is the right answer and (c) is out of scope. Tactician decides.
3. **`closer-mop` vs. `sb-mop`/`sb-introspect`.** OSS Check. Record the decision and its rationale. My lean: SBCL-native behind a thin internal facade, so we do not pay for `closer-mop` for one slice's needs. But the Tactician may disagree.
4. **Fixture format for `testsuite/fixtures/editor/canonicalize/`.** The slice-007 autofix-cycle fixtures used a four-document format. Canonicalize is simpler (input → output with optional findings). Pin the exact YAML front-matter fields and separator placement in `implementation-1.md` so the Maker doesn't reinvent it.
5. **`form` record representation.** `defclass` with accessors? `defstruct`? Tagged alist? `defclass` is the project convention and the slice 009 pattern; I am assuming it. If the Tactician sees a reason to use `defstruct` (speed? conciseness?), record the deviation.
6. **Free-variable analysis — deferred or built?** Slice 015 will need a real free-variable analyzer for the topological sort. Slice 014 will feed xref queries into it. **Slice 010 does not need it** — `canonicalize-form` is a single-form operation. The Tactician must **not** build the analyzer in slice 010. If a story tempts toward "compute dependencies during canonicalize", push back to me.
7. **The `form` record and CST representation.** Eclector already produces CST nodes. Is `form-body` the CST, or is it the plain s-expression? My lean is plain s-expression for storage, with CST available on-demand via `parse-common-lisp`. CST-as-storage is heavier and makes the fixture format harder. Tactician decides and records in `implementation-1.md`.
8. **`atelier:eval-form` wire format.** Does the form arrive as a string (agent-readable, readable by `read-from-string` in the child), as a JSON-encoded s-expression (harder to author, more precise), or as both with auto-detection? My lean is **string only** for slice 010; revisit if agents complain.

### References to create in `references/`

- **`prior-art.md`** — slice 010's OSS-check finding: projectional editors for Common Lisp (standalone or integrated). The Tactician writes this during the OSS Check stage. Format: a short list of candidates examined, a one-sentence disposition per candidate, and a final verdict ("nothing reusable" or "candidate X covers story S2, rework the plan").
- **`swank-child-lifecycle.md`** — the exact sequence of `uiop:launch-program` arguments, the `socketpair(2)` handoff, and the shutdown protocol. Cross-references the slice 009 `python-prototype-notes.md` §"Useful patterns to revisit in slice 010".
- **`form-record-decisions.md`** — the frozen design decisions: struct vs. class, CST vs. s-expression, default `eval-when`, how `form-source-position` is assigned on construction. This is what slice 011–015 will consult without re-asking.
- **`canonicalize-fixture-format.md`** — exact fixture file format for `testsuite/fixtures/editor/canonicalize/`. One page. Maker copies from this when adding a fixture.

### Risks to surface early

- **R1 — Hallucinated requirements.** Same as slice 009. Every exported symbol from `atelier/editor` must point at an acceptance criterion in this slice. The projectional editor is a track; the temptation to pre-build "just the scaffolding" for slice 015 is real. Resist. Slice 015 builds slice 015.
- **R2 — Child image lifecycle leaks orphan SBCLs in tests.** A test that spawns a child and then fails at an assertion may leave the child alive. Mitigation: every test that creates a `swank-socket-connection` wraps it in an `unwind-protect` or equivalent that calls `connection-shutdown`. Consider a `with-child-image` macro.
- **R3 — SWANK version skew.** The parent and the child must agree on the SWANK protocol version. Pin the version in the child's load path, not in the user's Quicklisp dist, to avoid the user's global install contaminating the child.
- **R4 — `fix-earmuffs` applied to DEFUN.** The existing maintainer is scoped to DEFVAR/DEFPARAMETER/DEFCONSTANT — verify this survives the `canonicalize-form` pipeline. If it doesn't, this is a pre-existing bug we inherit, not a slice 010 bug; record it and fix separately.
- **R5 — `form-source-position` semantics.** When is it assigned? On read from a file, it's the byte offset. On agent construction (no source), it's NIL. When `canonicalize-form` is called on a string, it's NIL. The field is really an optional stable-sort key for slice 015's topological sort; in slice 010 it is unused. Do not over-specify its semantics now.
- **R6 — Testsuite runner calling `(asdf:test-system "org.melusina.atelier")` in a fresh child image.** If the dev environment has an unpinned dependency (Quicklisp has updated one of Atelier's deps), the fresh child may fail to load the project in a way the session child would not. This is *correct* behaviour (the fresh runner is testing cold state), but it surprises agents. Document clearly in the tool description.
- **R7 — SBCL not on PATH.** Raised in S4 acceptance criteria. Clear condition, clear message, no silent fallback.

### Project knowledge to apply

From `product/knowledge/` (Tactician reads before planning):

- **`patterns.md`** — "Hallucinated requirements from surface intuition" applies hard here. Apply R1 discipline.
- **`invariants.md`** — INV-4 (fresh-SBCL subprocess test) is now enforced by slice 009's `testsuite/mcp/` subprocess-load test. Slice 010 adds the editor-standalone variant. INV-11 (template grep) applies unchanged. Slice 009 added INV-12 through INV-16 as candidates; check which are promoted before planning.
- **`calibration.md`** — slice 009 predicted 50–100 new assertions, actual was 184 (3.7× the midpoint). Tactician's S9 estimate for editor + MCP test count should use a **3–5× wider range** than first instinct. Slice 010 is probably heavier than slice 009; expect 200–400 assertions.
- **`reworks.md`** — slice 009 had 9 reworks, 5 of them CL-language surprises already documented in `MEMORY.md`. Lesson: **re-read the knowledge files and the Common Lisp skill before Phase 2 execution**, not "as needed during execution."

---

This slice is the moment the projectional editor track starts producing load-bearing code. Everything from slice 011 through slice 016 will consume the `form` record and the `canonicalize-form` primitive this slice ships. Getting the API shape right matters more than getting the scope expansive. When in doubt, **ship less and ship it clean**.
