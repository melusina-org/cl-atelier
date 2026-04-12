# Implementation Phase 2 — Slice 010: MCP Eval Layer

**Slice:** `product/slice/010-editor-foundation-and-mcp-eval/slice.md`
**Phase:** 2 (of 2)
**Scope:** Concrete `child-connection` subclass of `image-connection`, SWANK
client protocol, 8 new MCP tools (eval-form, canonicalize-form,
list-packages, list-package-symbols, describe-symbol, find-definition,
run-tests-fresh, run-tests-in-child), and the `child-worker` ASDF system
loaded in the child SBCL. No modification to `src/editor/`.
**Prerequisites:** Phase 1 complete (583/583 tests). SBCL with Quicklisp.
SWANK available via `(require :swank)`.

## Back-link

Slice: `product/slice/010-editor-foundation-and-mcp-eval/slice.md`

## Prior phases

**Phase 1** (complete, 104 assertions, 583 total):
- `org.melusina.atelier/editor` system and package (15 exports → 16 via amendment)
- `toplevel-form` record (5 slots: kind, name, body, eval-when, source-text)
- `read-toplevel-form-from-string`, `write-toplevel-form-to-string`,
  `normalize-toplevel-form`
- `atelier:lint-string` in core atelier
- `testsuite/editor/` module with 104 assertions
- 15 auto-discovered fixtures (45 assertions)
- Eclector custom client preserving `#+`/`#-`
- Technical decision: "source text is the truth, CST is for analysis"

Key decisions from Phase 1 that Phase 2 inherits:
- INV-17: editor loads without MCP dependencies
- INV-18: body is Eclector CST preserving reader conditionals
- INV-19: `normalize-toplevel-form` is idempotent
- INV-20: round-trip fixed point

## Project Knowledge Applied

| Source | Entry | How it shaped this plan |
|---|---|---|
| `patterns.md` | *Hallucinated requirements* | Every exported symbol traces to an AC. 8 new tools, each maps to a story. No speculative introspection API. |
| `patterns.md` | *Skim-then-code* | Maker reads SWANK wire protocol reference and MEMORY.md before step 1. |
| `patterns.md` | *Pass-count predictions* | Range 40–80 assertions (calibrated from Phase 1's 104 ÷ 2 for fewer fast tests, more slow). |
| `patterns.md` | *Amendment protocol* | Deviations from S4's "swank-socket-connection" name appended as amendment, not rewritten. |
| `invariants.md` | INV-4 (fresh SBCL subprocess) | Final regression in fresh subprocess. |
| `invariants.md` | INV-12 (MIME-type driven returns) | All 8 new tools return Lisp data; dispatcher encodes to JSON. |
| `invariants.md` | INV-13 (image-connection signatures stable) | `connection-eval`, `connection-shutdown`, `connection-alive-p` unchanged. New methods only. |
| `invariants.md` | INV-14 (every define-tool traces to AC) | Reviewer audit at closure: 8 tools, each to a story. |
| `invariants.md` | INV-16 (with-isolated-registries) | All tests registering tools use `with-isolated-registries`. |
| `calibration.md` | Counting assertions not testcases | Estimate: 40–80 assertions. Slow tests dominate. |
| `reworks.md` | Slice 009 rework #7 (subprocess --eval) | Child spawn uses multiple --eval forms, not one compound form. |

## Risk Register

| # | Risk | Category | Severity | Mitigation |
|---|---|---|:---:|---|
| R1 | SWANK wire protocol version skew | Dependency | HIGH | Pin SWANK version loaded via `(require :swank)`. Child-worker startup logs SWANK version. Test against SLIME v2.32 (Quicklisp current). |
| R2 | Orphan SBCL processes after test failures | Lifecycle | HIGH | All tests use `unwind-protect` → `connection-shutdown`. Orphan-check test counts SBCLs before/after. |
| R3 | TCP port conflict on localhost | Infrastructure | MEDIUM | Port 0 (OS-assigned). Child prints port to stdout; parent reads it. No hardcoded port. |
| R4 | `:emacs-rex` message format drift across SWANK versions | Dependency | MEDIUM | Minimal client: only `:emacs-rex`, `:return`, `:write-string`, `:ping`, `:debug`. No version-specific features. |
| R5 | Child startup race: parent connects before SWANK ready | Lifecycle | MEDIUM | Child prints port only after `create-server` returns. Parent reads port, then connects. Retry with backoff if connection refused. |
| R6 | `eval-form` output corrupts SWANK protocol stream | Protocol | LOW | SWANK captures output via `:write-string` messages. Protocol stream is separate from eval output. |
| R7 | `run-tests-fresh` takes >30s, parent timeout | Performance | MEDIUM | Configurable timeout (default 120s for test runner). `run-tests-fresh` is synchronous `uiop:run-program`, not SWANK. |
| R8 | Hallucinated requirements (from patterns.md) | Architecture | HIGH | Every tool traces to a story. No speculative API. |
| R9 | SWANK `:debug` events arrive during eval | Protocol | MEDIUM | Slice 010 auto-aborts debugger: send `(:emacs-rex (swank:throw-to-toplevel 0) ...)`. Eval returns error. Slice 011 adds interactive debugging. |

## OSS Components

| Component | Version | License | Rationale |
|---|---|---|---|
| `usocket` | latest Quicklisp | MIT | TCP client for SWANK connection. Portable across implementations. |
| `closer-mop` | latest Quicklisp | MIT | MOP introspection in child-worker. Part of the de facto standard. |
| SWANK | bundled with SLIME v2.32 | Public domain | SWANK server in child. Loaded via `(require :swank)`. |

**Not used:** `swank-client` (GPL-2.0, incompatible with MIT). We write our own minimal client (~200 LOC).

## Phase Scope

**Stories delivered in this phase:** S4, S5, S6, S7, S8 from `slice.md`.

**Stories delivered in Phase 1:** S0, S1, S2, S3, S9, S10, S11.

**Story-to-deliverable mapping:**

| Story | Deliverable | API name |
|---|---|---|
| S4 | `child-connection` subclass + SWANK protocol client | `child-connection`, `make-child-connection` |
| S5 | `eval-form` MCP tool + session child lifecycle | `eval-form` tool |
| S6 | 4 introspection MCP tools | `list-packages`, `list-package-symbols`, `describe-symbol`, `find-definition` tools |
| S7 | 2 testsuite runner MCP tools | `run-tests-fresh`, `run-tests-in-child` tools |
| S8 | `canonicalize-form` MCP adapter tool | `canonicalize-form` tool |

**Note on S4 naming deviation:** `slice.md` says `swank-socket-connection`.
Phase 2 names it `child-connection` because: (a) the class represents
a child SBCL process, not a socket; (b) the SWANK protocol is an
implementation detail of the connection, not its identity; (c) the
transport is TCP, not a socketpair. Recorded as deviation from S4-AC1.
Acceptance criterion adjusted: `(find-class 'atelier/mcp:child-connection)`.

## File Organisation

```
src/child-worker/                       [new]
  package.lisp                          — defpackage #:atelier/child-worker
  introspection.lisp                    — helper functions for package/symbol inspection
  entry-point.lisp                      — start SWANK on random port, print port, block

src/mcp/
  package.lisp                          [modify] — add ~15 new exports
  conditions.lisp                       [modify] — add child-image-spawn-failed
  swank-protocol.lisp                   [new] — minimal SWANK wire protocol client
  child-connection.lisp                 [modify] — child-connection subclass of image-connection
  server.lisp                           [modify] — add child-connection slot, session cleanup
  tools/
    canonicalize-form.lisp              [new] — S8
    eval-form.lisp                      [new] — S5
    list-packages.lisp                  [new] — S6
    list-package-symbols.lisp           [new] — S6
    describe-symbol.lisp                [new] — S6
    find-definition.lisp                [new] — S6
    run-tests-fresh.lisp                [new] — S7
    run-tests-in-child.lisp             [new] — S7

org.melusina.atelier.asd                [modify]
  — add org.melusina.atelier/child-worker system
  — add usocket + editor deps to /mcp
  — add new source files to /mcp

testsuite/mcp/
  package.lisp                          [modify] — add test symbols
  swank-protocol.lisp                   [new] — fast: message encoding/decoding
  canonicalize-tool.lisp                [new] — fast: canonicalize-form tool in-process
  child-tests.lisp                      [new] — slow: child connection + eval + introspection + runner
  entrypoint.lisp                       [modify] — wire new tests into MCP test suite

CLAUDE.md                               [modify] — update MCP section with child-worker and tools
```

## Build System Changes

### New system: `org.melusina.atelier/child-worker`

```lisp
(asdf:defsystem #:org.melusina.atelier/child-worker
  :description "Child SBCL worker for Atelier MCP eval."
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:closer-mop)
  :components
  ((:module "src/child-worker"
    :serial t
    :components ((:file "package")
                 (:file "introspection")
                 (:file "entry-point")))))
```

Note: SWANK is loaded at runtime via `(require :swank)` inside the entry
point, not as an ASDF dependency. SWANK ships with SBCL/Quicklisp and is
not a Quicklisp system in the usual sense.

### Modified system: `org.melusina.atelier/mcp`

```lisp
:depends-on (#:alexandria
             #:bordeaux-threads
             #:uiop
             #:usocket
             #:com.inuoe.jzon
             #:org.melusina.atelier
             #:org.melusina.atelier/editor)
```

New source files added to the `src/mcp` module (serial):
```lisp
(:file "swank-protocol")    ;; after image-connection, before dispatcher
```

New tool files added to the `tools` sub-module:
```lisp
(:file "canonicalize-form")
(:file "eval-form")
(:file "list-packages")
(:file "list-package-symbols")
(:file "describe-symbol")
(:file "find-definition")
(:file "run-tests-fresh")
(:file "run-tests-in-child")
```

### Modified system: `org.melusina.atelier/testsuite`

New test files added to the `mcp` module:
```lisp
(:file "swank-protocol")      ;; after utilities, before tool-invocation
(:file "canonicalize-tool")   ;; after tool-invocation
(:file "child-tests")         ;; after canonicalize-tool, before entrypoint
```

Testsuite `:depends-on` gains `#:org.melusina.atelier/child-worker`.

## Package / Module Architecture

### `#:atelier/child-worker` exports (~8 symbols)

```
;; Entry point
start-worker              ;; start SWANK, print port, block

;; Introspection helpers (called via SWANK eval from parent)
list-packages-data        ;; → list of package-descriptor alists
list-package-symbols-data ;; (package-name &key status) → list of symbol-descriptor alists
describe-symbol-data      ;; (designator) → symbol-info alist
find-definition-data      ;; (designator) → source-location alist or NIL
run-testsuite-data        ;; (system-name) → test-result alist
```

### `#:atelier/mcp` new exports (~15 symbols)

```
;; Child connection class
child-connection
make-child-connection
child-connection-port          ;; reader: TCP port child SWANK listens on

;; SWANK protocol (low-level, for testing)
swank-connect                  ;; (host port) → swank-connection
swank-disconnect               ;; (connection)
swank-send                     ;; (connection sexp)
swank-receive                  ;; (connection) → sexp
swank-eval                     ;; (connection form-string &key package timeout) → result

;; Condition
child-image-spawn-failed

;; Session child access (for tools)
server-child-connection        ;; reader on mcp-server
ensure-child-connection        ;; lazy spawn; returns child-connection
```

### `#:atelier/testsuite/mcp` additions

Test package gains internal symbols for new testcases. No new exports.

## Type / Class Hierarchy

```
image-connection (slice 009)
  └─ child-connection [new]
       id              ;; inherited
       process-info    ;; inherited; populated by uiop:launch-program
       port            ;; TCP port the child SWANK listens on
       swank-conn      ;; usocket connection to child SWANK
       next-id         ;; integer counter for :emacs-rex continuation IDs

swank-connection [new, internal]
       usocket         ;; usocket:stream-usocket
       host            ;; string
       port            ;; integer
```

### Conditions

| Name | Superclass | Slots | When signalled |
|---|---|---|---|
| `child-image-spawn-failed` | `mcp-error` | `reason` (string) | `make-child-connection` when SBCL not on PATH, or child fails to start, or port not received within timeout |

## Protocol Definitions

### SWANK Wire Protocol (parent-side client)

```lisp
(defun swank-connect (host port)
  "Open a TCP connection to a SWANK server at HOST:PORT.
   Returns a SWANK-CONNECTION. Signals error on failure."
  ...)

(defun swank-disconnect (connection)
  "Close the TCP connection to the SWANK server."
  ...)

(defun swank-send (connection sexp)
  "Send SEXP to the SWANK server. Wire format: 6-char hex length
   prefix followed by UTF-8 s-expression payload."
  ...)

(defun swank-receive (connection)
  "Read one message from the SWANK server. Returns the s-expression.
   Blocks until a complete message is available."
  ...)

(defun swank-eval (connection form-string
                   &key (package "CL-USER") (timeout 30))
  "Evaluate FORM-STRING in the SWANK server. Returns
   (VALUES result-string output-string). Handles :write-string
   messages for output capture, :ping for keepalive, and :debug
   for debugger events (auto-abort in slice 010). Signals error
   on timeout or evaluation abort."
  ...)
```

### Child Connection Lifecycle

```lisp
(defun make-child-connection (&key (timeout 10))
  "Spawn a child SBCL, load the child-worker system, start SWANK,
   connect via TCP. Returns a CHILD-CONNECTION. The child is
   spawned via uiop:launch-program. TIMEOUT is seconds to wait
   for the child to print its port number.
   Signals CHILD-IMAGE-SPAWN-FAILED on failure."
  ...)

(defmethod connection-eval ((conn child-connection) form)
  "Evaluate FORM (a string) in the child via the SWANK connection.
   Returns (VALUES result-string output-string).
   Captures stdout via :write-string messages."
  ...)

(defmethod connection-shutdown ((conn child-connection))
  "Shut down the child SBCL: disconnect SWANK, terminate process,
   wait for exit. Sets connection-alive-p to NIL."
  ...)

(defmethod connection-alive-p ((conn child-connection))
  "Return T if the child process is running and the SWANK socket
   is connected."
  ...)
```

### Session Child Lifecycle

```lisp
(defun ensure-child-connection (server)
  "Return the session's child-connection, creating one lazily on
   first call. If the existing child is dead, spawn a fresh one
   and log a child-restarted notice."
  ...)
```

### Child-Worker Entry Point

```lisp
(defun start-worker ()
  "Entry point for the child SBCL image.
   1. Load SWANK via (require :swank).
   2. Configure SWANK (disable indentation cache, set coding system).
   3. Start SWANK server on port 0 (OS-assigned).
   4. Print the assigned port as a single line to *standard-output*.
   5. Block forever (sleep loop); SWANK runs in background threads."
  ...)
```

### Child-Worker Introspection Helpers

```lisp
(defun list-packages-data ()
  "Return a list of alists, one per package:
   ((\"name\" . \"COMMON-LISP\") (\"nicknames\" . (\"CL\"))
    (\"use-list\" . (...)) (\"used-by-list\" . (...))
    (\"external-count\" . 978) (\"internal-count\" . 0))"
  ...)

(defun list-package-symbols-data (package-name &key (status :external))
  "Return a list of alists, one per symbol in PACKAGE-NAME matching STATUS.
   Each alist: name, package, status, home-package, kind, documentation."
  ...)

(defun describe-symbol-data (designator)
  "Return an alist describing the symbol named by DESIGNATOR (a string
   like \"cl:car\" or \"alexandria:flatten\"). Includes: function-lambda-list,
   function-type, class-slots, generic-methods, documentation, home-package.
   Uses closer-mop for class/generic introspection, sb-introspect for
   function signatures."
  ...)

(defun find-definition-data (designator)
  "Return an alist with source-file, line, column for the definition
   of the symbol named by DESIGNATOR. Uses sb-introspect. Returns NIL
   if no source location is available."
  ...)

(defun run-testsuite-data (system-name)
  "Load SYSTEM-NAME via ASDF, run its test system, capture output.
   Return an alist: system, passed, failed, errored, skipped,
   duration-ms, output."
  ...)
```

## Error / Condition Types

| Name | Superclass | Slots | When signalled |
|---|---|---|---|
| `child-image-spawn-failed` | `mcp-error` | `reason` (string) | `make-child-connection` fails: SBCL not on PATH, startup timeout, port not received |

## Test Plan

| Story | Test name | Category | Skip condition |
|---|---|---|---|
| — | `validate-swank-message-encoding` (T43) | fast | — |
| — | `validate-swank-message-decoding` (T44) | fast | — |
| S8 | `validate-canonicalize-tool-basic` (T45) | fast | — |
| S8 | `validate-canonicalize-tool-earmuffs` (T46) | fast | — |
| S8 | `validate-canonicalize-tool-forbidden` (T47) | fast | — |
| S8 | `validate-canonicalize-tool-progn-decompose` (T48) | fast | — |
| S8 | `validate-canonicalize-tool-no-child` (T49) | fast | — |
| S4 | `validate-child-connection-spawn` (T50) | slow | `#-sbcl` |
| S4 | `validate-child-connection-eval` (T51) | slow | `#-sbcl` |
| S4 | `validate-child-connection-sequential-state` (T52) | slow | `#-sbcl` |
| S4 | `validate-child-connection-shutdown` (T53) | slow | `#-sbcl` |
| S4 | `validate-child-connection-spawn-failure` (T54) | fast | — |
| S5 | `validate-eval-form-basic` (T55) | slow | `#-sbcl` |
| S5 | `validate-eval-form-multiple-values` (T56) | slow | `#-sbcl` |
| S5 | `validate-eval-form-error` (T57) | slow | `#-sbcl` |
| S5 | `validate-eval-form-output-capture` (T58) | slow | `#-sbcl` |
| S5 | `validate-eval-form-timeout` (T59) | slow | `#-sbcl` |
| S5 | `validate-eval-form-child-restart` (T60) | slow | `#-sbcl` |
| S6 | `validate-list-packages` (T61) | slow | `#-sbcl` |
| S6 | `validate-list-package-symbols` (T62) | slow | `#-sbcl` |
| S6 | `validate-describe-symbol` (T63) | slow | `#-sbcl` |
| S6 | `validate-find-definition` (T64) | slow | `#-sbcl` |
| S6 | `validate-introspection-missing-symbol` (T65) | slow | `#-sbcl` |
| S7 | `validate-run-tests-fresh` (T66) | slow | `#-sbcl` |
| S7 | `validate-run-tests-in-child` (T67) | slow | `#-sbcl` |
| S7 | `validate-run-tests-fresh-load-failure` (T68) | slow | `#-sbcl` |
| — | `validate-tool-registration-count` (T69) | fast | — |
| — | `validate-no-orphan-sbcl` (T70) | slow | `#-sbcl` |
| S9 | `validate-full-regression-fresh-sbcl` (T42, existing) | slow | `#-sbcl` |

**Assertion count estimate:** 40–80 new assertions (range, per calibration).
Slow tests dominate. Phase 2 has fewer but heavier tests than Phase 1.

**Shared child optimization:** Slow tests T50–T67 share a single child
SBCL connection spawned once in `run-child-dependent-tests`. The child
is shut down in an `unwind-protect` after all child-dependent tests
complete. This avoids spawning ~15 separate SBCLs.

## Implementation Order (Step Table)

| Step | File | Action | Form(s) | Test name(s) | Category |
|---|---|---|---|---|:---:|
| 0 | _Maker protocol_ | Re-read `product/knowledge/` files, SWANK wire protocol docs, MEMORY.md, Phase 1 notes | — | — | — |
| 1 | `references/swank-protocol.md` [new] | Document SWANK wire protocol: message framing, `:emacs-rex`/`:return` cycle, `:write-string`, `:ping`/`:pong`, `:debug` auto-abort strategy | — | — | — |
| 2 | `references/child-worker-contract.md` [new] | Document child-worker system contract: startup sequence, port reporting, introspection API shapes, shutdown protocol | — | — | — |
| 3 | `src/child-worker/package.lisp` [new] | Define `#:atelier/child-worker` with ~8 exports | `defpackage` | — | — |
| 4 | `src/child-worker/introspection.lisp` [new] | Implement `list-packages-data`, `list-package-symbols-data`, `describe-symbol-data`, `find-definition-data`, `run-testsuite-data` | 5 functions | — | — |
| 5 | `src/child-worker/entry-point.lisp` [new] | Implement `start-worker`: load SWANK, configure, create-server port 0, print port, block | `start-worker` | — | — |
| 6 | `org.melusina.atelier.asd` [modify] | Add `org.melusina.atelier/child-worker` system definition | `defsystem` | — | — |
| 7 | `src/mcp/package.lisp` [modify] | Add ~15 new exports to `#:atelier/mcp` | exports | — | — |
| 8 | `src/mcp/conditions.lisp` [modify] | Add `child-image-spawn-failed` condition | `define-condition` | — | — |
| 9 | `src/mcp/swank-protocol.lisp` [new] | Implement SWANK wire protocol client: `swank-connect`, `swank-disconnect`, `swank-send`, `swank-receive`, `swank-eval` | 5 functions + `swank-connection` class | — | — |
| 10 | `src/mcp/child-connection.lisp` [modify] | Implement `child-connection` subclass: `make-child-connection`, `connection-eval`, `connection-shutdown`, `connection-alive-p` methods | class + 4 methods | — | — |
| 11 | `src/mcp/server.lisp` [modify] | Add `child-connection` slot to `mcp-server`, implement `ensure-child-connection`, add session cleanup in `%serve-loop` | 2 functions + slot | — | — |
| 12 | `org.melusina.atelier.asd` [modify] | Add `usocket` + `editor` deps to `/mcp`, add `swank-protocol.lisp` and 8 tool files to components, add `child-worker` dep to testsuite | dependency + component entries | — | — |
| 13 | `src/mcp/tools/canonicalize-form.lisp` [new] | Implement `canonicalize-form` MCP tool (S8) | `define-tool canonicalize-form` | — | — |
| 14 | `src/mcp/tools/eval-form.lisp` [new] | Implement `eval-form` MCP tool (S5) | `define-tool eval-form` | — | — |
| 15 | `src/mcp/tools/list-packages.lisp` [new] | Implement `list-packages` MCP tool (S6) | `define-tool list-packages` | — | — |
| 16 | `src/mcp/tools/list-package-symbols.lisp` [new] | Implement `list-package-symbols` MCP tool (S6) | `define-tool list-package-symbols` | — | — |
| 17 | `src/mcp/tools/describe-symbol.lisp` [new] | Implement `describe-symbol` MCP tool (S6) | `define-tool describe-symbol` | — | — |
| 18 | `src/mcp/tools/find-definition.lisp` [new] | Implement `find-definition` MCP tool (S6) | `define-tool find-definition` | — | — |
| 19 | `src/mcp/tools/run-tests-fresh.lisp` [new] | Implement `run-tests-fresh` MCP tool (S7) — uses `uiop:run-program`, not SWANK | `define-tool run-tests-fresh` | — | — |
| 20 | `src/mcp/tools/run-tests-in-child.lisp` [new] | Implement `run-tests-in-child` MCP tool (S7) — uses session child via SWANK | `define-tool run-tests-in-child` | — | — |
| 21 | `testsuite/mcp/package.lisp` [modify] | Add test symbols for new testcases | package update | — | — |
| 22 | `testsuite/mcp/swank-protocol.lisp` [new] | Test SWANK message encoding/decoding (no child needed) | `validate-swank-message-encoding`, `validate-swank-message-decoding` | T43, T44 | fast |
| 23 | `testsuite/mcp/canonicalize-tool.lisp` [new] | Test canonicalize-form tool in-process (no child) | `validate-canonicalize-tool-basic`, `-earmuffs`, `-forbidden`, `-progn-decompose`, `-no-child` | T45–T49 | fast |
| 24 | `testsuite/mcp/child-tests.lisp` [new] | Test child-connection spawn | `validate-child-connection-spawn` | T50 | slow |
| 25 | `testsuite/mcp/child-tests.lisp` [modify] | Test child-connection eval | `validate-child-connection-eval` | T51 | slow |
| 26 | `testsuite/mcp/child-tests.lisp` [modify] | Test child-connection sequential state | `validate-child-connection-sequential-state` | T52 | slow |
| 27 | `testsuite/mcp/child-tests.lisp` [modify] | Test child-connection shutdown | `validate-child-connection-shutdown` | T53 | slow |
| 28 | `testsuite/mcp/child-tests.lisp` [modify] | Test spawn failure (fast — no real child) | `validate-child-connection-spawn-failure` | T54 | fast |
| 29 | `testsuite/mcp/child-tests.lisp` [modify] | Test eval-form tool basic | `validate-eval-form-basic` | T55 | slow |
| 30 | `testsuite/mcp/child-tests.lisp` [modify] | Test eval-form multiple values | `validate-eval-form-multiple-values` | T56 | slow |
| 31 | `testsuite/mcp/child-tests.lisp` [modify] | Test eval-form error handling | `validate-eval-form-error` | T57 | slow |
| 32 | `testsuite/mcp/child-tests.lisp` [modify] | Test eval-form output capture | `validate-eval-form-output-capture` | T58 | slow |
| 33 | `testsuite/mcp/child-tests.lisp` [modify] | Test eval-form timeout | `validate-eval-form-timeout` | T59 | slow |
| 34 | `testsuite/mcp/child-tests.lisp` [modify] | Test eval-form child restart after death | `validate-eval-form-child-restart` | T60 | slow |
| 35 | `testsuite/mcp/child-tests.lisp` [modify] | Test list-packages tool | `validate-list-packages` | T61 | slow |
| 36 | `testsuite/mcp/child-tests.lisp` [modify] | Test list-package-symbols tool | `validate-list-package-symbols` | T62 | slow |
| 37 | `testsuite/mcp/child-tests.lisp` [modify] | Test describe-symbol tool | `validate-describe-symbol` | T63 | slow |
| 38 | `testsuite/mcp/child-tests.lisp` [modify] | Test find-definition tool | `validate-find-definition` | T64 | slow |
| 39 | `testsuite/mcp/child-tests.lisp` [modify] | Test introspection missing symbol error | `validate-introspection-missing-symbol` | T65 | slow |
| 40 | `testsuite/mcp/child-tests.lisp` [modify] | Test run-tests-fresh (separate child) | `validate-run-tests-fresh` | T66 | slow |
| 41 | `testsuite/mcp/child-tests.lisp` [modify] | Test run-tests-in-child (session child) | `validate-run-tests-in-child` | T67 | slow |
| 42 | `testsuite/mcp/child-tests.lisp` [modify] | Test run-tests-fresh load failure | `validate-run-tests-fresh-load-failure` | T68 | slow |
| 43 | `testsuite/mcp/child-tests.lisp` [modify] | Test tool registration count (14 total) | `validate-tool-registration-count` | T69 | fast |
| 44 | `testsuite/mcp/child-tests.lisp` [modify] | Test no orphan SBCL processes | `validate-no-orphan-sbcl` | T70 | slow |
| 45 | `testsuite/mcp/entrypoint.lisp` [modify] | Wire new tests into MCP test suite entry point | — | — | — |
| 46 | `CLAUDE.md` [modify] | Update MCP section: child-worker system, 8 new tools, SWANK client, tool count update | — | — | — |
| 47 | _Full regression_ | Run `(asdf:test-system "org.melusina.atelier")` in fresh SBCL subprocess | T42 | — | slow |
| 48 | _Phase-closure checks_ | INV-11 grep; INV-14 tool-to-AC audit; export audit; no orphan SBCLs; assertion count within 40–80 | — | — | — |

## Invariants

**Carried forward (all confirmed):** INV-1 through INV-20.

**New for Phase 2:**

- **INV-21:** `child-connection` spawns a child SBCL via `uiop:launch-program`, connects via SWANK over TCP on localhost, and shuts down cleanly via `connection-shutdown`. No orphan SBCL processes after shutdown. Enforced by T50–T53, T70.
- **INV-22:** The `canonicalize-form` MCP tool runs entirely in the parent image. No child connection is created or used. Enforced by T49.
- **INV-23:** `eval-form` returns both the evaluation result and any captured stdout/stderr from the child. Output capture is via SWANK `:write-string` messages. Enforced by T58.
- **INV-24:** SWANK `:debug` events during eval are auto-aborted in slice 010 (invoke `ABORT` restart). The eval returns an error result with the condition text. Interactive debugging is deferred to slice 011. Enforced by T57.
- **INV-25:** `run-tests-fresh` spawns a separate SBCL (not the session child) via `uiop:run-program`. The session child is untouched. Enforced by T66.

## Test Fixtures

No new fixture files. All tests are `define-testcase` assertions.

## References to Create

| Step | File | Contents |
|---|---|---|
| 1 | `references/swank-protocol.md` | SWANK wire protocol: 6-char hex length prefix + UTF-8 s-expression. Message types: `:emacs-rex`, `:return`, `:write-string`, `:ping`/`:emacs-pong`, `:debug`/`:debug-activate`. Auto-abort strategy for debugger in slice 010. Connection sequence: TCP connect, optional secret, first `:emacs-rex`. |
| 2 | `references/child-worker-contract.md` | Child-worker startup contract: (1) `(require :swank)`, (2) configure SWANK, (3) `(swank:create-server :port 0 :dont-close t)`, (4) print port line to stdout, (5) block forever. Introspection API shapes: each function returns an alist or list of alists. Shutdown: parent terminates process via `uiop:terminate-process`. |

## Acceptance Criteria

| # | Criterion | Story | Verified by |
|---|---|:---:|---|
| AC19 | `(find-class 'atelier/mcp:child-connection)` returns a class inheriting from `image-connection` | S4 | T50 |
| AC20 | `make-child-connection` spawns child SBCL within 10 seconds | S4 | T50 |
| AC21 | `connection-eval` returns result of `(+ 1 2)` as `"3"` | S4 | T51 |
| AC22 | Sequential evals share state: `(defvar *x* 1)` then `*x*` returns `"1"` | S4 | T52 |
| AC23 | `connection-shutdown` terminates child; `connection-alive-p` returns NIL | S4 | T53 |
| AC24 | Missing SBCL signals `child-image-spawn-failed` | S4 | T54 |
| AC25 | `eval-form` tool returns JSON with `value`, `stdout`, `stderr`, `duration-ms` | S5 | T55 |
| AC26 | `eval-form` on signalling form returns `isError: true` with condition text | S5 | T57 |
| AC27 | `eval-form` captures stdout from child eval | S5 | T58 |
| AC28 | `eval-form` timeout returns error; child survives for next call | S5 | T59 |
| AC29 | Dead child auto-restarts on next `eval-form` | S5 | T60 |
| AC30 | `list-packages` returns JSON array with name, nicknames, counts | S6 | T61 |
| AC31 | `list-package-symbols` returns symbol descriptors with kind and docs | S6 | T62 |
| AC32 | `describe-symbol` returns function signature, class slots, or generic methods | S6 | T63 |
| AC33 | `find-definition` returns source-file and line for a known symbol | S6 | T64 |
| AC34 | Introspection on non-existent symbol returns `isError: true` | S6 | T65 |
| AC35 | `run-tests-fresh` spawns separate SBCL, runs tests, returns pass/fail counts | S7 | T66 |
| AC36 | `run-tests-in-child` runs tests in session child | S7 | T67 |
| AC37 | `canonicalize-form` tool runs in parent (no child), returns canonicalized + findings | S8 | T45–T46 |
| AC38 | `canonicalize-form` on forbidden form returns `isError: true` | S8 | T47 |
| AC39 | 14 tools registered total (6 from slice 009 + 8 new) | — | T69 |
| AC40 | No orphan SBCL processes after test suite completes | — | T70 |
| AC41 | Full regression (base + editor + MCP) passes in fresh SBCL subprocess | S9 | T42 |

23 acceptance criteria. Each maps to at least one test.

## Phase Closure Conditions

- [ ] All fast tests passing (T43–T49, T54, T69).
- [ ] All slow tests passing or skipped with `#-sbcl` (T50–T53, T55–T68, T70).
- [ ] All 23 acceptance criteria met (AC19–AC41).
- [ ] `CLAUDE.md` updated with child-worker system and 8 new tools.
- [ ] `resource/template/` grep for `atelier/child-worker` and `child-connection` returns zero matches.
- [ ] Every exported symbol from `#:atelier/mcp` (new) traces to an AC (Reviewer audit).
- [ ] Every `define-tool` traces to a story (INV-14).
- [ ] No `src/editor/*.lisp` file modified in Phase 2.
- [ ] No orphan SBCL processes after full test suite (T70).
- [ ] Assertion count within 40–80 range (documented in notes if outside).
- [ ] `references/swank-protocol.md` and `references/child-worker-contract.md` committed.
- [ ] Any plan deviations documented via the append-not-rewrite amendment protocol.
