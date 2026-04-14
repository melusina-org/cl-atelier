# Implementation Phase 1: Slice 015 — MCP Server Reliability

**Phase:** 1
**Slice:** product/slice/015-mcp-reliability/slice.md
**Scope:** All 6 stories (S1–S6) in one phase.
**Prerequisites:** Slice 014 complete (352/352 MCP tests, 40 tools).

---

## Prior Phases

None — this is the first implementation phase of slice 015.

## Project Knowledge Applied

- **Pattern 6 (test-registry pollution):** Shared test child must not leak
  state across groups. Groups that mutate child state clean up in
  unwind-protect. INV-16 (with-isolated-registries) remains unchanged.
- **Pattern 10 (pipe deadlock):** Drain thread stays for child-connection;
  the kernel's abstract image-connection has no I/O.
- **Pattern 11 (SWANK for Emacs ≠ for CL):** Validate broadcast-stream
  capture with exploratory test before committing to design.
- **INV-38 (connection-alive-p):** Addressed by S1 proactive health check.
- **INV-39 (UIOP handles both streams):** Informs S2 child spawn approach.
- **INV-40 (trace-output not captured):** Addressed by S2 separate capture.
- **Calibration:** Pure-tools slices produce ~2–5 assertions per tool.
  This slice is structural (kernel split, test restructure) with some
  tool changes — expect higher assertion density in fast tests, lower
  in slow tests.

## Risk Register

| # | Risk | Category | Mitigation |
|---|------|----------|-----------|
| R1 | SWANK ping in `connection-alive-p` adds latency to every tool call | Performance | Measure in test; accept if <5ms. `ensure-child-connection` is the only caller. |
| R2 | Kernel/mcp package split breaks `atelier/mcp:` qualified references | Scope boundary | `atelier/mcp` `:use`s `atelier/mcp-kernel`; tool files keep `(in-package #:atelier/mcp)`. |
| R3 | `make-instance 'child-connection` initialization is complex | CL surprise | Test spawn failure explicitly. Clean unwind-protect in `initialize-instance`. |
| R4 | Shared test child accumulates state across 7 groups | Test pollution | Mutating groups clean up. Assert debug-state nil at group entry. |
| R5 | `run-tests-fresh` switching to `run-all-tests` takes minutes | Test speed | Correct behavior — accept cost. Already has duration-ms. |
| R6 | Broadcast-stream capture across SWANK boundary needs round-trip drain | SWANK protocol | Validate A4 with exploratory test first. Fall back to per-eval binding if needed. |
| R7 | Kernel must load without SWANK/usocket/flexi-streams dependencies | Dependency | Abstract `image-connection` uses only UIOP. `child-connection` with SWANK deps stays in mcp. |

## OSS Components

None — all work uses existing dependencies.

## Phase Scope

**Stories in scope:** S1, S2, S3, S4, S5, S6 (all).

**S6 finding:** The FILE-EXISTS crash was already fixed in slice 014
(commit e2650fb, `:if-exists :supersede`). What remains is a regression
test and fixing `run-tests-fresh` to actually run tests (it currently
calls `asdf:test-system` which does nothing — no `perform` method on
`test-op` is defined).

## File Organisation

No new directories. Both `mcp-kernel` and `mcp` systems reference files
in `src/mcp/`. The kernel system lists the kernel subset; the mcp system
lists everything else and depends on kernel.

```
src/mcp/
├── package.lisp              [modify] — split into kernel + mcp sections
├── conditions.lisp           [kernel]
├── json-util.lisp            [kernel]
├── protocol-version.lisp     [kernel]
├── tool-name.lisp            [kernel]
├── input-schema.lisp         [kernel]
├── uri-template.lisp         [kernel]
├── tool.lisp                 [kernel]
├── message.lisp              [kernel]
├── define-tool.lisp          [kernel]
├── dispatcher.lisp           [kernel]
├── server.lisp               [kernel] — connection-class/initargs pattern
├── image-connection.lisp     [modify] — abstract class stays in kernel;
│                                        child-connection splits to mcp
├── child-connection.lisp     [new]    — child-connection class, SWANK spawn
├── swank-protocol.lisp       [mcp]
├── transcript.lisp           [mcp]
├── transcript-render.lisp    [mcp]
├── hyperspec.lisp            [mcp]
├── tools/reload-server.lisp  [kernel]
├── tools/eval-form.lisp      [modify] — add trace-output, stderr fields
├── tools/run-tests-fresh.lisp [modify] — fix to call run-all-tests
└── tools/*.lisp              [mcp]

src/child-worker/
├── entry-point.lisp          [modify] — broadcast streams for capture
└── introspection.lisp        [modify] — drain-capture-streams helper

testsuite/mcp/
├── entrypoint.lisp           [modify] — shared child wrapping
├── child-tests.lisp          [modify] — remove per-group *test-child* rebind
├── debugger-tests.lisp       [modify] — remove per-group *test-child* rebind
├── asdf-tools-tests.lisp     [modify] — remove per-group *test-child* rebind
├── documentation-tools-tests.lisp [modify] — ditto
├── xref-tools-tests.lisp     [modify] — ditto
├── inspect-trace-tests.lisp  [modify] — ditto, add trace cleanup
├── journey-tests.lisp        [modify] — ditto
├── health-check-tests.lisp   [new]   — S1 tests
├── output-capture-tests.lisp [new]   — S2 tests
├── fresh-sbcl-load.lisp      [modify] — kernel load test
└── registry-counts.lisp      [modify] — tool count unchanged (41)

testsuite/template.lisp       [modify] — S6 regression test
```

## Build System Changes

```
org.melusina.atelier/mcp-kernel   [NEW]
  :depends-on (alexandria uiop com.inuoe.jzon)
  :components — package (kernel section), conditions, json-util,
    protocol-version, tool-name, input-schema, uri-template, tool,
    message, define-tool, dispatcher, image-connection, server,
    tools/reload-server

org.melusina.atelier/mcp          [MODIFY]
  :depends-on — ADD org.melusina.atelier/mcp-kernel
                KEEP bordeaux-threads usocket flexi-streams
                KEEP org.melusina.atelier org.melusina.atelier/editor
                DROP com.inuoe.jzon (now via kernel)
  :components — package (mcp section), child-connection [NEW],
    swank-protocol, transcript, transcript-render, hyperspec,
    tools/* (except reload-server)

org.melusina.atelier/mcp-server   [MODIFY]
  :depends-on — org.melusina.atelier/mcp (unchanged behavior)
```

## Package / Module Architecture

### atelier/mcp-kernel (new package)

Exports grouped by concept:

**Protocol:**
`+mcp-protocol-version+`

**Conditions:**
`mcp-error`, `tool-not-found`, `resource-not-found`, `not-implemented`,
`invalid-tool-arguments`, `invalid-uri-template`

**JSON utilities:**
`encode-to-string`, `decode-from-string`, `make-json-object`,
`+json-null+`, `+json-false+`, `json-object-p`

**Tool infrastructure:**
`derive-tool-name`, `derive-input-schema`, `tool`, `resource-tool`,
`tool-name`, `tool-description`, `tool-input-schema`,
`resource-uri-template`, `resource-name`, `resource-mime-type`,
`*tool-registry*`, `*concrete-resource-registry*`,
`*template-resource-registry*`, `find-tool-by-name`,
`handle-tool-call`, `define-tool`

**URI templates:**
`parse-uri-template`, `match-uri-template`, `validate-uri-template`

**Messages:**
`mcp-request`, `mcp-notification`, `mcp-success-response`,
`mcp-error-response`, `request-id`, `request-method`, `request-params`,
`response-result`, `error-code`, `error-message`, `error-data`,
`parse-mcp-message`, `handle-message`

**Image connection (abstract):**
`image-connection`, `connection-id`, `connection-process-info`,
`connection-alive-p`, `connection-shutdown`, `connection-eval`

**Server:**
`mcp-server`, `server-stream`, `server-child-connection`,
`server-connection-class`, `server-connection-initargs`,
`serve-two-way-stream`, `ensure-child-connection`, `*current-server*`

### atelier/mcp (modified package)

`:use`s `atelier/mcp-kernel`. Re-exports all kernel symbols.
Adds transport-specific exports:

**SWANK protocol:**
`swank-connection`, `swank-connect`, `swank-disconnect`, `swank-eval`,
`swank-send-raw`, `swank-invoke-restart`, `swank-backtrace-frames`,
`swank-eval-in-frame`, `swank-interrupt`

**Child connection:**
`child-connection`, `child-connection-port`, `child-connection-swank-conn`,
`connection-debug-state`, `debug-state`, `debug-state-condition`,
`debug-state-restarts`, `debug-state-backtrace`, `debug-state-level`,
`debug-state-thread`, `child-image-spawn-failed`

**Transcript:**
`transcript`, `make-transcript`, `write-transcript-entry`,
`read-transcript-entries`

## Type / Class Hierarchy

```
image-connection               [kernel — abstract]
├── connection-id
├── connection-process-info
└── child-connection           [mcp — concrete]
    ├── port
    ├── swank-conn
    ├── stdout-drain-thread
    └── debug-state

mcp-server                     [kernel]
├── stream
├── connection-class           [NEW — defaults to 'image-connection]
├── connection-initargs        [NEW — defaults to nil]
├── output-mutex
├── transcript                 [MOVE to mcp subclass or mixin? No — keep]
└── child-connection
```

Note: `transcript` slot stays on `mcp-server` in kernel even though
transcript implementation is in mcp. The slot type is `(or transcript null)`
with initform nil. Kernel never writes to it; mcp's `serve-two-way-stream`
populates it. This avoids needing an mcp-server subclass.

## Protocol Definitions

```lisp
;; Kernel — image-connection.lisp
(defgeneric connection-alive-p (connection)
  "Return T if CONNECTION's image is currently running.")
  ;; Default: (uiop:process-alive-p process-info)

(defgeneric connection-shutdown (connection)
  "Gracefully shut down CONNECTION's image.")

(defgeneric connection-eval (connection form)
  "Evaluate FORM in CONNECTION's image. Return result as string.")

;; Kernel — server.lisp
(defun ensure-child-connection (server)
  "Return the session's connection, creating via CONNECTION-CLASS
   and CONNECTION-INITARGS if needed. Respawns if dead.")

;; MCP — child-connection.lisp
;; connection-alive-p :around method adds SWANK probe
;; initialize-instance :after handles spawn, port read, SWANK connect
```

## Error / Condition Types

No new condition types. Existing `child-image-spawn-failed` and
`mcp-error` suffice.

## Test Plan

| Story | Test | Category | Skip |
|-------|------|:--------:|------|
| S1 | `validate-health-check-alive` | slow | `#-sbcl` |
| S1 | `validate-health-check-dead-respawn` | slow | `#-sbcl` |
| S2 | `validate-eval-trace-capture` | slow | `#-sbcl` |
| S2 | `validate-eval-stderr-capture` | slow | `#-sbcl` |
| S4 | `validate-mcp-kernel-loads-independently` | slow | `#-sbcl` |
| S5 | `validate-run-tests-fresh-actually-runs` | slow | `#-sbcl` |
| S5 | `validate-run-tests-fresh-targeted` | slow | `#-sbcl` |
| S6 | `validate-template-idempotent-write` | fast | — |
| S3 | (structural — existing 352+ tests pass under shared child) | — | — |

## Implementation Order (Step Table)

| Step | File | Action | Form(s) | Test | Cat |
|------|------|--------|---------|------|:---:|
| 1 | `testsuite/template.lisp` | modify | `validate-template-idempotent-write` | `validate-template-idempotent-write` | fast |
| 2 | `testsuite/mcp/entrypoint.lisp` | modify | `run-mcp-tests` — wrap child-dependent groups in shared `*test-child*` | existing 352+ tests | slow |
| 3 | `testsuite/mcp/child-tests.lisp` | modify | `run-child-dependent-tests` — remove `(let ((*test-child* nil)) ...)`, keep body | existing tests | slow |
| 4 | `testsuite/mcp/debugger-tests.lisp` | modify | `run-debugger-tests` — remove per-group rebind | existing tests | slow |
| 5 | `testsuite/mcp/asdf-tools-tests.lisp` | modify | `run-asdf-tools-tests` — remove per-group rebind | existing tests | slow |
| 6 | `testsuite/mcp/documentation-tools-tests.lisp` | modify | `run-documentation-tools-tests` — remove per-group rebind | existing tests | slow |
| 7 | `testsuite/mcp/xref-tools-tests.lisp` | modify | `run-xref-tools-tests` — remove per-group rebind | existing tests | slow |
| 8 | `testsuite/mcp/inspect-trace-tests.lisp` | modify | `run-inspect-trace-tests` — remove per-group rebind, add trace cleanup | existing tests | slow |
| 9 | `testsuite/mcp/journey-tests.lisp` | modify | `run-journey-tests` — remove per-group rebind | existing tests | slow |
| 10 | `src/mcp/package.lisp` | modify | Split into `defpackage atelier/mcp-kernel` + modified `defpackage atelier/mcp` that `:use`s kernel | — | — |
| 11 | `src/mcp/image-connection.lisp` | modify | Remove `child-connection` class and all child-specific code; keep abstract `image-connection` with 3 generics | — | — |
| 12 | `src/mcp/server.lisp` | modify | Add `connection-class`, `connection-initargs` slots; update `ensure-child-connection` to use `make-instance`; update `serve-two-way-stream` | — | — |
| 13 | `src/mcp/child-connection.lisp` | new | `child-connection` class, `initialize-instance :after`, spawn logic, SWANK connect, drain thread, `connection-eval`, `connection-shutdown`, `connection-alive-p` with SWANK probe | — | — |
| 14 | `org.melusina.atelier.asd` | modify | Add `defsystem org.melusina.atelier/mcp-kernel`; update `/mcp` to depend on kernel; update component lists | — | — |
| 15 | `testsuite/mcp/fresh-sbcl-load.lisp` | modify | `validate-mcp-kernel-loads-independently` — fresh SBCL loads kernel without mcp | `validate-mcp-kernel-loads-independently` | slow |
| 16 | `testsuite/mcp/health-check-tests.lisp` | new | `validate-health-check-alive`, `validate-health-check-dead-respawn` | S1 tests | slow |
| 17 | `src/child-worker/entry-point.lisp` | modify | Add `*trace-capture*`, `*stderr-capture*` broadcast streams; add `drain-capture-streams` helper | — | — |
| 18 | `src/child-worker/introspection.lisp` | modify | Export `drain-capture-streams` | — | — |
| 19 | `src/mcp/tools/eval-form.lisp` | modify | After swank-eval, call drain-capture-streams in child; return `trace-output` and `stderr` fields | — | — |
| 20 | `testsuite/mcp/output-capture-tests.lisp` | new | `validate-eval-trace-capture`, `validate-eval-stderr-capture` | S2 tests | slow |
| 21 | `src/mcp/tools/run-tests-fresh.lisp` | modify | Fix to call `run-all-tests` (or targeted testcase) instead of `asdf:test-system`; add `testcase-designator` parameter | — | — |
| 22 | `testsuite/mcp/child-tests.lisp` | modify | `validate-run-tests-fresh` — assert output contains test results; add `validate-run-tests-fresh-targeted`, `validate-run-tests-fresh-actually-runs` | S5 tests | slow |
| 23 | `testsuite/mcp/entrypoint.lisp` | modify | Wire new test groups into `run-mcp-tests` | all tests | slow |
| 24 | `org.melusina.atelier.asd` | modify | Add new test files to testsuite component list | — | — |

## Invariants

Carried forward from prior slices (INV-1 through INV-40). New:

- **INV-41:** `atelier/mcp-kernel` loads independently without SWANK,
  usocket, flexi-streams, or bordeaux-threads. It depends only on
  alexandria, uiop, and jzon.
- **INV-42:** `connection-alive-p` on `child-connection` probes the SWANK
  socket (round-trip eval of `T` with 1s timeout). Returns NIL on any
  error, never signals.
- **INV-43:** `ensure-child-connection` creates connections via
  `(apply #'make-instance (server-connection-class server) (server-connection-initargs server))`.
  No hardcoded class names in kernel.
- **INV-44:** `eval-form` returns `trace-output` and `stderr` as separate
  fields. Trace output from `(trace ...)` appears in `trace-output`, not
  `stdout`.
- **INV-45:** `run-tests-fresh` calls `run-all-tests` (or a specific
  testcase), not `asdf:test-system`. Output contains actual test results.

## Test Fixtures

- `testsuite/mcp/health-check-tests.lisp` — new file, S1 tests
- `testsuite/mcp/output-capture-tests.lisp` — new file, S2 tests
- No data fixtures needed.

## References to Create

- `product/slice/015-mcp-reliability/references/kernel-boundary.md` —
  which files belong to kernel vs mcp, and the rationale for each.

## Acceptance Criteria

| # | Criterion | Mapped to |
|---|-----------|-----------|
| AC1 | `connection-alive-p` on a healthy child returns T with <5ms overhead | S1 |
| AC2 | `connection-alive-p` on a child with broken SWANK returns NIL | S1 |
| AC3 | After broken SWANK, next `ensure-child-connection` respawns transparently | S1 |
| AC4 | `eval-form` on traced function returns trace output in `trace-output` field | S2 |
| AC5 | `eval-form` on code writing to `*error-output*` returns it in `stderr` field | S2 |
| AC6 | All child-dependent MCP tests pass with one shared child | S3 |
| AC7 | `(asdf:load-system "org.melusina.atelier/mcp-kernel")` loads in fresh SBCL without mcp | S4 |
| AC8 | `reload-server` tool works and lives in mcp-kernel | S4 |
| AC9 | `run-tests-fresh` with testcase-designator runs only that testcase | S5 |
| AC10 | `run-tests-fresh` without designator runs full suite and output contains test results | S5 |
| AC11 | `new-lisp-project` called twice on same directory does not crash | S6 |
| AC12 | Full test suite passes (352+ tests) | All |

## Phase Closure Conditions

- All acceptance criteria AC1–AC15 verified
- Full MCP test suite passes under shared child (S3 structural proof)
- `atelier/mcp-kernel` loads independently in fresh SBCL
- No orphan SBCL processes after test run
- CLAUDE.md updated with new system
- Tool count assertion updated if changed

---

## Amendment 1: Child Process Cap and PID Ownership (2026-04-14)

**Trigger:** During step 1 execution, the MCP test suite spawned hundreds
of SBCL child processes — each child-dependent test group spawned its own
child, and `run-tests-fresh` spawned additional fresh SBCLs. The machine
ran out of resources. The root cause: no cap on concurrent children, no
PID tracking, no timeout kill-and-respawn.

**New requirement from user:**
1. Cap on concurrent child processes (default 3, adjustable)
2. Every child has its PID tracked explicitly
3. When a tool call times out, extract diagnostics + stderr, kill the
   child, and respawn

### Design

**Kernel level (`image-connection.lisp` + `server.lisp`):**

The abstract `image-connection` gains a `connection-pid` generic:
```lisp
(defgeneric connection-pid (connection)
  "Return the OS process ID of CONNECTION's image, or NIL.")
```

The `mcp-server` gains a child process cap:
```lisp
(defclass mcp-server ()
  (...
   (max-children
    :initarg :max-children
    :reader server-max-children
    :initform 3
    :type (integer 1)
    :documentation "Maximum concurrent child connections.")
   (children
    :accessor server-children
    :initform nil
    :type list
    :documentation "List of active IMAGE-CONNECTION instances."))
  ...)
```

`ensure-child-connection` enforces the cap:
```lisp
(defun ensure-child-connection (server)
  "Return the session's primary connection, respawning if dead.
   Enforces SERVER-MAX-CHILDREN cap by shutting down the oldest
   idle connection when at capacity."
  ...)
```

**MCP level (`child-connection.lisp`):**

`child-connection` implements `connection-pid`:
```lisp
(defmethod connection-pid ((conn child-connection))
  (let ((info (connection-process-info conn)))
    (when info (uiop/launch-program:process-info-pid info))))
```

**Timeout kill-and-respawn in `eval-form`:**

When `swank-eval` times out (SWANK interrupt fails or hangs), the tool:
1. Extracts any partial output from the SWANK connection
2. Calls `connection-shutdown` (kills the child by PID)
3. Clears the connection so `ensure-child-connection` respawns
4. Returns an error result with diagnostics

### Amended Risk Register

| # | Risk | Category | Mitigation |
|---|------|----------|-----------|
| R8 | Cap of 3 may be too low for `run-tests-fresh` which spawns a separate SBCL | Resource | `run-tests-fresh` spawns via `uiop:run-program` (synchronous, not tracked as a child connection). Only SWANK-connected children count toward the cap. |
| R9 | `uiop/launch-program:process-info-pid` may not be portable | Portability | SBCL-specific `#+sbcl` guard with fallback to nil. PID is diagnostic, not load-bearing. |

### Amended Step Table

Insert after step 12 (server.lisp):

| Step | File | Action | Form(s) | Test | Cat |
|------|------|--------|---------|------|:---:|
| 12a | `src/mcp/image-connection.lisp` | modify | Add `connection-pid` generic (returns nil by default) | — | — |
| 12b | `src/mcp/server.lisp` | modify | Add `max-children`, `children` slots; enforce cap in `ensure-child-connection` | — | — |
| 13 | `src/mcp/child-connection.lisp` | new | (as before, plus `connection-pid` method using `uiop` process-info) | — | — |
| 16a | `testsuite/mcp/health-check-tests.lisp` | new | Add `validate-child-cap-enforced`, `validate-connection-pid-tracked` | S1 tests | slow |
| 19a | `src/mcp/tools/eval-form.lisp` | modify | On timeout: extract diagnostics, kill child by PID, clear connection | — | — |

### Amended Invariants

- **INV-46:** The MCP server never has more than `server-max-children`
  concurrent child connections. `ensure-child-connection` enforces this
  by shutting down the oldest idle connection when at capacity.
- **INV-47:** Every `image-connection` exposes its OS PID via
  `connection-pid`. The kernel generic returns NIL; concrete subclasses
  return the actual PID.
- **INV-48:** When `eval-form` times out and the child is unresponsive,
  the server kills the child process (by PID), extracts available
  diagnostics, and returns an error result. The next tool call gets a
  fresh child.

### Amended Acceptance Criteria

| # | Criterion | Mapped to |
|---|-----------|-----------|
| AC13 | Server never exceeds `max-children` concurrent connections | S1 (amended) |
| AC14 | `connection-pid` returns the OS PID of a live child | S1 (amended) |
| AC15 | Timed-out eval kills the child and next call gets a fresh one | S1 (amended) |
