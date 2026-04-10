# Implementation Phase 1 — Slice 009: MCP server skeleton

**Slice:** `product/slice/009-mcp-skeleton/slice.md`
**Phase:** 1 (of 1 planned)
**Scope:** The complete MCP server skeleton: `org.melusina.atelier/mcp` system, `#:atelier/mcp` package, MCP JSON-RPC handshake over a two-way-stream, CLOS-based tool and message registries, unified `define-tool` macro with optional `(:resource ...)` dual-expose, sexp-canonical transcript with fast in-memory encoders plus slow filesystem tests, abstract `image-connection` class, 6 tools, 8 resources. No child image, no eval, no SWANK.
**Prerequisites:** SBCL with Quicklisp available; `com.inuoe.jzon` installable.

## Back-link

Slice: `product/slice/009-mcp-skeleton/slice.md`

## Prior phases

None. This is phase 1 of a single-phase slice.

## Project Knowledge Applied

| Source | Entry | How it shaped this plan |
|---|---|---|
| `patterns.md` | *Hallucinated requirements from surface intuition* | Every class, slot, and generic in this plan traces to a specific acceptance criterion. Reviewer will audit at phase closure. |
| `patterns.md` | *Wrong dispatch mechanism in first implementation* | S3, S4, S8 acceptance criteria verify which generic was called and which class specialized it, not just that "the tool ran." |
| `patterns.md` | *Test-registry pollution across test runs* | `with-isolated-registries` macro rebinds `*tool-registry*` and `*resource-registry*` per test from day one. Slice 009 does not inherit the `"TEST"` package heuristic; it does the hard fix. |
| `patterns.md` | *Documentation count drift from live registry* | Added fast testcase asserting `(length *tool-registry*)` and `(length *resource-registry*)`. CLAUDE.md drift caught by the suite. |
| `patterns.md` | *Stale-fasl masking of load-order bugs* | **Retired for this slice.** S1 AC1 automates the fresh-SBCL subprocess check into the testsuite; INV-4 becomes an enforced suite guarantee, not a discipline. |
| `patterns.md` | *Plan pass-count predictions* | Plan gives a **range** for assertion-count delta, not a tight number. |
| `calibration.md` | "Counting the wrong unit" (4 data points) | Numeric criteria use invariants: "zero failures, no new skips, delta within range." |
| `invariants.md` | INV-4 (fresh SBCL) | Automated into the suite via S1 AC1. |
| `invariants.md` | INV-11 (templates are API consumers) | Phase-closure grep of `resource/template/` for `atelier/mcp` / `mcp`. Expected zero. |

## Risk Register

| # | Risk | Category | Severity | Mitigation |
|---|---|---|:---:|---|
| R1 | Hallucinated requirements in foundation slice | Architecture | HIGH | Every exported symbol traces to an acceptance criterion. Reviewer audit at phase closure. |
| R2 | Wrong CLOS dispatch mechanism on first cut | Architecture | MED-HI | Explicit dispatch assertions in tests (`find-method`, class-of, generic on hierarchy). |
| R3 | `define-tool` macro complexity | Macro Complexity | MEDIUM | Helpers extracted and unit-tested independently: `derive-tool-name-from-symbol`, `derive-input-schema-from-lambda-list`, `validate-uri-template-against-lambda-list`. Macro body is a thin `progn` over helper calls. |
| R4 | `com.inuoe.jzon` encoding surprises | Dependency | MEDIUM | **Step 0 is a jzon round-trip smoke test** before any protocol code is written. Canonical MCP message shapes round-tripped; results recorded in `references/jzon-round-trip.md`. |
| R5 | SBCL-subprocess test brittleness (S1 AC1) | Portability | MEDIUM | `uiop:run-program` with 30-second timeout, `--no-sysinit --no-userinit --non-interactive`, skip on `#-sbcl` and unresolvable `sb-ext:*runtime-pathname*`. Verify test fails correctly by temporarily introducing a `warn` call. |
| R6 | Test-registry pollution | Test Hygiene | MEDIUM | `with-isolated-registries` macro — hard fix, no "TEST" heuristic. |
| R7 | Phase boundary — slice 010 signature fit | Phase-Boundary | MEDIUM | Minimal signatures shipped: `connection-eval`, `connection-shutdown`, `connection-alive-p`. Extension rule documented in class docstring: slice 010 *adds* generics, never *changes* these. |
| R8 | Scope creep — eval / SWANK in slice 009 | Scope | MEDIUM | Phase-closure audit: zero SWANK references, `image-connection` has no concrete subclass, exactly 6 tools and 8 resources in the registries. |
| R9 | Documentation count drift | Project-Knowledge | LOW | Count assertions live in the fast testsuite; CLAUDE.md drift caught on every run. |
| R10 | Templates grep (INV-11) | Project-Knowledge | LOW | Phase-closure grep. Expected zero matches. |
| R11 | MCP spec-version negotiation | Dependency | LOW | Server always replies with `"2024-11-05"`; documented. |
| R12 | Pass-count prediction | Project-Knowledge | LOW | Range, not tight number. Reviewer audits at closure. |
| R13 | Uncaught handler condition drops into debugger | Runtime | MEDIUM | Server binds `*debugger-hook*` to a function that converts any uncaught condition to a protocol error response and returns, preventing stdio hang. |

## OSS Components

**Added dependency:** `com.inuoe.jzon`
- **Version:** latest from Quicklisp at slice start (maintained through 2026-03).
- **License:** MIT.
- **Role:** JSON encoding and decoding for MCP JSON-RPC envelopes and tool/resource payloads.
- **Rationale:** Distinguishes `null` / `true` / `false` unambiguously by default, round-trips RFC 8259 correctly, permissive license, actively maintained, in stock Quicklisp. The three other credible candidates (shasht, yason, cl-json) were rejected per Stage 2 OSS check: shasht is equally correct but toggles defaults via specials (footgun under concurrency); yason's null-distinction is configurable per-parse; cl-json by default conflates `nil` / `false` / `null` / `[]` and mangles key case.
- **Integration approach:** Thin wrapper module `src/mcp/json-util.lisp` exposes `encode-to-string`, `decode-from-string`, and the `sexp-to-json` walker. Handlers never call jzon directly; they call the wrapper. This preserves the option to swap libraries later with a single-file change.
- **Not installed by the Maker.** Assume the operator has run `(ql:quickload "com.inuoe.jzon")` once in this image before test-system invocation, or that Quicklisp resolves it automatically on first `asdf:load-system`.

**Considered and rejected:**
- **40ants/mcp** — unclear license (not declared). Hard block.
- **belyak/mcp-srv-lisp** — Apache-2.0 orphan project; read as reference, not as dependency.
- **quasi/cl-mcp-server** — MIT reference implementation; read as reference, not as dependency.
- **cxxxr/jsonrpc** — ties JSON-RPC dispatch to transport; the 40ants team explicitly chose not to use it for their MCP server, which is a strong signal.

**Reference reading (not dependencies):**
- `belyak/mcp-srv-lisp` and `quasi/cl-mcp-server` — Maker may read these for edge-case framing details (particularly the `notifications/initialized` no-response handling) but must not copy non-trivial chunks verbatim. Any derived idiom is documented in `references/mcp-protocol.md` with the source repo named.

## Phase Scope

**Stories delivered in this phase:** all 8 from slice.md (S1–S8).

**Stories deferred:** none. Slice 009 is a single-phase slice.

**Tool count (revised from slice.md):** 6 tools, 8 resources. Slice.md listed 4 tools and 7 resources; the dual-expose design in Stage 4 grew this. Breakdown:

| Definition | Package home | Derived tool name | Resource URI | Mime-type |
|---|---|---|---|---|
| `probe-environment` | `atelier/mcp` | `atelier:probe-environment` | — | — |
| `list-inspectors` | `atelier/mcp` | `atelier:list-inspectors` | `atelier://inspectors` | application/json |
| `list-maintainers` | `atelier/mcp` | `atelier:list-maintainers` | `atelier://maintainers` | application/json |
| `list-systems` | `atelier/mcp` | `atelier:list-systems` | `lisp://systems` | application/json |
| `inspector-detail` | `atelier/mcp` | `atelier:inspector-detail` | `atelier://inspectors/{name}` | application/json |
| `maintainer-detail` | `atelier/mcp` | `atelier:maintainer-detail` | `atelier://maintainers/{name}` | application/json |
| `transcript-sexp` | `atelier/mcp` | — (resource only) | `lisp://transcript/{session-id}.sexp` | text/plain |
| `transcript-json` | `atelier/mcp` | — (resource only) | `lisp://transcript/{session-id}.json` | application/json |
| `transcript-markdown` | `atelier/mcp` | — (resource only) | `lisp://transcript/{session-id}.md` | text/markdown |

The slice.md tool and resource lists will be updated **before** phase execution begins (slice.md is still Planned, not In Progress) to reflect this count.

## File Organisation

```
org.melusina.atelier.asd              [modify]  — add org.melusina.atelier/mcp system
                                                    and org.melusina.atelier/testsuite mcp module

src/mcp/                              [new]
  package.lisp                        — defpackage #:atelier/mcp, exports
  conditions.lisp                     — mcp-error, tool-not-found, resource-not-found, not-implemented
  protocol-version.lisp               — +mcp-protocol-version+ constant "2024-11-05"
  json-util.lisp                      — jzon wrappers + sexp-to-json walker
  tool-name.lisp                      — derive-tool-name-from-symbol helper
  input-schema.lisp                   — derive-input-schema-from-lambda-list helper
  uri-template.lisp                   — parse-uri-template, match-uri-against-template, validate-against-lambda-list
  tool.lisp                           — tool class, resource-tool mixin, *tool-registry*, *resource-registry*, register-tool
  message.lisp                        — mcp-message hierarchy, parse-mcp-message, method-to-class table
  define-tool.lisp                    — define-tool macro (thin progn over helpers)
  transcript.lisp                     — transcript class, write-transcript-entry, read-transcript-entries
  transcript-render.lisp              — sexp-to-json walker, sexp-to-markdown renderer (pure functions on plist sequences)
  image-connection.lisp               — abstract class, connection-eval, connection-shutdown, connection-alive-p
  dispatcher.lisp                     — handle-message generic + methods for each request/notification class
  server.lisp                         — serve-two-way-stream entry point, *debugger-hook* installation
  tools/
    probe-environment.lisp            — atelier:probe-environment
    list-inspectors.lisp              — atelier:list-inspectors (dual-exposed)
    list-maintainers.lisp             — atelier:list-maintainers (dual-exposed)
    list-systems.lisp                 — atelier:list-systems (dual-exposed)
    inspector-detail.lisp             — atelier:inspector-detail (dual-exposed with template)
    maintainer-detail.lisp            — atelier:maintainer-detail (dual-exposed with template)
    transcript-resources.lisp         — three transcript resources (resource-only)

testsuite/mcp/                        [new]
  package.lisp                        — defpackage #:atelier/testsuite/mcp
  utilities.lisp                      — with-isolated-registries, transcript-filesystem-available-p
  jzon-round-trip.lisp                — Step 0 jzon smoke test
  tool-name-derivation.lisp           — derive-tool-name-from-symbol tests
  input-schema-derivation.lisp        — derive-input-schema-from-lambda-list tests
  uri-template.lisp                   — parse/match/validate-uri-template tests
  define-tool-macro.lisp              — macro expansion + dispatch tests
  message-parsing.lisp                — parse-mcp-message tests (one per method)
  dispatcher.lisp                     — handle-message per-class tests + debugger-hook test
  protocol-handshake.lisp             — initialize / tools-list / resources-list via string streams
  tool-invocation.lisp                — tools/call for each of the 6 tools
  resource-read.lisp                  — resources/read for each of the 8 resources
  image-connection.lisp               — abstract class + 3 generics
  transcript-encoding.lisp            — sexp-to-json + sexp-to-markdown walkers (fast, in-memory)
  transcript-filesystem.lisp          — file-touching tests (slow)
  transcript-torn-write.lisp          — torn-write recovery (slow)
  registry-counts.lisp                — assert 6 tools, 8 resources (drift detector)
  fresh-sbcl-load.lisp                — S1 AC1 subprocess load test (slow)
  entrypoint.lisp                     — testsuite entry point registration
  fixtures/
    initialize-request.json
    initialize-response.json
    tools-list-request.json
    resources-list-request.json
    tools-call-probe-environment-request.json

product/slice/009-mcp-skeleton/references/    [new or update]
  python-prototype-notes.md           [existing, to fill in]
  jzon-round-trip.md                  [new] Step 0 findings
  mcp-protocol.md                     [new] pinned spec version, envelope shapes, framing
  define-tool-macro.md                [new] macro design notes
  message-hierarchy.md                [new] class diagram and handle-message dispatch rules
```

## Build System Changes

`org.melusina.atelier.asd` gets two new defsystem forms (both in the primary `.asd` per MEMORY.md's ASDF conventions):

```lisp
(asdf:defsystem #:org.melusina.atelier/mcp
  :description "MCP server skeleton for Atelier."
  :author "Michaël Le Barbier"
  :depends-on (#:alexandria
               #:uiop
               #:com.inuoe.jzon
               #:org.melusina.atelier)
  :components
  ((:module "src/mcp"
    :serial t
    :components ((:file "package")
                 (:file "conditions")
                 (:file "protocol-version")
                 (:file "json-util")
                 (:file "tool-name")
                 (:file "input-schema")
                 (:file "uri-template")
                 (:file "tool")
                 (:file "message")
                 (:file "define-tool")
                 (:file "transcript-render")
                 (:file "transcript")
                 (:file "image-connection")
                 (:file "dispatcher")
                 (:file "server")
                 (:module "tools"
                  :serial t
                  :components ((:file "probe-environment")
                               (:file "list-inspectors")
                               (:file "list-maintainers")
                               (:file "list-systems")
                               (:file "inspector-detail")
                               (:file "maintainer-detail")
                               (:file "transcript-resources")))))))

(asdf:defsystem #:org.melusina.atelier/testsuite/mcp
  :description "Testsuite for org.melusina.atelier/mcp."
  :author "Michaël Le Barbier"
  :depends-on (#:org.melusina.atelier/mcp
               #:org.melusina.atelier/testsuite
               #:org.melusina.confidence)
  :components
  ((:module "testsuite/mcp"
    :serial t
    :components ((:file "package")
                 (:file "utilities")
                 (:file "jzon-round-trip")
                 (:file "tool-name-derivation")
                 (:file "input-schema-derivation")
                 (:file "uri-template")
                 (:file "define-tool-macro")
                 (:file "message-parsing")
                 (:file "dispatcher")
                 (:file "protocol-handshake")
                 (:file "tool-invocation")
                 (:file "resource-read")
                 (:file "image-connection")
                 (:file "transcript-encoding")
                 (:file "transcript-filesystem")
                 (:file "transcript-torn-write")
                 (:file "registry-counts")
                 (:file "fresh-sbcl-load")
                 (:file "entrypoint")))))
```

**Load-order rationale:** within `src/mcp/`, package → conditions → protocol-version → json-util (pure, only depends on jzon) → name/schema/uri-template helpers (pure, only depend on json-util) → tool (uses helpers) → message (uses tool for the tool-call-request slot) → define-tool (uses tool + helpers) → transcript-render (pure functions on plists) → transcript (uses render + filesystem) → image-connection → dispatcher (uses tool, message, transcript, image-connection) → server (uses dispatcher) → tools/*. This order satisfies INV-4's cold-start requirement: every file's dependencies are loaded before it.

**No changes** to `org.melusina.atelier` or `org.melusina.atelier/testsuite`. The MCP work is additive.

## Package / Module Architecture

**New package:** `#:atelier/mcp` (no nickname for slice 009).

**Exported symbols, grouped:**

| Group | Symbols | Defined in |
|---|---|---|
| Entry point | `serve-two-way-stream` | server.lisp |
| Protocol version | `+mcp-protocol-version+` | protocol-version.lisp |
| Tool definition | `define-tool`, `tool`, `resource-tool`, `handle-tool-call` | define-tool.lisp, tool.lisp |
| Registries | `*tool-registry*`, `*resource-registry*`, `register-tool`, `list-tools`, `list-resources`, `find-tool-by-name`, `find-resource-by-uri` | tool.lisp |
| Message hierarchy | `mcp-message`, `mcp-request`, `mcp-notification`, `mcp-response`, `mcp-success-response`, `mcp-error-response`, `initialize-request`, `initialized-notification`, `tools-list-request`, `tools-call-request`, `resources-list-request`, `resources-read-request`, `parse-mcp-message`, `handle-message` | message.lisp, dispatcher.lisp |
| Transcript | `transcript`, `make-transcript`, `write-transcript-entry`, `read-transcript`, `read-transcript-entries` | transcript.lisp |
| Image connection | `image-connection`, `connection-id`, `connection-process-info`, `connection-eval`, `connection-shutdown`, `connection-alive-p` | image-connection.lisp |
| Conditions | `mcp-error`, `tool-not-found`, `resource-not-found`, `not-implemented`, `tool-not-found-error`, `resource-not-found-error`, `resource-not-found` (function) | conditions.lisp |

**R1 audit point:** every symbol above is referenced in at least one acceptance criterion. The Reviewer checks this at phase closure.

## Type / Class Hierarchy

```
tool                                   [abstract base]
├── resource-tool                      [mixin — presence marks a definition as a resource]
└── (concrete tool classes generated per define-tool form)

mcp-message                            [abstract base]
├── mcp-request
│   ├── initialize-request
│   ├── tools-list-request
│   ├── tools-call-request
│   ├── resources-list-request
│   └── resources-read-request
├── mcp-notification
│   └── initialized-notification
└── mcp-response
    ├── mcp-success-response
    └── mcp-error-response

image-connection                       [abstract; no concrete subclass in slice 009]

transcript                             [standard-class; concrete]

condition hierarchy:
condition
└── mcp-error
    ├── tool-not-found
    ├── resource-not-found
    └── not-implemented
```

**Concrete vs. abstract:**
- `tool`, `mcp-message`, `mcp-request`, `mcp-notification`, `mcp-response`, `image-connection` are abstract (no direct instances).
- Concrete tool classes are generated per `define-tool` form (6 in slice 009).
- Concrete request classes: 5 in slice 009 (one per MCP method the server handles).
- `transcript` is concrete.

## Protocol Definitions

```lisp
(defgeneric handle-tool-call (tool arguments)
  (:documentation
   "Execute TOOL with parsed ARGUMENTS (an alist of string keys to values).
    Return the handler body's value. The dispatcher converts the result
    to the appropriate response envelope based on how the call arrived
    (tools/call wraps in content[].text; resources/read wraps in contents[].text
    with the declared MIME type). Handlers return Lisp data for JSON MIME types
    and strings for text/* MIME types."))

(defgeneric resource-uri-template (tool)
  (:documentation
   "Return the URI template string for TOOL when it is a resource-tool,
    NIL otherwise. Template syntax: {name} placeholders."))

(defgeneric resource-mime-type (tool)
  (:documentation
   "Return the declared MIME type keyword or string for TOOL when it is
    a resource-tool, NIL otherwise."))

(defgeneric handle-message (message server)
  (:documentation
   "Handle MESSAGE in the context of SERVER. Dispatch via CLOS specialization
    on the message class. Return a response instance for requests, NIL for
    notifications. An :after method on MCP-MESSAGE writes the message-response
    pair to the transcript."))

(defgeneric parse-mcp-message (json-hash-table)
  (:documentation
   "Parse a jzon-decoded hash-table into a typed MCP-MESSAGE subclass.
    Method string maps to class via METHOD-TO-CLASS. Presence of ID
    distinguishes requests from notifications."))

(defgeneric connection-eval (connection form)
  (:documentation
   "Evaluate FORM in CONNECTION's image. Return result as a string.
    Abstract — slice 009's primary method signals NOT-IMPLEMENTED.
    Slice 010's SWANK-CONNECTION will add a concrete method."))

(defgeneric connection-shutdown (connection)
  (:documentation
   "Gracefully shut down CONNECTION's image. Default method delegates to
    UIOP:TERMINATE-PROCESS + UIOP:WAIT-PROCESS on the PROCESS-INFO slot
    when non-NIL."))

(defgeneric connection-alive-p (connection)
  (:documentation
   "Return T if CONNECTION's image is running. Default method delegates
    to UIOP:PROCESS-ALIVE-P on the PROCESS-INFO slot when non-NIL."))

(defgeneric write-transcript-entry (transcript entry)
  (:documentation
   "Append ENTRY (a plist) to TRANSCRIPT's canonical sexp file.
    Protocol: PRIN1 entry + TERPRI + FINISH-OUTPUT, in that order,
    so a torn write at process exit leaves a readable file."))
```

**Extension rule (for slice 010 and beyond):** generic signatures above **may be extended** by adding methods; they **must not be changed**. Breaking changes to these signatures require a new slice with its own risk review.

## Error / Condition Types

| Name | Superclass | Slots | When signalled |
|---|---|---|---|
| `mcp-error` | `error` | `message` | Base class for all slice-009 conditions. Never signalled directly. |
| `tool-not-found` | `mcp-error` | `tool-name` | `tools/call` with a name not in `*tool-registry*`. Converted by the dispatcher to an in-band tool-call error result, NOT a JSON-RPC protocol error. |
| `resource-not-found` | `mcp-error` | `uri` | `resources/read` with a URI that matches no template in `*resource-registry*`. Converted to an in-band `resources/read` error result. |
| `not-implemented` | `mcp-error` | `operation`, `class` | Abstract generic's primary method called. Signalled by `connection-eval` and `connection-shutdown` defaults on `image-connection` when no concrete subclass overrides. |
| `invalid-tool-arguments` | `mcp-error` | `tool-name`, `errors` | `tools/call` arguments don't match the tool's input schema. Converted to an in-band error. |
| `invalid-uri-template` | `mcp-error` | `template`, `lambda-list`, `mismatch` | Compile-time error signalled by `define-tool` macro when URI template placeholders don't match `&key` lambda list. Not a runtime condition. |

**Error policy:** conditions signalled *inside* a tool handler or resource reader are caught by `handle-message`'s `:around` method on the relevant request class, converted to in-band tool-call error results, and the server continues serving. The `*debugger-hook*` installed by `serve-two-way-stream` catches anything that escapes and converts it to a JSON-RPC protocol error (`-32603` internal error) before returning. **No condition can hang the server.**

## Test Plan

Every story → test name → category → skip condition.

| Story | Test name | Category | Skip condition |
|---|---|:---:|---|
| — (Step 0) | `validate-jzon-round-trip` | fast | — |
| S1.2 | `validate-atelier/mcp-package-exists` | fast | — |
| S1.3 | `validate-atelier/mcp-exported-symbols` | fast | — |
| S1.4 | `validate-serve-two-way-stream-on-string-streams` | fast | — |
| S2.1 | `validate-initialize-handshake` | fast | — |
| S2.2 | `validate-tools-list-returns-six-tools` | fast | — |
| S2.3 | `validate-resources-list-returns-eight-resources` | fast | — |
| S2.4 | `validate-malformed-json-rpc-returns-parse-error` | fast | — |
| S2.5 | `validate-unknown-method-returns-method-not-found` | fast | — |
| S3.1 | `validate-define-tool-round-trip` | fast | — |
| S3.1 | `validate-derive-tool-name-from-symbol` | fast | — |
| S3.1 | `validate-derive-input-schema-from-lambda-list` | fast | — |
| S3.2 | `validate-probe-environment-tool` | fast | — |
| S3.3 | `validate-list-inspectors-tool-shape` | fast | — |
| S3.3 | `validate-list-inspectors-dispatch-specializes-on-class` | fast | — |
| S3.4 | `validate-list-maintainers-tool-shape` | fast | — |
| S3.5 | `validate-list-systems-tool-shape` | fast | — |
| S3.5 | `validate-list-systems-does-not-load-any-system` | fast | — |
| S3.6 | `validate-tool-handler-error-becomes-in-band-result` | fast | — |
| S3.6 | `validate-debugger-hook-installed-during-serve` | fast | — |
| S4.1 | `validate-define-tool-with-resource-registers-both-ways` | fast | — |
| S4.2 | `validate-read-atelier-inspectors-resource` | fast | — |
| S4.3 | `validate-read-atelier-inspector-detail-template` | fast | — |
| S4.3 | `validate-uri-template-parses-and-matches` | fast | — |
| S4.3 | `validate-uri-template-mismatch-is-compile-time-error` | fast | — |
| S4.4 | `validate-read-atelier-maintainers-resource` | fast | — |
| S4.4 | `validate-read-atelier-maintainer-detail-template` | fast | — |
| S4.5 | `validate-unknown-inspector-returns-in-band-error` | fast | — |
| S5.1 | `validate-read-lisp-systems-resource` | fast | — |
| S5.2 | `validate-lisp-systems-idempotent` | fast | — |
| S5.4 | `validate-lisp-systems-does-not-load-any-system` | fast | — |
| S6.1 | `validate-transcript-file-opens-under-xdg-state-home` | slow | `transcript-filesystem-available-p` |
| S6.2 | `validate-tool-call-appends-transcript-entry` | slow | `transcript-filesystem-available-p` |
| S6.3 | `validate-read-transcript-sexp-resource` | slow | `transcript-filesystem-available-p` |
| S6.4 | `validate-sexp-to-json-walker` | fast | — |
| S6.5 | `validate-sexp-to-markdown-renderer` | fast | — |
| S6.6 | `validate-transcript-torn-write-recovery` | slow | `transcript-filesystem-available-p` |
| S6.6-companion | `validate-transcript-write-protocol-flushes-per-entry` | slow | `transcript-filesystem-available-p` |
| S7.1 | `validate-image-connection-class-exists` | fast | — |
| S7.2 | `validate-image-connection-generics-defined` | fast | — |
| S7.3 | `validate-image-connection-documentation-populated` | fast | — |
| S8.1 | `validate-mcp-system-loads-cleanly` (S1 AC1 subprocess) | slow | `#-sbcl` or unresolvable `sb-ext:*runtime-pathname*` |
| — (drift) | `validate-registry-counts` | fast | — |

**Totals:** 40 testcases — 33 fast, 7 slow, 0 snail, 0 closure (S5 AC3, S7 AC4, S8 AC5 remain closure items per Stage 5, not in the test plan).

**Pass-count delta prediction (range, per calibration):** phase 1 adds between **50 and 100 assertions** to the suite. Reviewer confirms actual falls inside the range; outside the range becomes a calibration data point, not a plan failure.

## Implementation Order (Step Table)

Strict topological order. Each step touches exactly one file. Fast before slow per story.

| Step | File | Action | Form(s) | Test name | Category |
|:---:|---|---|---|---|:---:|
| 1 | `product/slice/009-mcp-skeleton/references/jzon-round-trip.md` | Write | Step 0 jzon round-trip findings after reading jzon docs and running the smoke test interactively | — | — |
| 2 | `product/slice/009-mcp-skeleton/references/mcp-protocol.md` | Write | Pinned MCP spec 2024-11-05 envelope shapes, framing rules, error codes, derived from spec + Python prototype | — | — |
| 3 | `product/slice/009-mcp-skeleton/references/define-tool-macro.md` | Write | Macro design notes: name derivation rule, schema derivation rule, URI template validation rule, dual-expose semantics | — | — |
| 4 | `product/slice/009-mcp-skeleton/references/message-hierarchy.md` | Write | Class diagram, method-to-class table, handle-message dispatch rules | — | — |
| 5 | `product/slice/009-mcp-skeleton/references/python-prototype-notes.md` | Modify (fill in) | Maker reads `/Users/michael/Workshop/lisp_mcp/sbcl_eval.py` and records extracted conventions + override log | — | — |
| 6 | `org.melusina.atelier.asd` | Modify | Add `org.melusina.atelier/mcp` and `org.melusina.atelier/testsuite/mcp` defsystem forms | — | — |
| 7 | `src/mcp/package.lisp` | New | `defpackage #:atelier/mcp` with initial (small) export list; grows as files are added | — | — |
| 8 | `src/mcp/conditions.lisp` | New | `mcp-error`, `tool-not-found`, `resource-not-found`, `not-implemented`, `invalid-tool-arguments`, `invalid-uri-template` | — | — |
| 9 | `src/mcp/protocol-version.lisp` | New | `(defconstant +mcp-protocol-version+ "2024-11-05")` | — | — |
| 10 | `src/mcp/json-util.lisp` | New | `encode-to-string`, `decode-from-string`, `sexp-to-json` walker (pure plist→JSON), `json-to-sexp` helper | — | — |
| 11 | `testsuite/mcp/package.lisp` | New | `defpackage #:atelier/testsuite/mcp :use #:cl #:atelier/mcp #:org.melusina.confidence` | — | — |
| 12 | `testsuite/mcp/utilities.lisp` | New | `with-isolated-registries` macro, `transcript-filesystem-available-p`, helper to build test MCP messages | — | — |
| 13 | `testsuite/mcp/jzon-round-trip.lisp` | New | `validate-jzon-round-trip` — round-trip 4 canonical MCP message shapes | `validate-jzon-round-trip` | fast |
| 14 | `src/mcp/tool-name.lisp` | New | `derive-tool-name-from-symbol` — package nickname → strip `/mcp` → `:` + symbol name, all lowercase | — | — |
| 15 | `testsuite/mcp/tool-name-derivation.lisp` | New | Unit tests for `derive-tool-name-from-symbol` covering the 4 examples in `references/define-tool-macro.md` | `validate-derive-tool-name-from-symbol` | fast |
| 16 | `src/mcp/input-schema.lisp` | New | `derive-input-schema-from-lambda-list` — empty → `{"type":"object","properties":{}}`; keyword args with type declarations → full schema | — | — |
| 17 | `testsuite/mcp/input-schema-derivation.lisp` | New | Unit tests for `derive-input-schema-from-lambda-list` | `validate-derive-input-schema-from-lambda-list` | fast |
| 18 | `src/mcp/uri-template.lisp` | New | `parse-uri-template` (extract `{placeholder}` positions), `match-uri-against-template` (extract bindings), `validate-uri-template-against-lambda-list` (compile-time assertion) | — | — |
| 19 | `testsuite/mcp/uri-template.lisp` | New | Unit tests for parse/match/validate — static URI, URI with one placeholder, URI with two placeholders, mismatched lambda list | `validate-uri-template-parses-and-matches`, `validate-uri-template-mismatch-is-compile-time-error` | fast |
| 20 | `src/mcp/tool.lisp` | New | `tool` class, `resource-tool` mixin, `*tool-registry*`, `*resource-registry*`, `register-tool`, `handle-tool-call` generic, `resource-uri-template` generic, `resource-mime-type` generic, `list-tools`, `list-resources`, `find-tool-by-name`, `find-resource-by-uri` | — | — |
| 21 | `src/mcp/message.lisp` | New | `mcp-message` hierarchy (base + 3 intermediate + 5 request + 1 notification + 2 response), `parse-mcp-message`, `method-to-class` table | — | — |
| 22 | `testsuite/mcp/message-parsing.lisp` | New | Parse tests for each of the 6 method names; notifications vs. requests (presence of id); unknown method falls through to base class | `validate-parse-mcp-message-*` | fast |
| 23 | `src/mcp/define-tool.lisp` | New | `define-tool` macro — thin `progn` over helpers from steps 14, 16, 18; emits `defclass`, `defmethod handle-tool-call`, optionally `defmethod resource-uri-template` + `defmethod resource-mime-type`, and `register-tool` | — | — |
| 24 | `testsuite/mcp/define-tool-macro.lisp` | New | Macro expansion tests: tool-only, tool+resource-static, tool+resource-templated; dispatch tests asserting `find-method` returns the right method on the right class | `validate-define-tool-round-trip`, `validate-define-tool-with-resource-registers-both-ways` | fast |
| 25 | `src/mcp/transcript-render.lisp` | New | `sexp-to-json-entries` (pure plist-sequence → JSON string), `sexp-to-markdown-entries` (pure plist-sequence → Markdown string) | — | — |
| 26 | `testsuite/mcp/transcript-encoding.lisp` | New | Walker tests on in-memory plist sequences — no filesystem | `validate-sexp-to-json-walker`, `validate-sexp-to-markdown-renderer` | fast |
| 27 | `src/mcp/transcript.lisp` | New | `transcript` class, `make-transcript`, `write-transcript-entry` (`prin1`+`terpri`+`finish-output`), `read-transcript`, `read-transcript-entries` (tolerates torn tail) | — | — |
| 28 | `src/mcp/image-connection.lisp` | New | `image-connection` class with `id`, `process-info` slots; `connection-alive-p` (UIOP-delegating default), `connection-shutdown` (UIOP-delegating default), `connection-eval` (signals `not-implemented`) | — | — |
| 29 | `testsuite/mcp/image-connection.lisp` | New | Class exists, generics defined, documentation populated, `connection-eval` on bare instance signals `not-implemented`, `connection-alive-p` on nil-process-info returns nil | `validate-image-connection-class-exists`, `validate-image-connection-generics-defined`, `validate-image-connection-documentation-populated` | fast |
| 30 | `src/mcp/dispatcher.lisp` | New | `handle-message` generic + 5 primary methods (one per request class) + 1 notification method (no-op) + `:around` method on `tools-call-request` / `resources-read-request` catching handler conditions + `:after` method on `mcp-message` writing to transcript | — | — |
| 31 | `testsuite/mcp/dispatcher.lisp` | New | Per-class handle-message tests; `:around` converts signalled condition to in-band error; `:after` writes transcript entry | `validate-tool-handler-error-becomes-in-band-result` | fast |
| 32 | `src/mcp/server.lisp` | New | `serve-two-way-stream` — read-line loop, parse JSON, `parse-mcp-message`, `handle-message`, `encode-to-string`, write-line + `finish-output`. Installs `*debugger-hook*` for the dynamic extent of the call. Single output mutex | — | — |
| 33 | `testsuite/mcp/protocol-handshake.lisp` | New | Initialize → valid response; `notifications/initialized` → no response; `tools/list` → 6 tools; `resources/list` → 8 resources; malformed JSON → -32700; unknown method → -32601 | `validate-serve-two-way-stream-on-string-streams`, `validate-initialize-handshake`, `validate-tools-list-returns-six-tools`, `validate-resources-list-returns-eight-resources`, `validate-malformed-json-rpc-returns-parse-error`, `validate-unknown-method-returns-method-not-found`, `validate-debugger-hook-installed-during-serve` | fast |
| 34 | `testsuite/mcp/fixtures/initialize-request.json` | New | Recorded MCP initialize request | — | — |
| 35 | `testsuite/mcp/fixtures/initialize-response.json` | New | Expected initialize response (modulo version/timestamp fields) | — | — |
| 36 | `testsuite/mcp/fixtures/tools-list-request.json` | New | Recorded tools/list request | — | — |
| 37 | `testsuite/mcp/fixtures/resources-list-request.json` | New | Recorded resources/list request | — | — |
| 38 | `testsuite/mcp/fixtures/tools-call-probe-environment-request.json` | New | Recorded tools/call for probe-environment | — | — |
| 39 | `src/mcp/tools/probe-environment.lisp` | New | `(define-tool probe-environment () ...)` returning the 9-key alist | — | — |
| 40 | `src/mcp/tools/list-inspectors.lisp` | New | `(define-tool list-inspectors () (:resource ...) ...)` dual-exposed | — | — |
| 41 | `src/mcp/tools/list-maintainers.lisp` | New | `(define-tool list-maintainers () (:resource ...) ...)` dual-exposed | — | — |
| 42 | `src/mcp/tools/list-systems.lisp` | New | `(define-tool list-systems () (:resource ...) ...)` dual-exposed; walks ASDF source registry without loading | — | — |
| 43 | `src/mcp/tools/inspector-detail.lisp` | New | `(define-tool inspector-detail (&key name) (:resource ...) ...)` templated | — | — |
| 44 | `src/mcp/tools/maintainer-detail.lisp` | New | `(define-tool maintainer-detail (&key name) (:resource ...) ...)` templated | — | — |
| 45 | `src/mcp/tools/transcript-resources.lisp` | New | 3 resource-only `define-tool` forms for sexp/json/md views | — | — |
| 46 | `testsuite/mcp/tool-invocation.lisp` | New | Per-tool invocation tests; dispatch assertions via `find-method` | `validate-probe-environment-tool`, `validate-list-inspectors-tool-shape`, `validate-list-inspectors-dispatch-specializes-on-class`, `validate-list-maintainers-tool-shape`, `validate-list-systems-tool-shape`, `validate-list-systems-does-not-load-any-system` | fast |
| 47 | `testsuite/mcp/resource-read.lisp` | New | Per-resource read tests for all 8 resources | `validate-read-atelier-inspectors-resource`, `validate-read-atelier-inspector-detail-template`, `validate-read-atelier-maintainers-resource`, `validate-read-atelier-maintainer-detail-template`, `validate-read-lisp-systems-resource`, `validate-lisp-systems-idempotent`, `validate-unknown-inspector-returns-in-band-error` | fast |
| 48 | `testsuite/mcp/registry-counts.lisp` | New | Assert `(length (list-tools))` = 6, `(length (list-resources))` = 8 | `validate-registry-counts` | fast |
| 49 | `testsuite/mcp/transcript-filesystem.lisp` | New | File-opening + append-entry + read-back tests | `validate-transcript-file-opens-under-xdg-state-home`, `validate-tool-call-appends-transcript-entry`, `validate-read-transcript-sexp-resource` | slow |
| 50 | `testsuite/mcp/transcript-torn-write.lisp` | New | Torn-write recovery + write-protocol flush verification | `validate-transcript-torn-write-recovery`, `validate-transcript-write-protocol-flushes-per-entry` | slow |
| 51 | `testsuite/mcp/fresh-sbcl-load.lisp` | New | Subprocess `sbcl --non-interactive` loading `org.melusina.atelier/mcp`, zero warnings assertion | `validate-mcp-system-loads-cleanly` | slow |
| 52 | `testsuite/mcp/entrypoint.lisp` | New | Testsuite entry point — register all testcases with org.melusina.confidence | — | — |
| 53 | `src/mcp/package.lisp` | Modify | Finalize export list; grep references, close any gap | — | — |
| 54 | `CLAUDE.md` | Modify | Add MCP server section: entry point, tool/resource counts, serve-two-way-stream usage | — | — |
| 55 | `product/slice/009-mcp-skeleton/slice.md` | Modify | Update tool/resource lists to reflect 6 tools + 8 resources; mark Status In Progress (done at step 6 actually — move up) | — | — |

Step 55 is presentational; the substantive slice.md update to 6+8 happens at step 6 (before phase execution begins, consistent with "slice.md immutable once phase execution begins").

## Invariants

Slice 009 carries forward all invariants INV-1 through INV-11 from `product/knowledge/invariants.md`. No invariants are stretched or weakened by this slice.

**Candidate new invariants proposed for phase closure** (Reviewer decides whether to promote):

| # | Proposed invariant |
|---|---|
| **INV-12** | MCP tool handlers return Lisp data for `:application/json` MIME types and strings for `text/*` MIME types. The dispatcher owns the envelope. Handlers never call jzon directly. |
| **INV-13** | Generic function signatures on `image-connection` are stable across slices. Methods may be added; signatures may not be changed. Breaking changes require a new slice and risk review. |
| **INV-14** | Every `define-tool` form traces to exactly one exported symbol in one acceptance criterion. Foundation-slice risk R1 mitigation made durable. |
| **INV-15** | Transcript entries are written atomically per entry via the `prin1` + `terpri` + `finish-output` triple. A torn write at process exit leaves a file readable up to the last complete entry. |
| **INV-16** | Test code that calls `define-tool` or `register-tool` must wrap itself in `with-isolated-registries`. No test is allowed to mutate the global `*tool-registry*` or `*resource-registry*` persistently. |

## Test Fixtures

Five JSON fixture files under `testsuite/mcp/fixtures/`:

- `initialize-request.json` — one recorded MCP `initialize` frame with `protocolVersion: "2024-11-05"`
- `initialize-response.json` — expected response (modulo dynamic fields)
- `tools-list-request.json` — one recorded `tools/list` frame
- `resources-list-request.json` — one recorded `resources/list` frame
- `tools-call-probe-environment-request.json` — one recorded `tools/call` frame for `atelier:probe-environment`

**No sexp fixture files.** Transcript tests write plists in-memory or into temp files; no pre-recorded transcript corpus is needed for slice 009.

## References to Create

| File | Contents |
|---|---|
| `references/jzon-round-trip.md` | Step 0 findings: how jzon encodes alist/plist/hashtable, null/true/false handling, key case preservation, integer range. Concrete round-trip examples for the 4 canonical MCP shapes. **Must exist before any code using jzon is written.** |
| `references/mcp-protocol.md` | Pinned MCP spec version `"2024-11-05"`, envelope shapes (request, success, error, notification), stdio line-delimited framing rule, error code table, dispatch rules between protocol errors and in-band tool-call errors. Single authoritative document for the protocol surface. |
| `references/define-tool-macro.md` | Macro design notes: name derivation rule, `/mcp` stripping general rule, schema derivation rule, URI template placeholder-to-&key matching rule, dual-expose semantics, expansion output. Worked examples for tool-only, tool+static-resource, tool+templated-resource. |
| `references/message-hierarchy.md` | Class diagram for `mcp-message`, method-to-class table, `handle-message` dispatch rules, error policy, debugger hook installation. |
| `references/python-prototype-notes.md` (modify existing) | Fill in the placeholders with extracted conventions from `/Users/michael/Workshop/lisp_mcp/sbcl_eval.py`. Record the override log for every decision that differs from the prototype. |

## Acceptance Criteria

Numbered and verifiable. Each maps to specific `slice.md` stories.

1. **AC1 (slice S1):** `(asdf:load-system "org.melusina.atelier/mcp")` loads cleanly in a fresh SBCL subprocess with zero warnings, zero style warnings, zero errors. Verified by `validate-mcp-system-loads-cleanly`.
2. **AC2 (slice S1, S2):** After load, `(find-package "ATELIER/MCP")` returns a package. The exported symbol set contains exactly the symbols listed in Package / Module Architecture above, with no extras and no missing. Verified by `validate-atelier/mcp-exported-symbols`.
3. **AC3 (slice S2):** Initialize handshake over a string-stream round-trips correctly against the recorded fixtures; `tools/list` returns exactly **6** tools; `resources/list` returns exactly **8** resources. Verified by `validate-initialize-handshake`, `validate-tools-list-returns-six-tools`, `validate-resources-list-returns-eight-resources`.
4. **AC4 (slice S2):** Malformed JSON produces a `-32700` parse error without crashing; unknown method produces `-32601` without crashing. Verified by the corresponding testcases.
5. **AC5 (slice S3):** The 6 tools listed in Phase Scope are all registered, dispatch to methods specialized on their own generated class, and return the shape their docstring declares. Dispatch verified via `find-method`. Verified by `validate-define-tool-round-trip` and per-tool invocation tests.
6. **AC6 (slice S4):** The 8 resources listed in Phase Scope are all registered, match incoming URIs via template engine, and return content of the declared MIME type. Unknown inspector/maintainer names produce in-band `resources/read` errors, not protocol errors. Verified by per-resource read tests.
7. **AC7 (slice S3.6, S6):** A condition signalled inside a tool handler or resource reader becomes an in-band error result; the server continues serving. The `*debugger-hook*` is installed during `serve-two-way-stream` and intercepts any uncaught condition. Verified by `validate-tool-handler-error-becomes-in-band-result` and `validate-debugger-hook-installed-during-serve`.
8. **AC8 (slice S6):** Transcript infrastructure writes entries atomically (`prin1`+`terpri`+`finish-output` per entry), tolerates torn tails on read, and produces JSON and Markdown views from the same in-memory plist sequence. Three views are readable as resources. Verified by 7 S6 testcases.
9. **AC9 (slice S7):** `image-connection` exists as an abstract class with `connection-eval`, `connection-shutdown`, `connection-alive-p` generics, two of which have UIOP-delegating default methods. No concrete subclass is loaded by slice 009. Verified by S7 testcases + Reviewer code review.
10. **AC10 (cross-cutting):** Full regression — `asdf:test-system "org.melusina.atelier/mcp"` passes with zero failures, zero new skips outside the declared skip conditions, in a fresh SBCL subprocess per INV-4. Pass-count delta falls within the predicted range (50–100 new assertions).
11. **AC11 (cross-cutting):** `grep -r 'atelier/mcp\|#:mcp' resource/template/` returns zero matches (INV-11 audit).
12. **AC12 (R1 audit):** Every exported symbol in `#:atelier/mcp` traces to a referenced acceptance criterion. Reviewer lists any orphan symbols at phase closure; Maker justifies or removes each.

## Phase Closure Conditions

For `implementation-1-notes.md` to record this phase complete, all of the following must be true:

- [ ] All 55 steps in the step table executed and their form(s) committed to the working tree.
- [ ] All 12 acceptance criteria verified as listed.
- [ ] Fresh SBCL `asdf:test-system "org.melusina.atelier/mcp"` — zero failures, zero new skips outside declared conditions.
- [ ] Fresh SBCL `asdf:test-system "org.melusina.atelier"` — zero regression in the existing Atelier suite.
- [ ] Manual MCP-client smoke test recorded: launch `(atelier/mcp:serve-two-way-stream)` from an MCP client (or simulate with a hand-crafted stdio driver), observe: initialize handshake, `tools/list` returning 6 tools, `resources/list` returning 3 concrete resources, `resources/templates/list` returning 5 templates, `tools/call` of `atelier:probe-environment`, `resources/read` of `atelier://inspectors`, `resources/read` of `lisp://transcript/<session-id>.md`. Screenshots or transcripts retained in the implementation notes.
- [ ] Manual S5 AC3 smoke test: add a directory to ASDF source registry during a session, re-read `lisp://systems`, verify new system appears. Result recorded.
- [ ] Reviewer's R1 audit (every exported symbol traces to an AC) passes.
- [ ] Reviewer's R8 audit (zero SWANK references, no concrete `image-connection` subclass, registry counts exactly 6 tools + 3 concrete resources + 5 templates) passes.
- [ ] INV-11 template grep: zero matches.
- [ ] CLAUDE.md updated with MCP server section stating 6 tools, 3 concrete resources, 5 resource templates, `(atelier/mcp:serve-two-way-stream)` entry point.
- [ ] slice.md tool/resource lists updated to reflect the split.

---

## Plan Amendments

### Amendment 1 — 2026-04-10 — `resources/templates/list` split

**Trigger:** during step 2 (writing `references/mcp-protocol.md`), the MCP 2024-11-05 spec was fetched and revealed that concrete resources and templated resources are listed via **two distinct methods**:

- `resources/list` returns an array of `{uri, name, description?, mimeType?}` — **concrete, fixed-URI resources only**.
- `resources/templates/list` returns an array of `{uriTemplate, name, description?, mimeType?}` — URI-templated resources with placeholders.

The original plan assumed a single `resources/list` method returning all 8 URIs together.

**Impact:** the 8 resources declared in slice 009 split as **3 concrete + 5 templated**.

| Split | Count | URIs |
|---|---|---|
| Concrete (`resources/list`) | 3 | `atelier://inspectors`, `atelier://maintainers`, `lisp://systems` |
| Templated (`resources/templates/list`) | 5 | `atelier://inspectors/{name}`, `atelier://maintainers/{name}`, `lisp://transcript/{session-id}.sexp`, `lisp://transcript/{session-id}.json`, `lisp://transcript/{session-id}.md` |

**Consequent changes to the original plan:**

1. **New method class** in `src/mcp/message.lisp` (step 21): `resources-templates-list-request`. The `method-to-class` table gains the `"resources/templates/list"` mapping. **Seven** request/notification classes total (was six).

2. **Two registries on the resource side** in `src/mcp/tool.lisp` (step 20), replacing the single `*resource-registry*`:
   - `*concrete-resource-registry*` — keyed by URI string
   - `*template-resource-registry*` — keyed by URI template string

   `register-tool` on a `resource-tool` instance inserts into exactly one of them depending on whether the `(:uri ...)` form contains `{...}` placeholders. The URI template parsing in `src/mcp/uri-template.lisp` (step 18) is the classifier.

   The public helper functions become: `list-concrete-resources`, `list-template-resources`, `find-concrete-resource-by-uri`, `find-template-resource-by-template`, `match-resource-uri` (tries concrete first, then templates).

3. **New dispatcher method** in `src/mcp/dispatcher.lisp` (step 30): `handle-message` specializing on `resources-templates-list-request`. It walks `*template-resource-registry*` and emits `{uriTemplate, name, description, mimeType}` entries.

4. **Adjusted dispatcher method** for `resources-list-request`: it walks only `*concrete-resource-registry*` (previously walked the unified registry).

5. **Adjusted `resources-read-request` dispatcher**: URI matching order is (a) concrete registry lookup → (b) template registry walk with `match-uri-against-template` for each. This is the same behavior as originally planned; only the data sources change.

6. **New fixture** under `testsuite/mcp/fixtures/`: `resources-templates-list-request.json`. Used by the handshake test (step 33).

7. **Revised test in `testsuite/mcp/protocol-handshake.lisp`** (step 33):
   - `validate-resources-list-returns-three-concrete-resources` (renamed from `validate-resources-list-returns-eight-resources`)
   - NEW: `validate-resources-templates-list-returns-five-templates`

8. **Revised test in `testsuite/mcp/registry-counts.lisp`** (step 48): asserts `(length (list-tools))` = **6**, `(length (list-concrete-resources))` = **3**, `(length (list-template-resources))` = **5**.

9. **Revised test in `testsuite/mcp/resource-read.lisp`** (step 47): adds assertions that URIs with placeholders get routed through template matching, not concrete lookup.

10. **Revised acceptance criterion AC3** (in implementation-1.md AC section, and in slice.md S2 AC3):
    > Initialize handshake over a string-stream round-trips correctly against the recorded fixtures; `tools/list` returns exactly **6** tools; `resources/list` returns exactly **3** concrete resources; `resources/templates/list` returns exactly **5** templates.

11. **Revised acceptance criterion AC6** (implementation-1.md):
    > The 3 concrete resources and 5 templates listed in Phase Scope are all registered in their respective registries, match incoming URIs via the dispatcher's concrete-then-templates lookup order, and return content of the declared MIME type. Unknown inspector/maintainer names produce in-band errors; unknown URIs produce `-32002` protocol errors.

12. **Revised R8 mitigation** (risk register): the phase-closure audit asserts *exactly 6 tools, 3 concrete resources, and 5 templates*.

13. **Revised R11** (MCP spec-version negotiation): refined with the exact rule the spec mandates — "server MUST respond with the same version if it supports the requested one, otherwise with another version it supports." Slice 009 only supports `"2024-11-05"` and always responds with it regardless of the client's request.

14. **New MCP-specific error code** at the dispatcher level: `-32002` Resource not found. Added to the error code table in `conditions.lisp` (step 8) and `dispatcher.lisp` (step 30). The `resource-not-found` condition class maps to this code.

15. **Methods served increased from 6 to 7**: `initialize`, `notifications/initialized`, `tools/list`, `tools/call`, `resources/list`, `resources/templates/list`, `resources/read`. The `method-to-class` table in `src/mcp/message.lisp` (step 21) carries 7 entries.

**What does NOT change:**

- The total tool count (6) is unchanged.
- The total resource count (8) is unchanged — just split across two methods.
- The unified `define-tool` macro is unchanged — the `(:uri ...)` option with placeholders is already the classifier; the registration side gets the two-registry logic transparently.
- Step count in the table grows by approximately +2 (one new fixture, the renamed fixture; the source file additions are absorbed into existing steps). Call it **~57 steps** now instead of 55. The table itself is not rewritten — each amendment's effect is applied in the relevant step when that step is reached.
- Pass-count delta prediction range (50–100) is unchanged; the amendment adds ~4 assertions (split test, template-list fixture test, registry-split assertions), absorbed within the range.
- Invariants INV-1 to INV-11 carry forward unchanged. No new invariants introduced by this amendment.

**Why it's safe:** the amendment is a protocol-level refinement, not an architectural change. The unified `define-tool` macro already distinguishes templates from concrete URIs (via the `{...}` placeholder test) for compile-time validation; the amendment simply wires that distinction through to registration and dispatch. No redesign of the class hierarchy, no new generic, no new condition types beyond what `conditions.lisp` already exposes.

**Reviewer note at phase closure:** verify that (a) no code uses a unified `*resource-registry*` symbol anywhere; (b) the method-to-class table has exactly 7 entries; (c) the `resources/templates/list` method round-trips against its fixture; (d) `resources/read` still finds both kinds via the same `match-resource-uri` entry point.

### Amendment 2 — reserved

(Future amendments append here. Every amendment carries a date, trigger, impact, and explicit list of changes so the plan's narrative stays durable.)
