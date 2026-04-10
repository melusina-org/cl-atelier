# Slice 009: MCP server skeleton for `org.melusina.atelier/mcp`

**Status:** In Progress
**Type:** New capability
**Goal addressed:** G5 — MCP server exposes REPL evaluation, debugger and image lifecycle, CLOS introspection, and rename refactorings
**Backlog items:** #5 (MCP server skeleton)
**Planned start / end:** 2026-04-10 / TBD
**Actual end:** —
**Implementation phases:**
  - Phase 1: `product/slice/009-mcp-skeleton/implementation-1.md` — In Progress

---

## What changes for users

After this slice, an MCP-aware client (Claude Desktop, Cursor, or any MCP host) can launch `org.melusina.atelier/mcp` as a stdio subprocess and discover Atelier as an MCP server. The server exposes a small but real surface:

- **Tools** the agent can invoke: `atelier:probe-environment`, `atelier:list-inspectors`, `atelier:list-maintainers`, `atelier:list-systems`, `atelier:inspector-detail`, `atelier:maintainer-detail` (six total).
- **Resources** the agent can read: the Atelier inspector and maintainer registries, the ASDF source-registry's view of available systems, and the live session transcript in three views (sexp, JSON, Markdown).

What this slice **does not** ship: child Lisp images, eval, SWANK transport, package/symbol introspection, the debugger, refactorings, the linter passthrough. Those are slices 010–016. This slice is the foundation everything else is scaffolded onto.

The architectural shape established here — tool registry, resource registry, transcript subsystem, abstract `image-connection` class, MCP JSON-RPC framing over stdio — is the contract subsequent slices build against. Getting it right now means slices 010–016 add features without rework. Getting it wrong means they all pay interest.

Two design choices are worth surfacing for the user up-front:

1. **Standalone binary, not in-REPL.** The MCP server is delivered as `atelier-mcp`, a dumped SBCL image launched by the MCP client over stdio. It is not an in-REPL service. Subsequent slices manage one or more child Lisp images on demand; the MCP server's own image is the *manager*, not the workbench.
2. **Transcript is sexp-canonical, JSON and Markdown derived.** The on-disk transcript file is a sequence of plists, one per entry, append-only. The JSON view is computed by a small in-process walker on read. The Markdown view is rendered the same way. There is no Hunchentoot, no template engine, no Bootstrap. The Lisp form is the source of truth; the other two views are renderings.

## Specification references

- **MCP specification** — Model Context Protocol, JSON-RPC 2.0 base envelope, MCP-specific framing for stdio transport. The Tactician will pin the exact version during planning. The relevant primitives for this slice are: `initialize`, `tools/list`, `tools/call`, `resources/list`, `resources/read`, `notifications/initialized`. Streaming and prompts are out of scope.
- **Python prototype reference:** `/Users/michael/Workshop/lisp_mcp/sbcl_eval.py` — a working SWANK MCP server in Python. The Tactician will read this during the planning interview to extract: tool naming conventions, JSON-RPC envelope shape, child-image lifecycle pattern, and gotchas the prototype hit. Notes captured in `references/python-prototype-notes.md`.
- **`org.melusina.atelier`** — the slice depends on `atelier:list-inspectors`, `atelier:list-maintainers`, and the Atelier inspector/maintainer registries being introspectable. Both functions exist as of slice 008.
- **ASDF source registry** — used by `lisp://systems` to enumerate available systems without loading them.

## Stories

### S1 — `org.melusina.atelier/mcp` system and `#:atelier/mcp` package

**In order to** depend on the MCP server from another system, **a** developer **can** declare `(:depends-on "org.melusina.atelier/mcp")` in their `.asd` file and `(:use #:atelier/mcp)` in their package definition.

**Acceptance criteria:**
- Given `org.melusina.atelier.asd` declares the secondary system `org.melusina.atelier/mcp`, when `(asdf:load-system "org.melusina.atelier/mcp")` is called from a fresh SBCL, then it loads cleanly with no warnings.
- Given the system is loaded, when `(find-package "ATELIER/MCP")` is called, then it returns a package whose name is `"ATELIER/MCP"`.
- Given the package exists, when its exported symbols are listed, then they include at minimum `serve-stdio`, `register-tool`, `register-resource`, `*tool-registry*`, `*resource-registry*`, and the abstract class `image-connection`.
- Given a fresh SBCL with only `org.melusina.atelier/mcp` loaded, when `(atelier/mcp:serve-stdio)` is called, then it begins reading MCP JSON-RPC frames from `*standard-input*` and writing responses to `*standard-output*`. (Manual smoke test, recorded in slice closure.)

### S2 — MCP protocol handshake over stdio

**In order to** be discovered by an MCP client, **a** running `atelier-mcp` instance **can** complete the MCP `initialize` handshake, declare its capabilities, and respond to `tools/list` and `resources/list`.

**Acceptance criteria:**
- Given a recorded MCP `initialize` request frame, when fed to `atelier/mcp:serve-stdio` via a string-input-stream, then the server responds with a valid `initialize` result declaring `tools` and `resources` capabilities and the server name `"org.melusina.atelier/mcp"`.
- Given the handshake has completed and a `notifications/initialized` notification has been sent, when a `tools/list` request arrives, then the server returns the registered tool list (6 tools: `atelier:probe-environment`, `atelier:list-inspectors`, `atelier:list-maintainers`, `atelier:list-systems`, `atelier:inspector-detail`, `atelier:maintainer-detail`), each with `name`, `description`, and JSON-Schema `inputSchema`.
- Given the handshake has completed, when a `resources/list` request arrives, then the server returns the registered resource list including all eight URIs declared in S4–S6.
- Given a malformed JSON-RPC frame, when received, then the server responds with a JSON-RPC error of code `-32700` (parse error) and does not crash.
- Given an unknown method, when received, then the server responds with `-32601` (method not found) and does not crash.

### S3 — Tool registry and six tools

**In order to** add a new MCP tool in a later slice, **a** developer **can** write a `define-tool` form in any loaded system and have the tool appear in `tools/list` without any other registration step.

**Acceptance criteria:**
- Given `define-tool` is used to declare a tool (name derived from the symbol's home package with the `/mcp` suffix stripped, combined with the symbol name via `:`), when the tool is invoked via `tools/call`, then the handler method specialized on the generated class is called with the parsed arguments and its return value is wrapped in a valid `tools/call` result.
- Given the tool `atelier:probe-environment` is registered with no arguments, when `tools/call` invokes it, then the result content is a JSON object carrying `lisp-implementation-type`, `lisp-implementation-version`, `machine-instance`, `machine-type`, `machine-version`, `software-type`, `software-version`, `short-site-name`, `long-site-name`.
- Given the tool `atelier:list-inspectors` is registered, when invoked, then the result is a JSON array of objects, each with at minimum the keys `name`, `severity`, `description`, derived by walking `(atelier:list-inspectors)`.
- Given the tool `atelier:list-maintainers` is registered, when invoked, then the result is a JSON array of objects with at minimum `name`, `kind`, `reacts-to`, `supersedes`, `maturity`, derived by walking `(atelier:list-maintainers)`.
- Given the tool `atelier:list-systems` is registered, when invoked, then the result is a JSON array of ASDF system names visible to the source registry, obtained without loading any system.
- Given the tool `atelier:inspector-detail` is registered with a `name` argument, when invoked with a known inspector name, then the result is a JSON object carrying that inspector's full metadata (name, severity, finding class hierarchy ancestors, languages, full docstring).
- Given the tool `atelier:maintainer-detail` is registered with a `name` argument, when invoked with a known maintainer name, then the result is a JSON object carrying that maintainer's full metadata.
- Given a tool handler signals an error, when caught by the dispatcher, then the JSON-RPC response is a `tools/call` result with `isError: true` (not a JSON-RPC protocol error) carrying the condition's printed representation, and the server continues serving subsequent requests.

### S4 — Resource registry and the Atelier registry resources

**In order to** add a new MCP resource in a later slice, **a** developer **can** call `(atelier/mcp:register-resource ...)` from any loaded system and have the resource appear in `resources/list`.

**Acceptance criteria:**
- Given `register-resource` is called with a URI template, name, MIME type, and reader function, when the resource is read via `resources/read`, then the reader function is called and its returned bytes are wrapped in a valid resource content block.
- Given `atelier://inspectors` is registered, when read, then the response is a JSON document containing one entry per inspector in `(atelier:list-inspectors)`, each with the same fields as `atelier.list-inspectors` returns.
- Given `atelier://inspectors/<name>` is registered as a templated resource, when read with a specific inspector name, then the response includes that inspector's full metadata: name, severity, finding class name, finding class hierarchy ancestors, languages, and full docstring.
- Given `atelier://maintainers` and `atelier://maintainers/<name>` are registered, when read, then they return analogous documents for the maintainer registry.
- Given an `atelier://inspectors/<unknown>` URI, when read, then the server responds with a `resources/read` error result (not a JSON-RPC protocol error) carrying a "no such inspector" message.

### S5 — `lisp://systems` resource (ASDF source registry)

**In order to** discover what systems are available without loading any of them, **a** developer **can** read `lisp://systems` and receive the list as data.

**Acceptance criteria:**
- Given the ASDF source registry is initialised, when `lisp://systems` is read, then the response is a JSON array of system names. Each entry includes `name` and `source-file` (the `.asd` pathname).
- Given the resource is read twice, when ASDF's registry has not changed in between, then both reads return identical data. (Sanity check that the reader is pure.)
- Given a directory is added to the source registry between reads, when re-read, then the new systems appear. (Manual smoke test in slice closure.)
- Given the reader runs, when it executes, then it does not call `asdf:load-system` for any system, verified by counting `system-loaded-p` entries before and after.

### S6 — Transcript subsystem (sexp-canonical, JSON and Markdown views)

**In order to** audit, replay, or share what an agent did in an MCP session, **a** developer **can** read `lisp://transcript/<session-id>.sexp`, `.json`, or `.md` at any time during a session.

**Acceptance criteria:**
- Given an MCP session is established, when the server starts, then a session id is assigned and a transcript file is opened in append mode at a deterministic path under `(uiop:xdg-cache-home "atelier/mcp/transcripts/")`.
- Given any tool call or resource read occurs, when the dispatcher handles it, then a transcript entry is appended to the file as one s-expression per line, an alist with at minimum the keys `:timestamp`, `:session-id`, `:seq`, `:kind`, plus kind-specific fields.
- Given the session has logged at least one entry, when `lisp://transcript/<session-id>.sexp` is read, then the response is the raw file contents (UTF-8 text, sexp framing).
- Given the same session, when `lisp://transcript/<session-id>.json` is read, then the response is a JSON array, one element per logged entry, derived by mapping each plist through a small `sexp-to-json` walker (no dependency on cl-json or jzon outside what S2 already needs).
- Given the same session, when `lisp://transcript/<session-id>.md` is read, then the response is a Markdown document with one section per entry, formatted by walking the same plists with a simple renderer.
- Given the server crashes mid-write, when restarted, then the transcript file is still readable (no partial-line corruption — entries are written as one `(prin1 entry stream) (terpri stream) (finish-output stream)` triple).

### S7 — Abstract `image-connection` class

**In order to** plug a child-image transport in slice 010 without rewriting the dispatcher, **a** developer in slice 010 **can** define a concrete subclass of `image-connection` and have the existing tool dispatch path delegate to it.

**Acceptance criteria:**
- Given `org.melusina.atelier/mcp` is loaded, when `(find-class 'atelier/mcp:image-connection)` is called, then it returns a `standard-class` (abstract — no direct instances expected).
- Given the class definition exists, when its protocol methods are listed, then at minimum the generic functions `connection-eval`, `connection-shutdown`, and `connection-alive-p` are defined as generics dispatching on `image-connection`, each signalling `not-implemented` in their primary method.
- Given the class is documented, when `(documentation 'atelier/mcp:image-connection 'type)` is called, then it returns a docstring explaining that this is the slice-010 extension point and that no concrete subclass ships in slice 009.
- Given no concrete subclass is loaded, when slice 009 ships, then no tool in this slice depends on a live `image-connection`. (Verified by code review during phase closure.)

### S8 — Test recording for the protocol handshake

**In order to** regression-test the MCP protocol surface without requiring an actual MCP client, **a** developer **can** run the testsuite and have the four `atelier/mcp` testcases pass against recorded JSON-RPC frame fixtures.

**Acceptance criteria:**
- Given the system `org.melusina.atelier/test/mcp` is loaded, when `(asdf:test-system "org.melusina.atelier/mcp")` is run, then it dispatches to the testsuite and reports zero failures.
- Given a fixture file `test/mcp/fixtures/initialize-request.json` containing a recorded MCP `initialize` frame, when fed to `serve-stdio` via a string-input-stream, then the response matches `test/mcp/fixtures/initialize-response.json` modulo the server-version field and the timestamp field.
- Given the handshake fixture has been replayed, when followed by a `tools/list` request, then the response contains exactly the four tools declared in S3.
- Given the handshake fixture has been replayed, when followed by a `resources/list` request, then the response contains exactly the eight resources declared in S4–S6.
- Given the test runs in a fresh SBCL subprocess (per INV-4), when it completes, then the pass count delta from the pre-slice baseline is exactly the number of new assertions added in this slice (Tactician to predict a *range*, not a tight count, per the slice-008 calibration lesson).

## Quality Criteria

- [ ] `(asdf:load-system "org.melusina.atelier/mcp")` in a fresh SBCL produces zero warnings, zero style warnings, zero notes.
- [ ] Full regression `(asdf:test-system "org.melusina.atelier")` plus `(asdf:test-system "org.melusina.atelier/mcp")` passes in a fresh SBCL subprocess (per **INV-4**).
- [ ] No tool handler can crash the server: every handler is wrapped in a `handler-case` that converts conditions to JSON-RPC error results.
- [ ] No resource reader can crash the server: same wrapping.
- [ ] Transcript entries are written atomically per entry: `prin1` + `terpri` + `finish-output`. A `kill -9` mid-session leaves the file readable up to the last fully-written entry.
- [ ] No new external dependency beyond what is needed for JSON encoding/decoding. The Tactician will pin the JSON library during the OSS check stage; preference order is (a) something already in the Atelier dependency tree, (b) `jzon`, (c) `com.inuoe.jzon`, (d) `cl-json`. Hunchentoot, cl-who, and any HTML library are explicitly excluded.
- [ ] No web server, no listening socket, no network port. Stdio only.
- [ ] No secret storage, credential handling, or SSH key management of any kind. (See backlog rejection R6.)
- [ ] **INV-11 audit:** templates under `resource/template/` are grepped for any reference to symbols this slice introduces. None expected, but the grep is a slice-closure step.

## Definition of Ready

- [x] Stories traceable to backlog items — slice maps to backlog item #5
- [x] Stories sized ≤ 2 days each — eight stories, each scoped to one subsystem
- [x] Acceptance criteria written for all stories
- [x] Quality criterion defined
- [x] Spec references identified — MCP spec, Python prototype, Atelier registries, ASDF source registry

## Definition of Done

- [ ] All eight stories complete with acceptance criteria passing
- [ ] Quality criteria passing
- [ ] Full test suite passes in fresh SBCL subprocess (per INV-4)
- [ ] Manual MCP-client smoke test recorded: launch `(atelier/mcp:serve-two-way-stream)` from an MCP client, observe successful handshake, list tools, list resources, invoke `atelier:probe-environment`, read `atelier://inspectors`, read `lisp://transcript/<session-id>.md`
- [ ] All implementation phases have completion notes
- [ ] `product/slice/009-mcp-skeleton/retrospective.md` created
- [ ] `product/backlog.md` updated (item #5 marked delivered for the skeleton; new items added for the slice 010–016 track; rejected items recorded; under-consideration items recorded)
- [ ] `product/roadmap.md` updated (009 to Now → Completed at slice closure; 010 to Now)
- [ ] `CLAUDE.md` updated with a brief MCP server section pointing at `atelier/mcp:serve-stdio` as the entry point
- [ ] No new template under `resource/template/` references the new package nickname (verified by grep)

---

## Notes for the Tactician

**Historical — superseded by `implementation-1.md`.** The planning interview resolved all eight questions below. Key outcomes: tool naming is **colon-separated** (`atelier:ping`, not `atelier.ping`) with the `/mcp` package suffix stripped when deriving names from symbols; JSON library is **`com.inuoe.jzon`**; MCP spec version **`"2024-11-05"`**; transcript file location is **`(uiop:xdg-state-home "atelier/mcp/transcripts/")`**; `image-connection` ships the three minimal generics only, with `uiop:process-info` composition for slice 010; the unified `define-tool` macro handles tool, resource, and dual-exposed definitions via an optional `(:resource ...)` option. Retained below as slice history.

Several questions need answering during the planning interview that I deliberately did **not** pin in the slice scope:

1. **MCP spec version.** Pin the exact spec version we target. Read the spec, capture the JSON-Schema for `initialize`, `tools/*`, `resources/*` into `references/mcp-protocol.md`. The spec evolves; lock to a version and stick to it for the slice.
2. **JSON library choice.** Run the OSS check stage. Preference order is in the quality criteria. Confirm whether the chosen library handles bidirectional encoding without surprises (symbol case, plist-vs-alist, true/false/null, integer ranges).
3. **Read the Python prototype.** `/Users/michael/Workshop/lisp_mcp/sbcl_eval.py`. Extract: tool naming conventions, the JSON-RPC envelope shape it uses, error-result vs. error-protocol distinction, child-image lifecycle pattern (relevant for slice 010 even though out of scope here), and any gotchas. Write the notes into `references/python-prototype-notes.md` before designing the tool registry. **Do not transliterate** — the Python implementation is a starting point, not a contract; if its choices conflict with idiomatic CL or with Atelier's architecture, override them and record the override in the notes.
4. **Stdio framing detail.** MCP framing for stdio is line-delimited JSON-RPC, but verify against the spec. Some MCP transports use length-prefixed frames; this matters for the read loop in `serve-stdio`.
5. **Transcript file location.** I proposed `(uiop:xdg-cache-home "atelier/mcp/transcripts/")`. Confirm this is the right XDG bucket — *cache* vs *state* matters. State (`xdg-runtime-dir` or `xdg-state-home`) is more correct if the user expects transcripts to survive a cache eviction. **My instinct is `xdg-state-home` is the right choice** but the Tactician should verify against the XDG spec and choose deliberately.
6. **`image-connection` protocol shape.** I proposed three generics: `connection-eval`, `connection-shutdown`, `connection-alive-p`. The Tactician should think one slice ahead and ask: what *else* will slice 010 need? If `connection-interrupt`, `connection-trace`, or `connection-list-threads` are predictable now, define them now as `not-implemented` generics so slice 010 only adds methods, never changes the protocol.
7. **Tool naming convention.** I used `atelier.ping`, `atelier.list-inspectors`, etc. Pin the convention: dot-separated namespace, lower-case, hyphens for word separation, the MCP server's name as the root namespace. Subsequent slices follow this so tools can be filtered/searched by namespace prefix (`atelier.*`, `lisp.*`, `confidence.*`).
8. **Slice 010 transport peek.** Slice 010 will spawn child SBCL processes and connect to in-image SWANK over a `socketpair(2)` (AF_UNIX). The Tactician does not need to design this in slice 009, but **must** ensure the `image-connection` abstraction does not bake assumptions that conflict with that plan — in particular, the abstraction must not assume TCP, must not assume a port, and must not assume that connection setup is asynchronous.

There are no architectural decisions on the `slice.md` critical path that the Strategist has not already made: Model A (standalone binary), socketpair for slice 010 (not 009), SWANK as the in-image backend (not 009), sexp-canonical transcript with derived JSON and Markdown views, no Hunchentoot, no secret management, no SQLite. The Tactician's job is to flesh out the plumbing decisions above and produce a step-table for phase 1.

This slice is a foundation slice. Its biggest risk is **over-building**: producing infrastructure that anticipates needs not yet driven by stories. Per `product/knowledge/patterns.md` ("Hallucinated requirements from surface intuition"), every new class slot, every new generic function, every new exported symbol must point at an acceptance criterion in this slice. If it cannot, drop it. Slice 010 will add what slice 010 needs.
