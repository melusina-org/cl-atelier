# MCP Message Hierarchy

**Authority:** this document. `src/mcp/message.lisp` and `src/mcp/dispatcher.lisp` must conform to the class diagram and dispatch rules here.

## Why CLOS

The JSON-RPC `method` string is the natural discriminator for dispatch. Two designs considered:

1. **Hash-table method dispatch** — `*method-handlers*` keyed by string, simple plain-function lookup.
2. **CLOS class hierarchy** — each method string maps to a class, `handle-message` generic dispatches on it.

Stage 3 of the Tactician interview chose (2) for three reasons, which this file makes concrete:

- **Structured audit trail.** The transcript logger is a `:after` method on `handle-message`. It sees typed objects, not raw JSON strings. Queries like "show me all tool calls" or "show me all debugger interactions" become `typecase` filters on the request class, not string parsing.
- **Policy hooks.** An `:around` method on `tools-call-request` can enforce lint-before-eval (slice 010), rate-limiting, or auth without touching the dispatcher core or individual tool handlers.
- **Response back-links.** Every `mcp-response` instance back-links to its originating request via a `request` slot. The transcript writes one entry per message-pair, not two independent entries.

## Class diagram

```
mcp-message                            [abstract base; slots: raw-json, timestamp]
├── mcp-request                        [abstract; slots: id, method, params]
│   ├── initialize-request
│   ├── tools-list-request
│   ├── tools-call-request
│   ├── resources-list-request
│   ├── resources-templates-list-request
│   └── resources-read-request
├── mcp-notification                   [abstract; slots: method, params]
│   └── initialized-notification
└── mcp-response                       [abstract; slots: id, request]
    ├── mcp-success-response           [adds: result]
    └── mcp-error-response             [adds: code, message, data]
```

**Seven request classes** after plan amendment 1. **One notification class.** **Two concrete response classes.**

## Slot contracts

| Slot | Class | Type | Purpose |
|---|---|---|---|
| `raw-json` | `mcp-message` | hash-table | The original parsed frame, preserved for transcript audit |
| `timestamp` | `mcp-message` | universal-time integer | When the message was parsed |
| `id` | `mcp-request`, `mcp-response` | integer or string | JSON-RPC id |
| `method` | `mcp-request`, `mcp-notification` | string | JSON-RPC method |
| `params` | `mcp-request`, `mcp-notification` | hash-table or NIL | JSON-RPC params object |
| `request` | `mcp-response` | `mcp-request` instance | Back-link to the originating request |
| `result` | `mcp-success-response` | hash-table | Response payload |
| `code` | `mcp-error-response` | integer | JSON-RPC error code |
| `message` | `mcp-error-response` | string | JSON-RPC error message |
| `data` | `mcp-error-response` | arbitrary | Optional error data |

Slots are read-only (no setters). Responses are constructed in one shot via `make-instance`.

## Method-to-class table

A hash-table populated at load time in `message.lisp`:

| Method string | Class | Direction |
|---|---|---|
| `"initialize"` | `initialize-request` | client → server |
| `"notifications/initialized"` | `initialized-notification` | client → server (notification) |
| `"tools/list"` | `tools-list-request` | client → server |
| `"tools/call"` | `tools-call-request` | client → server |
| `"resources/list"` | `resources-list-request` | client → server |
| `"resources/templates/list"` | `resources-templates-list-request` | client → server |
| `"resources/read"` | `resources-read-request` | client → server |

**Total: 7 entries.** Methods not in the table fall through to the base class and get `-32601` method-not-found.

## `parse-mcp-message`

The parser takes a hash-table from `jzon:parse` and produces a typed instance.

Rough shape:

```lisp
(defun parse-mcp-message (json)
  (let ((method (gethash "method" json))
        (id     (gethash "id" json :missing)))
    (unless (stringp method)
      (return-from parse-mcp-message
        (error 'invalid-request :reason "method field missing or not a string")))
    (let ((class (gethash method *method-to-class-table*
                          (if (eq id :missing)
                              'mcp-notification
                              'mcp-request))))
      (make-instance class
                     :raw-json json
                     :method method
                     :id (when (not (eq id :missing)) id)
                     :params (gethash "params" json)))))
```

**Three-state id extraction:**
- `id :missing` → notification (no response expected).
- `id = 'cl:null` → malformed request (spec says id MUST NOT be null in a request); dispatcher emits `-32600 Invalid Request`.
- `id = integer or string` → normal request.

## `handle-message` protocol

```lisp
(defgeneric handle-message (message server)
  (:documentation
   "Dispatch MESSAGE. Return an mcp-response for requests, NIL for notifications.
    Specialize on the concrete message class to add method-specific behavior.
    Standard method combination; :after writes to transcript; :around catches
    tool/resource handler conditions."))
```

**Primary methods shipped in slice 009:**

| Message class | What the method returns |
|---|---|
| `initialize-request` | `mcp-success-response` with server capabilities + serverInfo |
| `initialized-notification` | NIL (no response) |
| `tools-list-request` | Success response with the `tools` array built from `*tool-registry*` |
| `tools-call-request` | Success response with the content wrapping the handler's return, or in-band `isError: true` if the handler signalled |
| `resources-list-request` | Success response with the `resources` array built from `*concrete-resource-registry*` |
| `resources-templates-list-request` | Success response with the `resourceTemplates` array built from `*template-resource-registry*` |
| `resources-read-request` | Success response with the content wrapping the reader's return, or `-32002` if no registry match |
| `mcp-request` (fallback) | `mcp-error-response` with `-32601 Method not found` |
| `mcp-notification` (fallback) | NIL (unknown notifications are silently dropped per JSON-RPC 2.0 spec) |

**Shared methods (cross-class):**

- `:after (message mcp-message) server` → writes a transcript entry. Runs for every message regardless of class.
- `:around (message tools-call-request) server` → catches conditions from `handle-tool-call` and converts them to in-band tool-error results.
- `:around (message resources-read-request) server` → catches conditions from the reader function and converts them to internal-error protocol errors. The distinction from `tools-call-request` is that resource readers don't have an `isError` envelope; failures become protocol errors.

## Error policy (recap from `mcp-protocol.md`)

- **Protocol errors** (`-32xxx` in the JSON-RPC `error` envelope): parse error, invalid request, method not found, invalid params, internal error, resource not found.
- **Tool-execution errors** (`isError: true` in `result.content`): anything a tool handler signals at runtime. Caught by the `:around` method on `tools-call-request`.
- **Reader-execution errors** for resources: caught by the `:around` on `resources-read-request`, converted to `-32603 Internal error`. Slice 009 has no observable difference between "reader signalled" and "reader returned NIL" for unknown URIs because the lookup stage already filters unknowns to `-32002`.

## Debugger hook

`serve-two-way-stream` installs `*debugger-hook*` for the dynamic extent of the call. The hook catches any condition that escapes even the `:around` methods — primarily conditions inside the dispatcher itself, not inside handlers — and converts them to a last-resort `-32603 Internal error` response, logs the condition text to the transcript, and resumes the read loop.

```lisp
(defun make-mcp-debugger-hook (stream)
  (lambda (condition hook)
    (declare (ignore hook))
    (let ((response (make-internal-error-from-condition condition)))
      (write-response response stream))
    ;; Do not return to the debugger — throw back to the read loop.
    (throw 'continue-serving nil)))
```

The `throw 'continue-serving nil` target is a `catch` in `serve-two-way-stream`'s per-message dispatch body. Failed messages do not hang the server.

**Design alternative considered:** `handler-bind` with `invoke-restart` inside the dispatcher instead of `*debugger-hook*`. Rejected because the dispatcher's `:around` methods already catch the expected handler conditions; `*debugger-hook*` is the correct mechanism for the unexpected ones that escape all `handler-case` / `handler-bind` guards.

## Load order implications

The class hierarchy is in `src/mcp/message.lisp`. The `handle-message` generic is also in `message.lisp` (just the `defgeneric`). Primary methods are in `src/mcp/dispatcher.lisp`, which loads after `message.lisp`. This way, `src/mcp/tool.lisp` (which needs the request classes as slot types but does not dispatch on them) can load before `dispatcher.lisp`.

Load order within `src/mcp/`:
1. `package.lisp`
2. `conditions.lisp`
3. `protocol-version.lisp`
4. `json-util.lisp`
5. `tool-name.lisp` (helper)
6. `input-schema.lisp` (helper)
7. `uri-template.lisp` (helper)
8. `tool.lisp` — defines tool class, registries, `handle-tool-call` generic
9. `message.lisp` — defines the hierarchy and `handle-message` generic
10. `define-tool.lisp` — macro using tool, tool-name, input-schema, uri-template
11. `transcript-render.lisp`
12. `transcript.lisp`
13. `image-connection.lisp`
14. `dispatcher.lisp` — all `handle-message` primary methods
15. `server.lisp`
16. `tools/*.lisp`

## Test coverage

`testsuite/mcp/message-parsing.lisp` (step 22 of the plan) asserts:

1. Each method string in the table parses to its declared class.
2. A missing id produces a notification class.
3. A null id produces an invalid-request condition.
4. An unknown method string parses to `mcp-request` / `mcp-notification` base class.
5. `params` is correctly extracted and typed as hash-table or NIL.

`testsuite/mcp/dispatcher.lisp` (step 31) asserts:

1. Each primary method returns the correct response class.
2. `:after` transcript writer is called for every message (spy method on a subclass during the test).
3. `:around` on `tools-call-request` catches a signalled condition and produces `isError: true`.
4. `:around` on `resources-read-request` catches a condition and produces `-32603`.
5. The debugger hook installation is verified via a test that forces an uncaught condition and checks for continuation of the read loop.
