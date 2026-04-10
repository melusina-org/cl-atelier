# MCP Protocol Reference — pinned to 2024-11-05

**Authority:** this document. Atelier code that handles MCP messages must conform to the shapes and rules below, not to assumptions from the Python prototype or from other MCP implementations.

**Spec revision:** `"2024-11-05"`
**Source pages consulted (2026-04-10):**
- `https://modelcontextprotocol.io/specification/2024-11-05/basic` — overview
- `https://modelcontextprotocol.io/specification/2024-11-05/basic/lifecycle` — initialize handshake
- `https://modelcontextprotocol.io/specification/2024-11-05/basic/transports` — stdio framing
- `https://modelcontextprotocol.io/specification/2024-11-05/server/tools` — tools/list and tools/call
- `https://modelcontextprotocol.io/specification/2024-11-05/server/resources` — resources/list, resources/templates/list, resources/read

All claims in this document were verified by direct reading of the spec pages on the date above. If a future slice needs to target a different spec version, it cuts a new reference document and a new constant `+mcp-protocol-version+`.

## Transport: stdio framing

Atelier ships only the stdio transport for slice 009. HTTP+SSE is out of scope permanently for this slice; if it becomes needed later, it is its own slice.

**Framing rules, verbatim from spec:**

- Messages are delimited by newlines.
- Messages **MUST NOT** contain embedded newlines.
- The server **MUST NOT** write anything to `stdout` that is not a valid MCP message.
- The server **MAY** write UTF-8 strings to `stderr` for logging. The client **MAY** capture, forward, or ignore them.
- Clients launch the server as a subprocess. `stdin` carries client-to-server messages; `stdout` carries server-to-client messages.

**Atelier interpretation:** one complete JSON-RPC 2.0 message per line, terminated by `#\Newline`. The read loop in `serve-two-way-stream` uses `read-line`. The write loop calls `encode-to-string`, writes the line, writes `#\Newline`, and `finish-output`. Because JSON has no native newline escape at the message level (strings inside JSON can contain `\n` but are encoded as `\\n`), the "no embedded newlines" rule is automatically satisfied by jzon's compact output mode.

**Slice 009 does not write to `stderr`.** The transcript subsystem is the only logging surface. If slice 010 or later wants to emit stderr diagnostics, it adds a discriminator on a new parameter, not by repurposing `stdout`.

**Shutdown:** per spec, the client initiates shutdown by closing its end of `stdin`. `read-line` returns NIL on EOF, and `serve-two-way-stream`'s loop terminates. No explicit shutdown message is defined.

## JSON-RPC envelope shapes

### Request

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": { ... }
}
```

Required fields: `jsonrpc` (MUST be `"2.0"`), `id` (integer or string; MUST be present for requests), `method` (string).
Optional: `params` (object). Absent `params` means no arguments.

### Successful response

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": { ... }
}
```

`result` is always an object. Its shape depends on the method and is defined by the per-method schema below.

### Error response

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32601,
    "message": "Method not found",
    "data": { ... }
  }
}
```

`error.code` (integer) and `error.message` (string) are required; `error.data` (arbitrary) is optional.

### Notification

```json
{
  "jsonrpc": "2.0",
  "method": "notifications/initialized",
  "params": { ... }
}
```

Notifications have **no `id`** and MUST NOT receive a response. Atelier distinguishes notifications from requests using the three-state key extraction pattern documented in `jzon-round-trip.md`: `(gethash "id" obj :missing)` returns `:missing` for notifications, a value for requests, and the symbol `cl:null` for the (invalid) case where `id` is JSON null.

## Error codes

**Standard JSON-RPC 2.0 codes:**

| Code | Name | Atelier usage |
|---|---|---|
| `-32700` | Parse error | Invalid JSON on the stdio stream |
| `-32600` | Invalid Request | JSON parsed but does not match envelope shape (e.g. missing `jsonrpc` or `method`; `id` is JSON `null`) |
| `-32601` | Method not found | Unknown MCP method string |
| `-32602` | Invalid params | Known method but malformed params (wrong shape or types); also used for unsupported `protocolVersion` in initialize |
| `-32603` | Internal error | Uncaught condition inside a handler; set by the `*debugger-hook*` to prevent stdio hang |

**MCP-specific codes** (not in JSON-RPC 2.0, defined by the spec):

| Code | Name | Atelier usage |
|---|---|---|
| `-32002` | Resource not found | `resources/read` on an unknown URI. **Never used for tool-call errors** — those go in-band. |

**Tool execution errors** are in-band: `result.isError = true` with the error text in `result.content[0].text`. They are **not** JSON-RPC protocol errors. A tool that signals a condition is caught by the dispatcher's `:around` method, converted to `isError: true`, and the server continues. The only time a tool-call emits a JSON-RPC error is `-32601` (no such tool) or `-32602` (invalid arguments); once the handler runs, any further failure is in-band.

## Initialize handshake

### `initialize` request (client → server)

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "protocolVersion": "2024-11-05",
    "capabilities": {
      "roots": { "listChanged": true },
      "sampling": {}
    },
    "clientInfo": {
      "name": "ExampleClient",
      "version": "1.0.0"
    }
  }
}
```

Required params: `protocolVersion`, `capabilities`, `clientInfo`.

### `initialize` response (server → client)

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "protocolVersion": "2024-11-05",
    "capabilities": {
      "tools":     { "listChanged": false },
      "resources": { "subscribe": false, "listChanged": false }
    },
    "serverInfo": {
      "name":    "org.melusina.atelier/mcp",
      "version": "0.1.0"
    }
  }
}
```

**Slice 009 capability declaration:** `tools` and `resources` both declared. Neither `listChanged` nor `subscribe` is supported. Values are explicit `false`, not omitted, to keep the declaration unambiguous.

**`serverInfo.name`** is the ASDF system name, not the package name. **`serverInfo.version`** is `"0.1.0"` for slice 009 and will bump with each released slice.

### Version negotiation rule (verbatim from spec)

> If the server supports the requested protocol version, it **MUST** respond with the same version. Otherwise, the server **MUST** respond with another protocol version it supports. This **SHOULD** be the *latest* version supported by the server. If the client does not support the version in the server's response, it **SHOULD** disconnect.

**Slice 009 behavior:** only supports `"2024-11-05"`. If the client requests that version, reply with it. If the client requests anything else, reply with `"2024-11-05"` anyway. Client decides whether to disconnect.

Alternative (considered, rejected for slice 009): return `-32602` with `{"supported": ["2024-11-05"], "requested": X}` when versions don't match. Rejected because the spec's "server MUST respond with another version it supports" language is the canonical path; the `-32602` example in the spec is for a stricter server implementation. Atelier follows the canonical path.

### `notifications/initialized` (client → server)

```json
{
  "jsonrpc": "2.0",
  "method": "notifications/initialized"
}
```

No `id`, no `params`, no response. Marks the client as ready for operation-phase requests. Atelier's dispatcher consumes this silently and does nothing — it's a state marker only.

## Tools

### `tools/list` request (client → server)

```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "tools/list",
  "params": {
    "cursor": "optional-cursor-value"
  }
}
```

`params.cursor` is the pagination cursor. **Slice 009 ignores cursors.** With 6 tools we do not paginate. The response always contains the full list and no `nextCursor`.

### `tools/list` response (server → client)

```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {
    "tools": [
      {
        "name":        "atelier:list-inspectors",
        "description": "List all registered Atelier inspectors with their finding class and severity.",
        "inputSchema": {
          "type": "object",
          "properties": {}
        }
      }
    ]
  }
}
```

Each tool entry: `name` (string), `description` (string), `inputSchema` (JSON Schema object with at minimum `"type": "object"`).

### `tools/call` request (client → server)

```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "tools/call",
  "params": {
    "name": "atelier:probe-environment",
    "arguments": {}
  }
}
```

`params.name` (string, required), `params.arguments` (object, optional — absent = no arguments).

### `tools/call` response (server → client)

Successful:

```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "result": {
    "content": [
      { "type": "text", "text": "..." }
    ],
    "isError": false
  }
}
```

Tool-execution error (in-band):

```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "result": {
    "content": [
      { "type": "text", "text": "No such inspector: foo" }
    ],
    "isError": true
  }
}
```

`result.content` is an array of content items. Slice 009 emits only `{"type": "text", "text": ...}` items. The `image` and `resource` content types from the spec are not used.

**Atelier encoding rule for tool results:** handlers return Lisp data. The dispatcher:
1. Applies `alist-to-json-object` / `plist-to-json-object` recursively to turn the data into a hash-table tree.
2. Calls `jzon:stringify` on the tree to produce the JSON text.
3. Wraps the text in a single `{"type": "text", "text": <stringified>}` content item.
4. Sets `isError: false`.

If the handler signals, the dispatcher's `:around` method catches it, formats it as a condition-printed string, wraps that in a text content item, and sets `isError: true`.

### Unknown tool

```json
{ "jsonrpc": "2.0", "id": 3, "error": { "code": -32601, "message": "Unknown tool: foo" } }
```

**This is a protocol error**, not in-band. Unknown tool means "your method dispatch was wrong."

## Resources — concrete and templated are DIFFERENT METHODS

**This was the most important finding of the spec fetch and resulted in a plan amendment.** Concrete (fixed-URI) resources and URI-templated resources are listed via distinct methods:

| Method | Returns | Atelier count in slice 009 |
|---|---|---|
| `resources/list` | Concrete resources with fixed `uri` | **3** |
| `resources/templates/list` | URI templates with `uriTemplate` | **5** |

The `resources/read` method handles both — a URI that matches a template is extracted and the reader is invoked with the bindings.

### Slice 009 resource split

**Concrete (3):**

| URI | Tool that defines it | MIME type |
|---|---|---|
| `atelier://inspectors` | `list-inspectors` (dual-exposed) | `application/json` |
| `atelier://maintainers` | `list-maintainers` (dual-exposed) | `application/json` |
| `lisp://systems` | `list-systems` (dual-exposed) | `application/json` |

**Templated (5):**

| URI template | Tool that defines it | MIME type |
|---|---|---|
| `atelier://inspectors/{name}` | `inspector-detail` (dual-exposed) | `application/json` |
| `atelier://maintainers/{name}` | `maintainer-detail` (dual-exposed) | `application/json` |
| `lisp://transcript/{session-id}.sexp` | `transcript-sexp` (resource only) | `text/plain` |
| `lisp://transcript/{session-id}.json` | `transcript-json` (resource only) | `application/json` |
| `lisp://transcript/{session-id}.md` | `transcript-markdown` (resource only) | `text/markdown` |

### `resources/list` request and response

```json
{ "jsonrpc": "2.0", "id": 4, "method": "resources/list", "params": {} }
```

```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "result": {
    "resources": [
      { "uri": "atelier://inspectors",  "name": "Atelier inspector registry",  "description": "...", "mimeType": "application/json" },
      { "uri": "atelier://maintainers", "name": "Atelier maintainer registry", "description": "...", "mimeType": "application/json" },
      { "uri": "lisp://systems",        "name": "ASDF systems",                "description": "...", "mimeType": "application/json" }
    ]
  }
}
```

Entry fields: `uri` (required), `name` (required), `description` (optional), `mimeType` (optional but recommended).

### `resources/templates/list` request and response

```json
{ "jsonrpc": "2.0", "id": 5, "method": "resources/templates/list", "params": {} }
```

```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "result": {
    "resourceTemplates": [
      {
        "uriTemplate": "atelier://inspectors/{name}",
        "name": "Atelier inspector detail",
        "description": "...",
        "mimeType": "application/json"
      },
      {
        "uriTemplate": "atelier://maintainers/{name}",
        "name": "Atelier maintainer detail",
        "description": "...",
        "mimeType": "application/json"
      },
      {
        "uriTemplate": "lisp://transcript/{session-id}.sexp",
        "name": "MCP session transcript (sexp)",
        "description": "...",
        "mimeType": "text/plain"
      },
      {
        "uriTemplate": "lisp://transcript/{session-id}.json",
        "name": "MCP session transcript (JSON)",
        "description": "...",
        "mimeType": "application/json"
      },
      {
        "uriTemplate": "lisp://transcript/{session-id}.md",
        "name": "MCP session transcript (Markdown)",
        "description": "...",
        "mimeType": "text/markdown"
      }
    ]
  }
}
```

The spec cites RFC 6570 for URI templates. Slice 009 uses a tiny subset: single-segment level-1 expressions `{name}`. No reserved expansions, no path expansions, no query expansions.

### `resources/read` request and response

```json
{ "jsonrpc": "2.0", "id": 6, "method": "resources/read", "params": { "uri": "atelier://inspectors/check-earmuffs" } }
```

```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "result": {
    "contents": [
      {
        "uri":      "atelier://inspectors/check-earmuffs",
        "mimeType": "application/json",
        "text":     "{\"name\":\"check-earmuffs\",...}"
      }
    ]
  }
}
```

`result.contents` is an array of content items. Each has `uri` (required — the actual URI that was read, not the template), `mimeType` (optional), and either `text` (string) or `blob` (base64). Slice 009 only emits `text` items.

**URI matching order:** the dispatcher first looks in the concrete registry, then in the template registry. On template match, the extracted bindings become the keyword arguments to the handler's `&key` lambda list.

**Unknown URI:**

```json
{ "jsonrpc": "2.0", "id": 6, "error": { "code": -32002, "message": "Resource not found", "data": { "uri": "atelier://inspectors/nonexistent" } } }
```

`-32002` is the MCP-specific code for resource-not-found. **Note:** for `resources/read`, "no such URI" is a protocol error, NOT in-band. This contrasts with tool handlers, where runtime failures are in-band. The rationale is that the client sent an unknown URI — it is a client-side error in addressing, not a server-side failure in reading.

**However**, a *reader* that fails at runtime (e.g., `inspector-detail` called with a valid template match but the inspector in question has been unregistered since list-time) returns an in-band error result the same way `tools/call` does: `contents[0].text = "error text"` with an explicit `isError` field. Wait — the spec doesn't define `isError` on `resources/read`. Atelier policy: if a reader signals at runtime, the dispatcher returns `-32603` internal error. The Reviewer will flag this at audit if a better path is found.

## Methods Atelier implements in slice 009

| Method | Direction | Handler class | Notes |
|---|---|---|---|
| `initialize` | request | `initialize-request` | Always replies with `"2024-11-05"` |
| `notifications/initialized` | notification | `initialized-notification` | No-op, no response |
| `tools/list` | request | `tools-list-request` | Returns 6 tools |
| `tools/call` | request | `tools-call-request` | Dispatches to `handle-tool-call` via CLOS |
| `resources/list` | request | `resources-list-request` | Returns 3 concrete resources |
| `resources/templates/list` | request | `resources-templates-list-request` | Returns 5 templates |
| `resources/read` | request | `resources-read-request` | Matches URI against concrete + templates |

**Seven methods.** (Originally plannedas six; the split between `resources/list` and `resources/templates/list` adds the seventh.)

## Methods Atelier does NOT implement in slice 009

- `ping` (utility) — not required
- `prompts/*` (server feature) — no prompts declared in capabilities
- `logging/*` (server feature) — no logging declared in capabilities
- `resources/subscribe`, `resources/unsubscribe`, `notifications/resources/updated` — `subscribe: false` declared
- `notifications/tools/list_changed`, `notifications/resources/list_changed` — `listChanged: false` declared
- `completion/complete` — no completion support
- `sampling/*` — client-side, not server-side
- `roots/*` — client-side, not server-side

Any request for an unimplemented method returns `-32601` method-not-found.

## Capability negotiation reference table

Slice 009's server capabilities:

```json
"capabilities": {
  "tools":     { "listChanged": false },
  "resources": { "subscribe": false, "listChanged": false }
}
```

Missing categories (`prompts`, `logging`, `experimental`) mean Atelier does not support those features at all. The client MUST NOT send requests for unsupported features; if it does, it gets `-32601`.

## Commit log

- 2026-04-10 — initial draft. Spec fetched and verified. **Plan amendment:** added `resources/templates/list` as a distinct method; split resource count from 8-in-one-method to 3+5-in-two-methods. See `implementation-1.md` "Plan amendments" section.
