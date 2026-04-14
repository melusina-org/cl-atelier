# MCP Kernel Boundary — File Assignment

## Design Rationale

The kernel (`org.melusina.atelier/mcp-kernel`) is a reusable MCP server
framework. Any MCP server can be built on it by providing a concrete
`image-connection` subclass. The transport layer (SWANK, transcript,
HyperSpec, concrete tools) lives in `org.melusina.atelier/mcp`.

The boundary criterion: **kernel has no knowledge of SWANK, TCP sockets,
child processes, or Atelier-specific domain concepts.** It provides the
JSON-RPC dispatch loop, tool/resource registry, message parsing, and the
abstract connection contract.

## Kernel Files

| File | Reason |
|------|--------|
| `package.lisp` (kernel section) | Package definition for `atelier/mcp-kernel` |
| `conditions.lisp` | Protocol-level conditions (tool-not-found, etc.) |
| `json-util.lisp` | JSON encoding/decoding — protocol layer |
| `protocol-version.lisp` | MCP version constant |
| `tool-name.lisp` | Tool name derivation — registry mechanics |
| `input-schema.lisp` | JSON Schema from lambda lists — registry mechanics |
| `uri-template.lisp` | URI template parsing/matching — resource dispatch |
| `tool.lisp` | Tool class, 3 registries, find-tool-by-name |
| `message.lisp` | MCP message classes, parse-mcp-message |
| `define-tool.lisp` | `define-tool` macro — tool registration |
| `dispatcher.lisp` | `handle-message` methods — protocol dispatch |
| `image-connection.lisp` | Abstract `image-connection`, 3 generics |
| `server.lisp` | `mcp-server` class, serve-two-way-stream, ensure-child-connection |
| `tools/reload-server.lisp` | Calls `asdf:load-system :force t` — pure ASDF, no transport |

## MCP Files (transport + domain)

| File | Reason |
|------|--------|
| `package.lisp` (mcp section) | Package `atelier/mcp` `:use`s kernel, adds transport exports |
| `child-connection.lisp` | Concrete subclass — SWANK spawn, TCP connect |
| `swank-protocol.lisp` | SWANK wire protocol client — transport |
| `transcript.lisp` | Session logging — not protocol-essential |
| `transcript-render.lisp` | Transcript formatting — not protocol-essential |
| `hyperspec.lisp` | Atelier-specific domain knowledge |
| `tools/*.lisp` (except reload-server) | Concrete tools — domain-specific |

## Dependencies

```
mcp-kernel: alexandria, uiop, com.inuoe.jzon
mcp:        mcp-kernel, bordeaux-threads, usocket, flexi-streams,
            org.melusina.atelier, org.melusina.atelier/editor
```

## Connection Class/Initargs Pattern

Borrowed from Hunchentoot's acceptor pattern. The server holds the class
designator and initargs; `ensure-child-connection` calls `make-instance`:

```lisp
(apply #'make-instance
       (server-connection-class server)
       (server-connection-initargs server))
```

This means:
- Kernel never mentions `child-connection` or `make-child-connection`
- New transports provide a new `image-connection` subclass
- Server construction sets `connection-class` and `connection-initargs`
