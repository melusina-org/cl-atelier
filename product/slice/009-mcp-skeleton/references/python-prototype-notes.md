# Python Prototype Notes — `sbcl_eval.py`

**Source:** `/Users/michael/Workshop/lisp_mcp/sbcl_eval.py`
**Version read:** 675 lines, read during slice 009 Tactician stage 2 (2026-04-10).
**Status:** read as reference, not copied. Override log below.

## Purpose of this file

The Python prototype is a working SWANK MCP server used by the maintainer against a running SBCL image. It is the only existing reference implementation of "MCP for Common Lisp" known at the time of slice 009. This file records the conventions the Tactician extracted and every deliberate divergence the Atelier implementation makes from it.

## Conventions inherited

1. **Stdio framing.** Line-delimited JSON, `\n` terminator, `read-line` / `print`-line loop. Confirmed by reading the MCP 2024-11-05 spec; no divergence.

2. **JSON-RPC envelope shapes.** The prototype uses the standard JSON-RPC 2.0 shapes documented in `mcp-protocol.md`. No divergence.

3. **`initialize` response.** The prototype returns:
   ```python
   {"protocolVersion": "2024-11-05",
    "capabilities": {"tools": {}},
    "serverInfo": {"name": "sbcl-eval", "version": "1.0.0"}}
   ```
   Atelier returns the same structure with explicit `false` values for `listChanged` and `subscribe`, and adds a `resources` capability (the Python prototype implements only tools, not resources).

4. **`tools/call` result envelope.** The prototype returns `{"content": [{"type": "text", "text": <str>}], "isError": bool}`. Atelier does the same.

5. **Method-not-found error.** The prototype returns `-32601`. Atelier does the same.

6. **In-band vs. protocol error distinction.** The prototype uses protocol errors for "unknown method" and in-band errors for "tool ran and failed". This matches the MCP spec. Atelier inherits this wholesale.

7. **Output serialization lock.** The prototype holds a `threading.Lock` around every `sys.stdout.write`. Atelier will hold a `bordeaux-threads:lock` around every response write for the same reason, even though slice 009 is single-threaded — slice 010 will make concurrent tool dispatch possible, and the discipline is cheap to start with.

8. **`notifications/initialized` returns no response.** The prototype returns `None` for notifications. Atelier does the same (NIL from `handle-message`).

9. **Unknown notifications are silently dropped.** The prototype returns `None` for any request without an `id`. Atelier does the same.

## Overrides from the prototype

Every deviation is recorded with the reason.

### Override 1: colon-separated tool names, not snake_case

- **Prototype:** `eval`, `describe`, `apropos`, `invoke_restart`, `abort`, `backtrace` — bare snake_case.
- **Atelier:** `atelier:eval`, `atelier:describe`, etc. — namespaced by MCP server name, colon-separated to match CL's `package:symbol` convention, kebab-case inside the name.
- **Reason:** Atelier will host tools from multiple subsystems (linter, formatter, documentation, CFFI diagnostics) in future slices. Namespacing prevents collisions between `atelier:eval` and (say) `confidence:eval` and makes filter queries like "show me all atelier tools" trivial.

### Override 2: `resources/list` and `resources/templates/list`

- **Prototype:** implements only `tools/list` and `tools/call`. No resources at all. The dispatcher stub at line 499 does not handle `resources/list` or `resources/read`.
- **Atelier:** ships resources as a first-class primitive in slice 009. Concrete and templated resources are split across the two MCP methods per spec.
- **Reason:** resources are how an agent retrieves Atelier state cheaply (read-only, cacheable, no side effects) — inspector registry, maintainer registry, ASDF systems, session transcript. Tools are for actions. The split lets an MCP client show the user "what data is available" (resources UI picker) separately from "what actions the agent can take" (tool call confirmation).

### Override 3: SWANK client is NOT part of slice 009

- **Prototype:** 200+ lines of SWANK client (sockets, hex length-prefix framing, reader loop, per-call id queue, debugger state tracking, timeout cleanup via `swank:throw-to-toplevel`).
- **Atelier slice 009:** zero SWANK code. The `image-connection` abstract class exists as the extension point, but no concrete subclass is loaded. Slice 010 will add `swank-connection` over a `socketpair(2)` — NOT over TCP as the prototype does.
- **Reason:** slice 009 is the MCP framing and tool/resource registry. Adding SWANK would pull in a child image, a socket, timeouts, debugger state plumbing — that is the entire slice 010 surface area. Foundation slices must not leak into feature slices.

### Override 4: TCP SWANK → socketpair SWANK (deferred to slice 010)

- **Prototype:** `socket.AF_INET`, port 4005, TCP. The prototype spawns its own SBCL or assumes one is already running; either way it talks TCP to `localhost:4005`.
- **Atelier slice 010 plan:** `AF_UNIX socketpair(2)` — bidirectional unnamed pipe. No TCP port, no listening socket, no network stack involved. The parent-child pair is established at `fork(2)`-time via inheritance of the socketpair fd.
- **Reason:** matches the user's explicit preference ("avoid a network server"), avoids port conflicts, avoids firewall surfaces, and makes the connection private to the parent-child process tree. The architectural abstraction `image-connection` in slice 009 is signature-compatible with either transport; slice 010 picks socketpair.

### Override 5: No per-tool `timeout` argument in slice 009

- **Prototype:** every tool accepts an optional `timeout` argument (seconds). The SwankClient enforces the budget and interrupts the REPL thread on overrun.
- **Atelier slice 009:** zero timeout arguments. Slice 009 tools (`probe-environment`, `list-inspectors`, etc.) are all pure registry walks with O(1) or O(N) bounded work. There is no long-running computation to time out.
- **Reason:** timeouts belong to the eval surface, not the tool registry. Slice 010 will re-introduce `:timeout` as a lambda-list parameter on the eval tool specifically. Slice 009 keeps the macro surface clean.

### Override 6: Transcript subsystem

- **Prototype:** no transcript. Uses Python's `print`/`sys.stderr.write` for debugging.
- **Atelier slice 009:** sexp-canonical transcript with JSON and Markdown views, persisted under `(uiop:xdg-state-home "atelier/mcp/transcripts/")`, exposed as three resources.
- **Reason:** agents want replay and audit. The transcript is the single most load-bearing piece of state for post-hoc inspection of a session.

### Override 7: CLOS class hierarchy for messages

- **Prototype:** a flat `if method == '...'` chain in `MCPServer._handle` (lines 500–546). ~50 lines for 4 methods.
- **Atelier slice 009:** class hierarchy with `handle-message` generic, `:around` methods for policy hooks, `:after` method for transcript writing.
- **Reason:** transcript audit, policy hooks, response back-links — all three are `defmethod` one-liners in the CLOS design and would require explicit threading in a flat dispatcher. This is the one place the Lisp version is strictly larger than the Python, and deliberately so.

### Override 8: Hash-table registry, not dict-per-method

- **Prototype:** tools declared as a `TOOLS` list of dicts at line 357. Lookups use `for tool in TOOLS if tool['name'] == name`.
- **Atelier slice 009:** hash-table `*tool-registry*` keyed by tool name string. O(1) lookup.
- **Reason:** matches Atelier's existing inspector/maintainer registries (consistency), scales as slices add more tools, and `(gethash name *tool-registry*)` reads naturally in CL.

### Override 9: No Swank output-grabbing in slice 009

- **Prototype:** uses `swank:eval-and-grab-output` to capture stdout/stderr from the evaluated form. Returns `(output value)`.
- **Atelier slice 009:** no eval, so no output capture. Slice 010 will reintroduce output capture via the `image-connection` transport; the prototype's approach (wrap the form in `swank:eval-and-grab-output`) is the reference.

## Conventions considered and rejected

### The `:presentation-start` / `:presentation-end` message handling

- **Prototype:** ignores them at line 242 (`# :write-string, :presentation-start, etc. are ignored`).
- **Atelier decision:** also ignore in slice 010 initially. SLIME's presentation system is a client-side richness feature; MCP clients don't render it, so there's nothing to pass through.

### TCP port fallback / auto-detect

- **Prototype:** hardcoded port 4005 with a CLI override.
- **Atelier decision:** not applicable (socketpair, not TCP). No port to detect.

## Useful patterns to revisit in slice 010

When slice 010 adds the SWANK client, these prototype patterns should be revisited:

1. **Reader loop structure** (lines 200–242). Per-call id dispatch via a `dict` keyed by id with a queue per pending call. Atelier version: per-call `bordeaux-threads:condition-variable` keyed by id in a hash-table.

2. **Debugger entry sentinel** (lines 140–152). The prototype places a `_DebugEntered` sentinel in every pending call's queue when the image enters the debugger. Atelier version: signal a `debugger-entered` condition in every pending call.

3. **Timeout cleanup** (lines 294–317). On timeout: send `(:emacs-interrupt :repl-thread)` then `(swank:throw-to-toplevel)` with a fresh call id, don't wait long for it. Atelier version: same logic, same swank forms, different CL syntax.

4. **Debugger state formatting** (lines 126–135). The prototype renders the debugger state as "level N: condition\n\nRestarts:\n  0: [...]\n\nBacktrace (top frames):\n  N: frame". Atelier version: same output in the condition's `print-object` method.

5. **`swank:apropos-list-for-emacs`** usage (lines 591–607). Extracts `:designator` and `:documentation` from the plist. Atelier version: same.

## Commit log

- 2026-04-10 — filled in from reading the full prototype. All conventions and overrides recorded. Reference is complete for slice 009; slice 010 will extend the useful-patterns section with implementation notes as the SWANK client takes shape.
