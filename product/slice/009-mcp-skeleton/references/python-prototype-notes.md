# Python Prototype Notes — `sbcl_eval.py`

**Source:** `/Users/michael/Workshop/lisp_mcp/sbcl_eval.py`
**Status:** Placeholder — to be filled in by the Tactician during the slice 009 planning interview.

## Purpose of this file

The Python prototype is a working SWANK MCP server that the maintainer has been using to drive an AI agent against an SBCL image. It is the only existing reference implementation of "MCP for Common Lisp" we know of. The Tactician reads it during the planning interview to extract concrete decisions before designing the CL re-implementation.

## What to extract

- **Tool surface.** Names, parameters, return shapes. Does the prototype namespace tools? How does it handle multi-line forms?
- **JSON-RPC envelope.** Exact framing on stdio. Length-prefixed or line-delimited? How are notifications distinguished from requests?
- **Initialize handshake.** What capabilities does the prototype declare? What client capabilities does it negotiate against?
- **Error handling.** Distinction between JSON-RPC protocol errors (`-32xxx`) and tool-call result errors (in-band). The Python implementation almost certainly settled this and the convention is worth inheriting.
- **SWANK client.** How does the prototype talk to SWANK? Direct socket? SWANK's `:dedicated-output-stream`? What does it do with `:debug-activate`, `:return`, `:abort`, `:write-string`, `:read-string`?
- **Child image lifecycle.** Does it spawn SBCL itself, or assume an already-running image? Where is `SBCL_HOME` set? How does it pass the source registry to the child?
- **Transcript / logging.** Does it journal sessions? In what format?
- **Gotchas the prototype hit.** Anything in comments or `# TODO`/`# XXX` lines is gold.

## What NOT to inherit

- **Python idioms** that do not translate (async/await, decorators, dataclasses-as-protocol). Use idiomatic CL.
- **Anything that bakes a TCP assumption** into the SWANK transport. Slice 010 will use a `socketpair(2)`, not TCP.
- **Tool names that conflict with Atelier conventions.** Atelier uses dot-separated lowercase with hyphens (`atelier.list-inspectors`). If the prototype uses snake_case or slashes, override.
- **Any secret-management surface.** Slice 009 explicitly rejects this; if the prototype has it, drop it on transliteration.

## Override log

When the Tactician decides to deviate from a prototype choice, record the override here as one bullet per decision: *prototype did X, we do Y, because Z*. This is the durable record of why the CL implementation diverges.

- *(empty — to be filled in)*
