# `define-tool` Macro Design Notes

**Authority:** this document. The implementation in `src/mcp/define-tool.lisp` must conform to the rules stated here.

## Purpose

`define-tool` is the single entry point for registering anything an MCP client can reach in an Atelier MCP server: tools (invoked via `tools/call`), resources (read via `resources/read`), and dual-exposed definitions that are both. It feels like a `defmethod` with extra clauses.

One macro, three modes, selected by the shape of the form:

| Mode | Trigger | Registered in | Method dispatched to |
|---|---|---|---|
| Tool only | No `(:resource ...)` clause | `*tool-registry*` only | `tools/call` |
| Dual-exposed (concrete) | `(:resource :uri "foo://bar" ...)` with no `{...}` in URI | Both `*tool-registry*` and `*concrete-resource-registry*` | `tools/call` or `resources/read` |
| Dual-exposed (templated) | `(:resource :uri "foo://{x}" ...)` with `{...}` placeholders | Both `*tool-registry*` and `*template-resource-registry*` | `tools/call` or `resources/read` |

A **resource-only** mode (registered in the resource registry but with no tool name) is reachable by a fourth variant where the definition opts out of tool registration. Slice 009 uses this for the three transcript resources because they don't make sense as named tools. Trigger: an explicit `(:tool nil)` clause. Absence of `(:tool nil)` means the tool is also registered.

## Syntax

```
(define-tool NAME LAMBDA-LIST
  CLAUSE*
  BODY*)
```

`NAME` is a symbol. The tool-name string is derived from it:
- Home package → shortest nickname if any, else package name.
- Lowercased.
- If the result ends in `/mcp`, strip the `/mcp` suffix.
- Concatenate with `":"` and the lowercased symbol name.

Examples, with the symbol's home package in parentheses:

| Symbol | Home package | Derived tool name |
|---|---|---|
| `list-inspectors` | `atelier/mcp` | `"atelier:list-inspectors"` |
| `probe-environment` | `atelier/mcp` | `"atelier:probe-environment"` |
| `eval` | `my-project/mcp` | `"my-project:eval"` |
| `foo` | `org.melusina.atelier` (nickname `atelier`) | `"atelier:foo"` |
| `do-thing` | `raw-pkg` (no `/mcp`) | `"raw-pkg:do-thing"` |

`LAMBDA-LIST` is either empty `()` or a keyword-only lambda list `(&key p1 p2 ...)`. Required and optional positional parameters are forbidden — tool arguments are always named in MCP. The macro signals a compile-time error for any non-keyword parameter.

`CLAUSE*` are forms of the shape `(option-name ...)`. Supported options:

| Clause | Required | Purpose |
|---|---|---|
| `(:description "...")` | **yes** | Human-readable description. Surfaces in `tools/list` and `resources/list`. |
| `(:resource ...)` | no | Marks the definition as a resource. Nested options: `:uri`, `:name`, `:mime-type`, `:description`. |
| `(:tool nil)` | no | Opts out of tool registration. Only valid when `(:resource ...)` is also present. |

`(:resource ...)` nested options:

| Nested option | Required | Purpose |
|---|---|---|
| `:uri` | **yes** | URI template (with optional `{...}` placeholders). |
| `:name` | **yes** | Human-readable resource name. |
| `:mime-type` | **yes** | Keyword (`:application/json`, `:text/plain`, `:text/markdown`) or string. |
| `:description` | no | Defaults to the outer `(:description ...)` if absent. |

`BODY*` is the handler body. It is evaluated in a context where every keyword in the lambda list is bound as a local variable.

## Derived input schema

The input schema is derived from the lambda list at macro expansion time. No explicit `:input-schema` option.

- Empty lambda list `()` → `{"type": "object", "properties": {}}`
- `(&key name)` with a `(declare (type string name))` in the body → `{"type": "object", "properties": {"name": {"type": "string"}}, "required": ["name"]}`

Type declarations map to JSON Schema types:

| CL type | JSON Schema type |
|---|---|
| `string` | `"string"` |
| `integer` | `"integer"` |
| `(integer LO HI)` | `"integer"` with `minimum`/`maximum` |
| `real`, `number`, `float`, `double-float` | `"number"` |
| `boolean` or `(member t nil)` | `"boolean"` |
| `(member :a :b :c)` | `"string"` with `enum` |
| anything else | no property schema (the key is declared but its type is omitted) |

If a `&key` parameter has no type declaration, its schema is `{}` — present but unconstrained. Atelier's slice 009 tools either declare types or have empty lambda lists, so the "unconstrained" path is tested only via a helper unit test.

**Required fields:** every `&key` parameter is required. There are no optional keyword arguments in MCP tools — if it's optional, it's absent from the lambda list. This is a deliberate restriction: it keeps schemas simple and callers explicit.

## Compile-time validation rules

The macro signals compile-time errors (via `error` inside the macro body) for:

1. **Non-keyword parameters.** `(a b &key c)` is rejected. Only `()` or `(&key ...)` are accepted.
2. **`(:resource ...)` present but missing required nested options.** `:uri`, `:name`, `:mime-type` are all required.
3. **URI template `{placeholder}` set does not equal `&key` parameter set.** E.g. `(define-tool foo (&key name) (:resource :uri "x://{other}" ...))` is rejected because `{other}` has no matching `&key other`. Likewise `(define-tool foo (&key name other) (:resource :uri "x://{name}" ...))` is rejected because `other` has no corresponding placeholder.
4. **`(:tool nil)` without `(:resource ...)`.** Cannot opt out of tool registration for a definition that isn't a resource.
5. **Duplicate option clauses.** `(:description "a") (:description "b")` is rejected.
6. **Unrecognized top-level clauses.** `(:foo ...)` is rejected to catch typos.

These are raised as `error` from inside the macro, so `compile-file` fails with a legible message and the defining system does not load.

## Expansion target

Rough shape of the emitted forms for a dual-exposed templated definition:

```lisp
;; Source:
(define-tool inspector-detail (&key name)
  (:description "Full metadata for one inspector.")
  (:resource :uri "atelier://inspectors/{name}"
             :name "Atelier inspector detail"
             :mime-type :application/json)
  (declare (type string name))
  (find-inspector-detail name))

;; Expands to (roughly):
(progn
  (defclass inspector-detail-tool (tool resource-tool) ())
  
  (defmethod tool-name        ((tool inspector-detail-tool)) "atelier:inspector-detail")
  (defmethod tool-description ((tool inspector-detail-tool)) "Full metadata for one inspector.")
  (defmethod tool-input-schema ((tool inspector-detail-tool))
    (make-json-object "type" "object"
                      "properties" (make-json-object
                                    "name" (make-json-object "type" "string"))
                      "required" #("name")))
  
  (defmethod resource-uri-template ((tool inspector-detail-tool)) "atelier://inspectors/{name}")
  (defmethod resource-name         ((tool inspector-detail-tool)) "Atelier inspector detail")
  (defmethod resource-mime-type    ((tool inspector-detail-tool)) "application/json")
  
  (defmethod handle-tool-call ((tool inspector-detail-tool) arguments)
    (let ((name (cdr (assoc "name" arguments :test #'equal))))
      (declare (type string name))
      (find-inspector-detail name)))
  
  (register-tool (make-instance 'inspector-detail-tool)))
```

The expansion is deterministic and does one thing per step: class definition, per-slot reader methods (not slots, because each is computed from the source form, not stored), the handle-tool-call method, and the registration call. **No state lives on tool instances** other than class identity — all the metadata is computed from the class via dispatch.

**Why reader methods and not slots:** slots would force the `make-instance` call to pass six keyword arguments every time, and would let the metadata drift from the source form if someone `(setf)`s a slot. Reader methods tie the metadata to the class, which is tied to the symbol, which is tied to the source form.

## Atelier naming conventions for tool symbols

- Use kebab-case: `list-inspectors`, `probe-environment`.
- Symbol lives in the `atelier/mcp` package (for Atelier-native tools) or in the downstream system's `<name>/mcp` package (for third-party extensions).
- Don't export the symbol unless other Lisp code needs to reference the class or the generated methods. The tool-name string is the MCP-facing identifier; the symbol is an implementation detail.

## Dispatch flow

The runtime dispatch:

1. **`tools/call`** → dispatcher looks up the tool name in `*tool-registry*` → retrieves the tool instance → calls `handle-tool-call` with the parsed `arguments` alist → wraps the return value per the content-encoding rule in `mcp-protocol.md`.

2. **`resources/read`** → dispatcher looks up the URI first in `*concrete-resource-registry*` → on miss, walks `*template-resource-registry*` calling `match-uri-against-template` on each → on match, extracts the template bindings into an alist → calls `handle-tool-call` with that alist → wraps the return value with the declared MIME type.

Both paths call the same `handle-tool-call` method. The only difference is how `arguments` is populated.

## What the macro does NOT do

- It does not validate the JSON Schema at compile time against the handler body. Type declarations are advisory.
- It does not generate a schema for keyword parameters without type declarations — it emits an empty properties entry.
- It does not install a `method-combination` — plain standard method combination is enough.
- It does not allow multiple `(:resource ...)` clauses. A definition has at most one URI, one template, one MIME type.
- It does not support optional keyword arguments. MCP tools in Atelier have exactly the required arguments their schema declares.

## Test coverage

`testsuite/mcp/define-tool-macro.lisp` (step 24 of the plan) asserts:

1. Tool-only form: class created, `handle-tool-call` method present, registered in `*tool-registry*`, absent from both resource registries.
2. Concrete-resource form: class created, method present, registered in both `*tool-registry*` and `*concrete-resource-registry*`, absent from `*template-resource-registry*`.
3. Templated-resource form: class created, method present, registered in `*tool-registry*` and `*template-resource-registry*`, absent from `*concrete-resource-registry*`.
4. Resource-only form (`(:tool nil)`): absent from `*tool-registry*`, present in the relevant resource registry.
5. Invalid form rejected at compile time: positional parameter, missing `:description`, mismatched template placeholders, `(:tool nil)` without `(:resource ...)`, duplicate `(:description)`, unknown clause `(:foo ...)`.

Each validation test uses `handler-case (compile-form ...) (error ...)`. The form is built with `compile` rather than `read` so that the error arises from macro expansion, not from eval.

All tests run inside `with-isolated-registries` so that test-defined classes and their registry entries do not leak into the production state.
