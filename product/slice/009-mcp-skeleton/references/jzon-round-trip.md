# jzon Round-Trip Reference

**Library:** `com.inuoe.jzon` (Quicklisp)
**Version probed:** 1.1.4 (`/Users/michael/share/quicklisp/dists/quicklisp/software/jzon-v1.1.4/`)
**Probed:** 2026-04-10
**Authority:** this document. Atelier code that touches jzon must conform to the conventions here, not to assumptions from other JSON libraries.

This is the **Step 0** finding for slice 009 phase 1, written before any protocol code, per risk R4 in `implementation-1.md`.

## Default representations

| JSON | Lisp value (parse output) | Lisp value (stringify input) |
|---|---|---|
| `null` | symbol `cl:null` | symbol `cl:null` |
| `true` | `t` | `t` |
| `false` | `nil` | `nil` |
| `42` | integer `42` | integer |
| `3.14` | `3.14d0` (double-float) | float |
| `"hi"` | `"hi"` | string |
| `[1,2,3]` | `#(1 2 3)` (vector) | vector OR plain list |
| `{}` | `#<HASH-TABLE :TEST EQUAL>` | hash-table only |

## Asymmetries that bite

**1. JSON `null` parses to the symbol `cl:null`, not to `nil`.**

```lisp
(eq (jzon:parse "null") nil)  ; → NIL  (different!)
(eq (jzon:parse "null") 'cl:null)  ; → T
```

The CL symbol `null` is the same one that names the predicate `(null x)`. Atelier code must use the symbol, not `nil`, when it means "JSON null."

**2. CL `nil` stringifies to `"false"`, not to `"null"`.**

```lisp
(jzon:stringify nil)   ; → "false"
(jzon:stringify 'null) ; → "null"
(jzon:stringify t)     ; → "true"
```

This means `nil` is overloaded: it's the empty list, the boolean false, and the absence of a value. When Atelier code wants to emit JSON `null`, it must use the symbol `'null` explicitly. When it wants to emit `false`, plain `nil` works.

**3. Plain lists encode as JSON arrays, not objects.**

```lisp
(jzon:stringify '(1 2 3))                ; → "[1,2,3]"
(jzon:stringify '(:foo 1 :bar 2))        ; → "[\"FOO\",1,\"BAR\",2]"  ← plist becomes flat array
(jzon:stringify '(("foo" . 1)))          ; → ERROR — alist iteration breaks on the cdr
(jzon:stringify '((:foo . 1)))           ; → ERROR — same
```

**Critical:** alists and plists do NOT encode as JSON objects. Atelier handlers that want to emit a JSON object must:

- build a hash-table directly, or
- pass an alist/plist through the wrapper's `make-json-object` helper (defined in `src/mcp/json-util.lisp`), or
- use the incremental writer macros.

## Three-state key extraction

JSON-RPC 2.0 distinguishes a notification (no `id` key) from a request with `id: null` (which is malformed but parseable). To tell them apart:

```lisp
(let ((id (gethash "id" request :missing)))
  (cond ((eq id :missing)  ;; Notification — no id field
         (handle-notification request))
        ((eq id 'null)     ;; Malformed — id field present but JSON null
         (make-protocol-error -32600 "Invalid Request: id is null"))
        (t
         (handle-request request id))))
```

This three-state pattern is the **only** correct way to extract any optional MCP field. Atelier wrappers always use the three-argument `gethash` form when the absence of a key has different meaning from a null value.

## Integer and float ranges

- Integers parse to CL integers with no upper bound. JSON-RPC IDs above `2^53` (JavaScript's `Number.MAX_SAFE_INTEGER`) parse correctly to bignums. Verified with `9007199254740993`.
- Floats parse to `double-float`. SBCL prints them with the `d0` reader-syntax suffix; jzon strips that on stringify.

## UTF-8

Round-trips cleanly. Verified with `"Michaël Le Barbier"`, `"©"`, and the wrench emoji `🛠`. No `:external-format` or `:encoding` parameter needed; the library reads and writes UTF-8 by default. (The underlying SBCL stream encoding determines this; on macOS and Linux the default is UTF-8.)

## Incremental writer macros

The `with-writer*` / `with-object*` / `with-array*` / `write-property*` / `write-key*` macros let you build a JSON-RPC response without intermediate hash-tables:

```lisp
(with-output-to-string (s)
  (jzon:with-writer* (:stream s)
    (jzon:with-object*
      (jzon:write-property* "jsonrpc" "2.0")
      (jzon:write-property* "id"      1)
      (jzon:write-key*      "result")
      (jzon:with-object*
        (jzon:write-property* "protocolVersion" "2024-11-05")
        (jzon:write-key*      "serverInfo")
        (jzon:with-object*
          (jzon:write-property* "name"    "atelier-mcp")
          (jzon:write-property* "version" "0.1.0"))))))
;; → "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"protocolVersion\":\"2024-11-05\",\"serverInfo\":{\"name\":\"atelier-mcp\",\"version\":\"0.1.0\"}}}"
```

**Atelier policy:** the dispatcher uses incremental writers for the JSON-RPC envelope (jsonrpc, id, result/error). Tool handlers return Lisp data (alists, plists, or hash-tables) and the dispatcher converts that to a hash-table before invoking `jzon:stringify` on the wrapped result. Tool handlers never call jzon directly.

## Output shape

Compact: no pretty-printing by default, no trailing newline, no whitespace between elements. Hash-table iteration order is preserved on SBCL (insertion order), so building a response with `(setf (gethash "jsonrpc" h) "2.0")` first guarantees that key appears first in the output. **This SBCL guarantee is load-bearing for round-trip determinism in fixture tests.** Other implementations may not preserve order; documented as an SBCL-specific assumption (acceptable per the tech stack — SBCL is primary).

## Constants the wrapper module exposes

`src/mcp/json-util.lisp` will expose:

```lisp
(defparameter +json-null+   'cl:null   "The Lisp value that round-trips with JSON null.")
(defparameter +json-true+   t          "The Lisp value that round-trips with JSON true.")
(defparameter +json-false+  nil        "The Lisp value that round-trips with JSON false.")
;; Note: +json-false+ is also CL nil, the empty list, and the absence of a value.
;; Be explicit when you mean false vs absent.

(defun encode-to-string (object)        "jzon:stringify wrapper.")
(defun decode-from-string (string)      "jzon:parse wrapper.")
(defun make-json-object (&rest pairs)   "Build a hash-table from alternating string keys and values.")
(defun alist-to-json-object (alist)     "Convert an alist with string keys to a hash-table.")
(defun plist-to-json-object (plist)     "Convert a plist with keyword keys to a hash-table; keys downcased.")
(defun json-object-to-alist (object)    "Inverse — useful in tests.")
(defun missing-key ()                   "Return the unique sentinel for absent JSON keys; use with (gethash key obj (missing-key)).")
```

## What is NOT used

- `coerced-fields` and the CLOS-based serialization protocol — too implicit; we want explicit construction at the boundary so JSON shape changes are obvious.
- `*writer*` global parameter — we always pass streams explicitly.
- `parse-next` / `parse-next-element` streaming API — slice 009 reads complete frames; streaming is unnecessary.

## Test coverage in slice 009

`testsuite/mcp/jzon-round-trip.lisp` (step 13 of the plan) asserts:

1. Empty object stringify and parse round-trip.
2. Empty array stringify and parse round-trip.
3. The `null` / `true` / `false` distinction on both sides.
4. Plist conversion via `plist-to-json-object` produces `{"foo":1,"bar":2}` and not `["FOO",1,"BAR",2]`.
5. Alist conversion via `alist-to-json-object` likewise.
6. Three-state key extraction: missing vs null vs value.
7. Round-trip of a recorded MCP `initialize` request.
8. Round-trip of a recorded MCP `tools/list` response.

If any of these assertions fail when jzon is upgraded, the wrapper module is the only file that needs to change.

## Commit log

- 2026-04-10 — initial probe and reference, jzon 1.1.4. Probed via the running sbcl-eval MCP server. All claims in this document were verified by direct evaluation, not from documentation.
