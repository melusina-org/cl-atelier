# Child-Worker System Contract

The `org.melusina.atelier/child-worker` ASDF system is loaded in the
child SBCL process. It provides the startup sequence and introspection
helper functions that the parent MCP tools call via SWANK eval.

## System Dependencies

- `closer-mop` — portable MOP for class/generic introspection.

SWANK is loaded at runtime via `(require :swank)`, not as an ASDF dependency.

## Package: `#:atelier/child-worker`

### Startup

#### `start-worker`

Entry point for the child SBCL image. Called by the parent's spawn
sequence via `--eval`.

1. `(require :swank)` — load SWANK.
2. Configure SWANK:
   - `(setf swank:*configure-emacs-indentation* nil)` — suppress Emacs
     indentation configuration.
   - `(setf swank::*coding-system* "utf-8-unix")` — force UTF-8.
3. `(swank:create-server :port 0 :dont-close t)` — start SWANK on
   OS-assigned port. Returns the port integer.
4. Print port: `(format t "~D~%" port) (finish-output)`.
5. Block: `(loop (sleep 3600))` — SWANK runs in background threads;
   main thread must not exit.

The parent reads the port line from the child's stdout, then connects
via TCP.

### Introspection Helpers

These functions are called by the parent via SWANK eval:
```lisp
(:emacs-rex (swank:interactive-eval "(atelier/child-worker:list-packages-data)") ...)
```

The parent reads the printed return value as a string, then `read-from-string`
to get the Lisp data.

#### `list-packages-data` → list of alists

Each alist:
```lisp
(("name" . "COMMON-LISP")
 ("nicknames" . ("CL"))
 ("use-list" . ())
 ("used-by-list" . ("CL-USER" ...))
 ("external-count" . 978)
 ("internal-count" . 0))
```

Uses `list-all-packages`, `package-name`, `package-nicknames`,
`package-use-list`, `package-used-by-list`. Counts via
`do-external-symbols` / `do-symbols` tally.

#### `list-package-symbols-data` (package-name &key (status :external)) → list of alists

Each alist:
```lisp
(("name" . "CAR")
 ("package" . "COMMON-LISP")
 ("status" . "EXTERNAL")
 ("home-package" . "COMMON-LISP")
 ("kind" . "FUNCTION")
 ("documentation" . "Return the 1st object in a list."))
```

**kind** is one of: `"FUNCTION"`, `"GENERIC-FUNCTION"`, `"MACRO"`,
`"SPECIAL-FORM"`, `"VARIABLE"`, `"CONSTANT"`, `"CLASS"`, `"CONDITION"`,
`"TYPE"`, `"UNBOUND"`.

Uses `do-external-symbols` / `do-symbols` / `do-all-symbols` depending
on status. Kind determined via `fboundp`, `macro-function`,
`special-operator-p`, `find-class`, `boundp`, `constantp`.

For generic function detection: `(typep (fdefinition sym) 'generic-function)`.

Documentation via `(documentation sym 'function)` or `'variable`.

#### `describe-symbol-data` (designator) → alist

**designator** is a string like `"cl:car"` or `"alexandria:flatten"`.
Parsed via `(let ((*package* (find-package :cl-user))) (read-from-string designator))`.

Returns:
```lisp
(("name" . "CAR")
 ("package" . "COMMON-LISP")
 ("kind" . "FUNCTION")
 ("lambda-list" . "(LIST)")
 ("documentation" . "Return the 1st object in a list.")
 ("type" . "(FUNCTION (LIST) (VALUES T &OPTIONAL))"))
```

For classes, adds `"slots"` (list of slot-name strings via `closer-mop:class-slots`).
For generic functions, adds `"methods"` (list of method specializer strings
via `closer-mop:generic-function-methods`).

#### `find-definition-data` (designator) → alist or NIL

Returns:
```lisp
(("source-file" . "/path/to/file.lisp")
 ("line" . 42)
 ("column" . 0))
```

Uses `sb-introspect:find-definition-sources-by-name` (behind `#+sbcl`).
Returns NIL if no source location available.

Line number from `sb-introspect:definition-source-form-number` or
`sb-introspect:definition-source-character-offset` — the exact
conversion depends on what SBCL provides. File from
`sb-introspect:definition-source-pathname`.

#### `run-testsuite-data` (system-name) → alist

Loads the system via `(asdf:load-system system-name)`, then runs
`(asdf:test-system system-name)`, capturing stdout/stderr.

Returns:
```lisp
(("system" . "org.melusina.atelier")
 ("passed" . 583)
 ("failed" . 0)
 ("errored" . 0)
 ("duration-ms" . 12345)
 ("output" . "Running test suite...\n..."))
```

Parsing the test result format depends on the test framework. For
`org.melusina.confidence`, the output contains pass/fail counts.
The Maker will implement a simple parser or use the return value from
the test runner if structured.

## Shutdown

The parent does not call a shutdown function on the child-worker.
Instead:
1. Parent sends `(:emacs-rex (swank:quit-lisp) ...)` via SWANK.
2. Parent calls `uiop:terminate-process` on the child process.
3. Parent calls `uiop:wait-process` to reap the child.

## Testing

The child-worker system's introspection functions can be tested in
the parent process (they don't require SWANK). Unit tests call the
functions directly after `(asdf:load-system "org.melusina.atelier/child-worker")`.

Integration tests (via the full SWANK path) are slow tests that spawn
a child, eval the function, and verify the result shape.
