# SWANK Wire Protocol Reference

Reference for implementing the parent-side SWANK client in Phase 2 of
slice 010. Based on SLIME v2.32 (`swank/rpc.lisp`, `swank.lisp`).

## Message Framing

Every SWANK message on the wire is:

```
<6-char-hex-length><UTF-8-encoded-s-expression>
```

- **Header:** 6 ASCII characters encoding the payload length in hexadecimal.
  E.g., `00002a` for a 42-byte payload.
- **Payload:** A UTF-8 encoded s-expression, `prin1`-printed with
  `*print-readably*` and `*package*` bound to the `SWANK-IO-PACKAGE`.
- **No trailing newline** in the framing itself. The s-expression may
  contain newlines as part of its printed representation.

### Writing a message (from `rpc.lisp:write-message`)

```lisp
(let* ((string (prin1-to-string sexp))
       (octets (string-to-utf8 string))
       (length (length octets)))
  (write-header stream length)    ;; 6-char hex
  (write-sequence octets stream)
  (finish-output stream))
```

### Reading a message (from `rpc.lisp:read-message`)

```lisp
(let* ((length (parse-integer (read-chunk stream 6) :radix 16))
       (octets (read-chunk stream length))
       (string (utf8-to-string octets)))
  (read-from-string string))
```

**Important:** The stream must be an octet stream (`(unsigned-byte 8)`
element type). `usocket:socket-stream` provides this.

## Client-to-Server Messages

### `:emacs-rex` — Remote Evaluation

```lisp
(:emacs-rex FORM PACKAGE THREAD-ID CONTINUATION-ID)
```

- **FORM:** An s-expression to evaluate in the server. Commonly
  `(swank:interactive-eval STRING)` or any SWANK-exported function call.
- **PACKAGE:** A string naming the buffer package, e.g. `"CL-USER"`.
  The server binds `*buffer-package*` to this package during eval.
- **THREAD-ID:** `:repl-thread` or `t` (current thread). Use `t`.
- **CONTINUATION-ID:** A positive integer. The server echoes this in
  the `:return` response. Increment per request.

Example:
```lisp
(:emacs-rex (swank:interactive-eval "(+ 1 2)" 0 0) "CL-USER" t 1)
```

### `:emacs-pong` — Keepalive Response

```lisp
(:emacs-pong THREAD TAG)
```

Sent in response to a `:ping` from the server. Echo the THREAD and TAG.

### `:emacs-interrupt` — Interrupt Evaluation

```lisp
(:emacs-interrupt THREAD-ID)
```

Interrupts the running evaluation in the given thread.

## Server-to-Client Messages

### `:return` — Evaluation Result

```lisp
(:return THREAD VALUE CONTINUATION-ID)
```

- **THREAD:** The thread that performed the evaluation (integer).
- **VALUE:** One of:
  - `(:ok RESULT)` — successful evaluation; RESULT is the printed value.
  - `(:abort CONDITION-TEXT)` — evaluation aborted; condition as string.
- **CONTINUATION-ID:** Matches the ID from the `:emacs-rex` request.

### `:write-string` — Captured Output

```lisp
(:write-string STRING TARGET)
```

- **STRING:** Text written to `*standard-output*` during evaluation.
- **TARGET:** Typically `:repl-result` or nil.

Accumulate all `:write-string` messages between sending `:emacs-rex`
and receiving `:return` to build the captured stdout.

### `:ping` — Keepalive

```lisp
(:ping THREAD TAG)
```

Respond immediately with `(:emacs-pong THREAD TAG)`.

### `:debug` — Debugger Activation

```lisp
(:debug THREAD LEVEL CONDITION RESTARTS FRAMES CONTINUATIONS)
```

- **CONDITION:** A list of condition description strings.
- **RESTARTS:** A list of `(NAME DESCRIPTION)` pairs.
- **FRAMES:** Backtrace frames.

### `:debug-activate` — Debugger Ready

```lisp
(:debug-activate THREAD LEVEL &optional SELECT)
```

Signals that the debugger is ready for interaction at LEVEL.

## Slice 010 Auto-Abort Strategy

When `:debug` is received, slice 010 does not offer interactive debugging
(that is slice 011). Instead:

1. Record the condition text from the `:debug` message.
2. Send an `:emacs-rex` to invoke the ABORT restart:
   ```lisp
   (:emacs-rex (swank:invoke-nth-restart-for-emacs LEVEL 0) "CL-USER" t NEXT-ID)
   ```
   Restart 0 is conventionally ABORT. If not, try `swank:throw-to-toplevel`.
3. Wait for the `:return` from the abort.
4. Return the original condition text as the eval error.

## Connection Lifecycle

### Startup

1. Parent spawns child SBCL via `uiop:launch-program`.
2. Child runs `(require :swank)`, then `(swank:create-server :port 0 :dont-close t)`.
3. `create-server` returns the OS-assigned port.
4. Child prints the port to stdout as a single decimal integer line.
5. Parent reads the port from child's stdout stream.
6. Parent connects via TCP to `127.0.0.1:PORT` using `usocket:socket-connect`.
7. (Optional) Parent sends the SLIME secret from `~/.slime-secret` if it exists.
8. Parent is now ready to send `:emacs-rex` messages.

### Shutdown

1. Parent sends `(:emacs-rex (swank:quit-lisp) "CL-USER" t N)`.
2. Parent closes the TCP socket via `usocket:socket-close`.
3. Parent calls `uiop:terminate-process` + `uiop:wait-process` on the child.

### Liveness Check

- Check `(uiop:process-alive-p process-info)` on the child process.
- Optionally check the socket is still connected (catch write errors).

## SWANK Functions Used by Slice 010

| Function | Purpose | Slice |
|---|---|---|
| `swank:interactive-eval` | Eval string, return printed result | 010 (S5) |
| `swank:invoke-nth-restart-for-emacs` | Auto-abort debugger | 010 (S5) |
| `swank:throw-to-toplevel` | Fallback abort | 010 (S5) |
| `swank:quit-lisp` | Graceful shutdown | 010 (S4) |
| `swank:list-all-package-names` | Package listing | 010 (S6) |
| `swank:apropos-list-for-emacs` | Symbol search | 010 (S6) |
| `swank:find-definitions-for-emacs` | Source location | 010 (S6) |

## Notes for the Maker

- The SWANK package is the `*package*` for reading messages on both sides.
  When the client `prin1`s an s-expression for sending, it must ensure
  symbols like `swank:interactive-eval` are printed with the package prefix.
  Use `(let ((*package* (find-package :keyword))) (prin1-to-string sexp))`.
- `interactive-eval` takes `(STRING LINES WIDTH)`. LINES and WIDTH are
  for pretty-printing context. Pass `0` and `0` for no pretty-print limits.
- `swank:create-server` returns the port as an integer. This is the value
  the child prints. **Not** a string.
- After `create-server` with `:style :spawn` (the default on SBCL), SWANK
  creates a listener thread. The child's main thread must block (sleep loop)
  or the process exits.
