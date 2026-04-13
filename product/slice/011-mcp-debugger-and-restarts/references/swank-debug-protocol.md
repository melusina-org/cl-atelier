# SWANK Debug Protocol Reference

## Debug Message Flow

When an eval enters the debugger:

1. Client sends `:emacs-rex (swank:eval-and-grab-output "...") PKG t ID`
2. Server sends `:debug THREAD LEVEL CONDITION RESTARTS FRAMES CONTS`
   - CONDITION: list of strings (condition type, message)
   - RESTARTS: list of `(NAME DESCRIPTION)` pairs
   - FRAMES: list of `(INDEX DESCRIPTION &rest ANNOTATIONS)` — typically top 20
   - CONTS: list of continuation IDs (internal)
3. Server sends `:debug-activate THREAD LEVEL`
4. Client can now:
   - Send `:emacs-rex (swank:invoke-nth-restart-for-emacs LEVEL N) PKG t ID2` — choose restart N
   - Send `:emacs-rex (swank:backtrace START END) PKG t ID3` — get more frames
   - Send `:emacs-rex (swank:frame-locals-and-catch-tags FRAME-INDEX) PKG t ID4` — inspect frame
   - Send `:emacs-rex (swank:eval-string-in-frame STRING FRAME-INDEX PKG) PKG t ID5` — eval in frame
5. After restart: server sends `:debug-return THREAD LEVEL`
6. If restart continues execution: server eventually sends `:return (:ok ...) ID` (original eval)
7. If restart aborts: server sends `:return (:ok ...) ID2` (restart), original ID never gets `:return`

## Interrupt

- Client sends `:emacs-interrupt :repl-thread` (raw, not inside `:emacs-rex`)
- This causes the child to enter the debugger with a keyboard-interrupt condition
- Normal debug flow follows

## Key Invariant (INV-24)

After abort: the original eval's `:return` **never arrives**. Only the abort restart's `:return` does.

## SWANK Functions Used

- `swank:eval-and-grab-output STRING` — primary eval
- `swank:invoke-nth-restart-for-emacs LEVEL INDEX` — select restart
- `swank:backtrace START END` — get backtrace frames
- `swank:frame-locals-and-catch-tags FRAME-INDEX` — get frame locals
- `swank:eval-string-in-frame STRING FRAME-INDEX PACKAGE` — eval in frame context
