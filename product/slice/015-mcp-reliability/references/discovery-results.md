# Discovery Results — Slice 015

## A1: Child spawn time — VALIDATED, hypothesis REFINED

**Result:** A single SBCL spawn is ~180ms (including quicklisp + child-worker load).
The bottleneck is NOT process spawning — it's `asdf:load-system` inside the child
(loading atelier, testsuite, etc.), which takes seconds per system.

| Operation | Time |
|-----------|------|
| `true` spawn+exit | 58ms |
| Bare SBCL spawn+exit | 164ms |
| SBCL + quicklisp load | 176ms |
| SBCL + ql + child-worker load | 181ms |

**Implication:** Sharing one child across test groups saves the system-load time
(~2–5s per group for loading atelier + testsuite), not the spawn time. Total
savings: ~10–20s from avoiding 4 redundant `asdf:load-system` calls.

## A5: UIOP output handling — VALIDATED, drain thread unnecessary

**Result:** UIOP on SBCL supports:

1. **Both `:output` and `:error-output` as functions simultaneously** — both
   receive their respective streams and capture data correctly.
2. **`launch-program` with `:stream` for both** — returns process-info with
   separate `process-info-output` and `process-info-error-output` streams.
3. **`:error-output :output`** — merges stderr into stdout cleanly.

**Implication:** The hand-rolled drain thread in `make-child-connection` is
unnecessary. Two clean alternatives:

**Option A (recommended for child spawn):** Use `launch-program` with
`:output :stream :error-output :stream`. Read stdout line-by-line until
the port number appears. Then read stderr in a second pass (process
is still running but stderr buffer is small). No thread needed.

**Option B:** Use `run-program` with `:output function :error-output function`.
Both functions execute. But this requires the process to complete — not
suitable for long-running child SBCL.

**For child spawn specifically:** Option A fits because we need to read
stdout incrementally (looking for the port), while stderr is diagnostic
(compiler warnings). Read stdout for the port, connect SWANK, then
optionally read stderr later.

**Deadlock risk:** If the child produces >64KB of stderr before printing
the port to stdout, the stderr pipe buffer fills and the child blocks.
Mitigation: redirect stderr to a temp file during spawn
(`:error-output PATHNAME`), read it after SWANK connects.

## Experiment source

`testsuite/discovery/uiop-output-handling.lisp` — runnable with
`sbcl --non-interactive --load`.
