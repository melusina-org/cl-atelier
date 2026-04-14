# Assertions to Validate — Slice 015

Experiments whose truth value is unknown and whose outcome shapes the
implementation. Each should become a discovery test in
`testsuite/discovery/` or `testsuite/mcp/`.

## A1. Child spawn time is the test bottleneck

**Hypothesis:** MCP test slowness comes from spawning 5+ child SBCLs
(~3–5s each = 15–25s of pure spawn overhead).

**Experiment:** Time `make-child-connection` in isolation (spawn +
SWANK connect, no eval). Time `connection-eval "(+ 1 2)"` on a warm
child. Compare. If spawn time dominates, sharing one child across all
test groups saves 80%+ of the overhead.

**Alternative hypothesis:** The bottleneck is `asdf:load-system` in
the child (loading atelier, testsuite, etc.), not the spawn itself.
If true, sharing one child is even more beneficial because the loaded
systems persist.

**How to encode:** Add a `validate-child-spawn-timing` discovery
testcase that measures both timings and asserts the spawn dominates.

## A2. SWANK socket health can be probed cheaply

**Hypothesis:** A non-blocking `swank-eval` of `T` (or `:emacs-ping`)
with a 100ms timeout can detect broken SWANK sockets without
measurable overhead on the happy path.

**Experiment:** Measure round-trip time of `swank-eval conn "T"` on
a warm connection. Then kill the child process, verify that the probe
detects the broken socket within 200ms.

**Alternative approach:** Catch the `broken-pipe` error in
`connection-eval` and treat it as a dead connection signal (reactive,
not proactive). This avoids any probe overhead entirely.

**Decision needed:** Proactive probe on every `connection-alive-p`
call? Or reactive catch in `ensure-child-connection`?

## A3. ASDF force-reload works for tool registration

**Hypothesis:** `(asdf:load-system "org.melusina.atelier/mcp" :force t)`
in the running server image correctly re-registers all tools,
resources, and generic function methods.

**Experiment:** In a running image:
1. Count tools via `(hash-table-count *tool-registry*)`
2. Create a new tool file on disk
3. Add it to the .asd
4. Call `(asdf:load-system "org.melusina.atelier/mcp" :force t)`
5. Verify the new tool appears in the registry

**Risk factors:**
- `define-tool` macro creates new CLOS classes; old classes may persist
- Package export lists may not be updated
- Stale generic function methods may shadow new ones
- `*tool-registry*` hash table may need clearing before reload

**How to encode:** Exploratory test in `testsuite/mcp/` that
performs the reload and checks invariants.

## A4. Child output streams can be captured separately

**Hypothesis:** SWANK's `eval-and-grab-output` only captures
`*standard-output*`. Binding `*trace-output*` and `*error-output*`
to broadcast streams in the child-worker entry point can capture
them separately.

**Experiment:** In the child, eval:
```lisp
(let ((trace-out (make-string-output-stream))
      (err-out (make-string-output-stream)))
  (let ((*trace-output* (make-broadcast-stream *trace-output* trace-out))
        (*error-output* (make-broadcast-stream *error-output* err-out)))
    (trace car)
    (car '(1 2 3))
    (untrace car)
    (warn "test warning"))
  (values (get-output-stream-string trace-out)
          (get-output-stream-string err-out)))
```

If the broadcast-stream approach works, the child-worker can install
these streams at startup and return them as extra values from
`eval-and-grab-output`.

**Alternative:** Modify the child-worker's `start-worker` to redirect
these streams globally so all subsequent evals capture them.

## A5. UIOP handles child I/O correctly without hand-rolled drain threads

**Hypothesis:** The current stdout drain thread in `make-child-connection`
is unnecessary if UIOP is used correctly. UIOP's `launch-program` with
proper stream options may handle non-blocking I/O natively.

**Investigation needed:**
1. Does UIOP use `select(2)` or `poll(2)` internally for stream
   multiplexing?
2. What does `uiop:launch-program` with `:output :stream
   :error-output :output` actually do on SBCL/Darwin?
3. Can `uiop:wait-process` + `uiop:slurp-input-stream` replace the
   manual drain thread?
4. What do advanced UIOP consumers (e.g., ASDF's own subprocess
   handling) do for long-running child processes?

**Current anti-pattern:** `make-child-connection` spawns a
`bordeaux-threads:make-thread` that loops on `read-line` from child
stdout. This is fragile (thread leaks on error paths), platform-
specific in behavior, and duplicates functionality that UIOP likely
provides.

**How to encode:** Read UIOP source for `launch-program` and
`%process-info-*` on SBCL. Write a discovery test that launches a
child with heavy stdout, reads the SWANK port, and verifies no
deadlock — using only UIOP primitives, no drain thread.

## A6. MCP kernel extraction is clean (was A5)

**Hypothesis:** The MCP server code can be split into a kernel system
(protocol, dispatch, connection management) and a tools system
(concrete tool definitions) without circular dependencies.

**Experiment:** Create `org.melusina.atelier/mcp-kernel` in the .asd
with everything except the `tools/` module and `hyperspec.lisp`.
Verify it loads independently. Verify `/mcp` loads on top of it.

**Key question:** Does `define-tool` (macro) live in kernel or tools?
It must live in kernel because tools use it. But does it pull in any
tool-specific dependencies? Check the macro expansion.
