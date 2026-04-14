# Slice 015: MCP Server Reliability

**Status:** Planned
**Type:** Improvement
**Goal addressed:** G5
**Backlog items:** (new — MCP server reliability)
**Planned start / end:** 2026-04-14 /
**Actual end:**
**Implementation phases:**
  - Phase 1: product/slice/015-mcp-reliability/implementation-1.md — Planned

---

## What changes for users

The MCP server becomes a reliable development partner:
- **Broken SWANK connections auto-recover** instead of returning "broken pipe" errors
  until the client reconnects
- **Child stdout, stderr, and trace output are captured separately** so debugging
  information is never lost
- **Tests run fast** — a single shared child serves all test groups instead of
  spawning 5+ child SBCLs
- **The server can reload its own code** — tool and child-worker changes take
  effect without rebuilding the binary
- **Targeted test execution** — run a specific testcase by name, not just the
  full suite
- **The template bug no longer crashes `run-tests-fresh`**

## Stories

### S1: SWANK socket health check
**In order to** recover automatically when the SWANK connection breaks,
**a** developer **can** rely on the MCP server to detect dead SWANK sockets
and respawn the child.
**Acceptance criteria:**
- Given a child whose SWANK socket is closed (broken pipe), when
  `eval-form` is called, then the server detects the broken connection,
  shuts down the child, spawns a fresh one, and evaluates the form
- Given a healthy child, when `connection-alive-p` is called, then it
  returns T without probing the socket (no performance cost on happy path)
- Discovery test: measure SWANK health-check overhead (must be <1ms)

### S2: Separate stdout, stderr, trace-output capture
**In order to** see trace output, compiler warnings, and normal output
distinctly,
**a** developer **can** read `stdout`, `stderr`, and `trace-output` as
separate fields in eval-form results.
**Acceptance criteria:**
- Given a traced function, when `eval-form` calls it, then trace output
  appears in a `trace-output` field (not mixed into `stdout`)
- Given code that writes to `*error-output*`, when `eval-form` runs it,
  then the output appears in a `stderr` field
- Given code that writes to `*standard-output*`, then output appears in
  `stdout` as before

### S3: Single shared child for MCP tests
**In order to** run the MCP test suite in under 60 seconds,
**a** developer **can** run all child-dependent tests against one shared child.
**Acceptance criteria:**
- Discovery test: measure child spawn time (expected ~3–5s each, ~15–25s
  total for 5 groups)
- All child-dependent test groups (`run-child-dependent-tests`,
  `run-asdf-tools-tests`, `run-documentation-tools-tests`,
  `run-xref-tools-tests`, `run-inspect-trace-tests`,
  `run-journey-tests`) share one child connection
- Tests that mutate child state (trace, defvar) clean up after themselves
- Total MCP test time <90s (measured by discovery test)

### S4: MCP kernel system for reload
**In order to** reload server code without rebuilding the binary,
**a** developer **can** call a `reload-server` tool that reloads all MCP
tool/resource definitions from disk.
**Acceptance criteria:**
- `org.melusina.atelier/mcp-kernel` system contains: package, conditions,
  JSON utilities, tool/resource protocol, message hierarchy, dispatcher,
  server loop, image-connection, SWANK protocol, transcript — everything
  needed to bootstrap and reload
- `org.melusina.atelier/mcp` depends on `/mcp-kernel` and adds all tools,
  resources, and hyperspec infrastructure
- `reload-server` tool calls `(asdf:load-system "org.melusina.atelier/mcp"
  :force t)` and re-registers tools
- After reload, new tools are discoverable via `tools/list`
- After reload, existing child connection is preserved (not respawned)
- Discovery test: verify that adding a new tool file, reloading, and
  calling the tool works end-to-end

### S5: Targeted test execution in run-tests-fresh
**In order to** run a specific testcase without running the full suite,
**a** developer **can** pass a testcase designator to `run-tests-fresh`.
**Acceptance criteria:**
- Given `system-name` and optional `testcase-designator`, when
  `run-tests-fresh` is called, then only that testcase runs
- Given only `system-name`, when `run-tests-fresh` is called, then
  `run-all-tests` (or `asdf:test-system`) runs as before
- The fresh SBCL loads the system, resolves the testcase symbol, and
  calls it

### S6: Fix template FILE-EXISTS crash
**In order to** run `asdf:test-system` without crashes,
**a** developer **can** rely on `write-template` to overwrite existing files.
**Acceptance criteria:**
- `write-template` for `file-template` uses `:if-exists :supersede`
- `run-tests-fresh "org.melusina.atelier"` completes with exit-code 0
- A test verifies that `new-lisp-project` can be called twice on the
  same directory without error

## Quality Criteria

- [ ] No broken-pipe errors surface to MCP clients under any child crash
  scenario
- [ ] MCP test suite completes in <90s in fresh SBCL
- [ ] `reload-server` preserves child connection and session state
- [ ] All 40+ tools still function after `reload-server`
- [ ] CLAUDE.md updated

## Definition of Ready

- [x] Stories traceable to session findings
- [x] Stories sized <= 2 days each
- [x] Acceptance criteria written
- [x] Quality criterion defined
- [x] Discovery tests specified

## Definition of Done

- [ ] All stories complete with acceptance criteria passing
- [ ] Quality criteria passing
- [ ] Full test suite passes
- [ ] All implementation phases have completion notes
- [ ] `product/slice/015-mcp-reliability/retrospective.md` created
- [ ] `product/backlog.md` updated
- [ ] `product/roadmap.md` updated
