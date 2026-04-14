# Implementation Phase 1 Notes: Slice 014 — MCP xref, CLOS inspector, trace

**Phase:** 1
**Plan:** product/slice/014-mcp-xref-inspector-trace/implementation-1.md
**Recorded:** 2026-04-14
**Status:** Complete

## Stories delivered in this phase

- S1: Who-calls — acceptance criteria: all passed
- S2: Who-references — acceptance criteria: all passed
- S3: Who-binds — acceptance criteria: all passed
- S4: Who-specializes — acceptance criteria: all passed
- S5: Who-macroexpands — acceptance criteria: all passed
- S6: CLOS class inspector — acceptance criteria: all passed
- S7: Trace/untrace — acceptance criteria: all passed (with `*trace-output*` binding at eval time)
- S8: Who-tests — acceptance criteria: all passed
- S9: Run-impacted — acceptance criteria: all passed

## Acceptance criteria results

| # | Criterion | Result | Verified by |
|---|-----------|:------:|-------------|
| 1 | who-calls returns callers with source file | PASS | validate-who-calls |
| 2 | who-calls on undefined symbol returns empty | PASS | validate-who-calls-unknown |
| 3 | who-references returns referencing functions | PASS | validate-who-references |
| 4 | who-binds returns binding sites | PASS | validate-who-binds |
| 5 | who-specializes returns method info | PASS | validate-who-specializes |
| 6 | who-macroexpands returns expansion sites | PASS | validate-who-macroexpands |
| 7 | inspect-class returns full class structure | PASS | validate-inspect-class |
| 8 | inspect-class on non-class returns error | PASS | validate-inspect-class-non-class |
| 9 | trace output visible in eval-form | PASS | validate-trace-output-in-eval |
| 10 | untrace removes tracing | PASS | validate-untrace-function |
| 11 | who-tests returns testcase names | PASS | validate-who-tests |
| 12 | run-impacted runs impacted tests | PASS | validate-run-impacted |
| 13 | 40 tools registered | PASS | 6 count assertions across test files |
| 14 | Full test suite passes | PASS | fresh SBCL run |
| 15 | CLAUDE.md updated | PASS | tool count 30 → 40 |
| 16 | No orphan child processes | PASS | validate-no-orphan-sbcl |

## Test results

- MCP tests: 352/352 (100%)
- Fast: all count assertions, handshake, dispatcher, registry — passed
- Slow: xref tools, inspect/trace, journey tests, child-dependent — passed
- Snail: none

## Invariants established or confirmed

- INV-36: `*current-server*` must be defined before the dispatcher
- INV-37: `sb-introspect:who-*` returns `(NAME . DEFINITION-SOURCE)` conses
- INV-38: `connection-alive-p` must detect broken SWANK sockets (deferred to slice 015)
- INV-39: UIOP handles both stdout and stderr as functions on SBCL
- INV-40: Trace output goes to `*trace-output*`, not `*standard-output*`

## Deferred items (for next phase)

None — all stories delivered. MCP reliability improvements deferred to slice 015.

## Reworks performed

1. `*current-server*` defvar ordering — moved from eval-form.lisp to dispatcher.lisp. Trigger: live MCP protocol failed silently. Impact: minor, added regression test.
2. `sb-introspect:who-*` return shape — code treated entries as bare definition-sources. Trigger: TYPE-ERROR at runtime. Impact: minor.
3. `*trace-output*` capture — trace output lost because SWANK only captures `*standard-output*`. Trigger: journey test. Impact: tests bind `*trace-output*` at eval time. Proper separate capture deferred to slice 015.
4. Template `FILE-EXISTS` crash — `write-template` used default `:if-exists :error`. Fixed to `:if-exists :supersede`.

## New risks discovered

- MCP server cannot recover from broken SWANK socket without client reconnection (INV-38)
- MCP tests cannot run inside the child (nested child spawning crashes SWANK)
- Hand-rolled drain thread in `make-child-connection` unnecessary (UIOP handles both streams)

## Technical decisions made

- `who-tests` uses direct caller matching (check if `who-calls` result symbol has testcase property) rather than transitive closure. Simpler and correct for direct callers.
- `inspect-class` filters anonymous classes from subclass list.
- Trace output captured via `(let ((*trace-output* *standard-output*)) ...)` binding at eval time rather than global redirect. Proper separate capture is a slice 015 story.
- `.asd` test-op integration reverted — `run-tests-fresh` handles test execution directly, ASDF test-op was causing recursive child spawns.

## Notes for Strategist retrospective

- Stories remaining: none
- Snail tests requiring confirmation: none
- Recommended: Slice 015 (MCP reliability) should be next — broken-pipe recovery, test speed, MCP kernel for reload are blocking MCP server usability as a development tool.
