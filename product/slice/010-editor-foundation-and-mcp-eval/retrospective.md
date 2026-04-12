# Retrospective: Slice 010 — Editor Foundation and MCP Eval

**Recorded:** 2026-04-12
**Implementation phases:** 2 total
  - Phase 1: `implementation-1.md` — Editor foundation (complete)
  - Phase 2: `implementation-2.md` — MCP eval layer (complete)

## Delivery Summary

- Stories delivered: 11 of 11 (S0–S11)
- Acceptance criteria: 39 of 41 passed, 2 deferred (AC28 timeout, AC29 auto-restart)
- Quality criteria: all passing
- Full test suite: 623/623 (100%) in fresh SBCL subprocess

## Phase Notes Summary

| Phase | Key deliverables | Deferred to backlog |
|-------|-----------------|---------------------|
| 1 | `org.melusina.atelier/editor` system (16 exports), `toplevel-form` record (5 slots), `read-toplevel-form-from-string`, `write-toplevel-form-to-string`, `normalize-toplevel-form`, `atelier:lint-string`, 15 auto-discovered fixtures, custom Eclector client preserving `#+`/`#-`. 104 assertions. | — |
| 2 | `org.melusina.atelier/child-worker` system, SWANK wire protocol client, `child-connection` subclass, 8 new MCP tools (14 total), exploratory test systems for I/O and SWANK behavior. 40 assertions. | eval-form timeout (AC28), dead-child auto-restart (AC29) |

## Goal Progress

- **G5** (MCP server exposes REPL evaluation, debugger, CLOS introspection, rename refactorings): closer — eval, introspection, and testsuite runners are now operational. Debugger (slice 011), ASDF operations (slice 012), and rename refactorings (slice 015) remain.

## What we learned

1. **SWANK is a viable CL-to-CL communication protocol** despite being designed for Emacs. The wire protocol is generic s-expression RPC. The key is to use `eval-and-grab-output` (not `interactive-eval`) and to handle the debug/abort lifecycle correctly. A minimal client is ~170 LOC.

2. **Pipe I/O deadlock is a fundamental pattern** that any process-spawning code must handle. Merging stderr into stdout (`:error-output :output`) plus a background drain thread after reading the port is the reliable approach. This was discovered through systematic testing (exploratory test system) rather than ad-hoc debugging.

3. **Exploratory test systems are a high-value investment.** The `testsuite/input-output` and `testsuite/swank` systems encoded 28 assertions of learning about external behavior. Every rework in Phase 2 was diagnosed and fixed using these tests. They transform "I think SWANK works this way" into "I know SWANK works this way, and here's the test."

4. **The "source text is the truth" decision from Phase 1 proved load-bearing.** `canonicalize-form` runs the full lint pipeline on a form's source text and re-reads the result. Feature guards (`#+`/`#-`) survive because the write path copies verbatim from source, not from CST reconstruction.

## User feedback since delivery

*(to be filled 2–4 weeks after release)*

## Backlog and roadmap changes

- Completed: backlog item #6 (MCP child image lifecycle, eval, package/symbol introspection) — editor foundation + MCP eval half delivered
- Added: eval-form timeout and dead-child auto-restart as polish items in queued section
- No reprioritization needed
