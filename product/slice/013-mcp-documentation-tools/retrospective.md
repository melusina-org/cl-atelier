# Retrospective: Slice 013 — MCP Documentation Tools

**Recorded:** 2026-04-13
**Implementation phases:** 1

## Delivery Summary

- Stories delivered: 8 of 8
- Acceptance criteria: all passing (16/16)
- Quality criteria: all passing
- Full test suite: 695/695 (100%)

## Phase Notes Summary

| Phase | Key deliverables | Deferred to backlog |
|-------|-----------------|---------------------|
| 1 | 7 new tools (apropos-search, hyperspec-lookup, hyperspec-issue, hyperspec-issues, macroexpand-form, disassemble-symbol, compile-form), 3 new resources (lisp://hyperspec/symbol/{name}, lisp://hyperspec/issues/{name}, lisp://hyperspec/issues), 4 child-worker functions, HyperSpec infrastructure module. 36 new assertions. | None |

## Goal Progress

- **G5**: closer — documentation and introspection tools delivered. 30 tools total. The child SBCL is now a complete reference environment.

## What we learned

1. **PAT-11 continues to bite.** `CL:APROPOS` is exported from COMMON-LISP, so `define-tool apropos` creates `APROPOS-TOOL` which triggers the package lock. The pattern knowledge file correctly predicted this risk, but the plan still used the bare name. Fix: renamed to `apropos-search` in 2 minutes. Lesson: check `(find-symbol NAME :cl)` during planning, not just during implementation.

2. **Pure-tools pattern is now the default.** Three consecutive slices (011, 012, 013) with zero SWANK reworks for child-worker tools. The pattern is stable: child-worker function returns alist, `connection-eval` sends, `read-from-string` receives. Direct SWANK protocol usage (debug, interrupt) is the exception.

3. **HyperSpec Map files are trivially parseable.** Two-line alternating format, ASCII, no edge cases. Lazy loading into hash tables works well — no measurable startup cost.

4. **Count assertion drift is mechanical but wide.** Updating tool/resource counts touched 5 test files across 3 prior slices. The drift-detector pattern (PAT-2) works as intended — but the fix is always the same: find-and-replace the old count. Consider a single canonical count assertion rather than per-slice assertions.

## User feedback since delivery

*(to be filled 2–4 weeks after release)*

## Backlog and roadmap changes

- Completed: backlog item #9
- No new items added
- No reprioritization
