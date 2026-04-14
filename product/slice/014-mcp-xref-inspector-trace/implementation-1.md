# Implementation Phase 1: Slice 014 — MCP xref, CLOS inspector, trace

**Slice:** product/slice/014-mcp-xref-inspector-trace/slice.md
**Phase:** 1
**Scope:** All 9 stories — xref tools (S1–S5), CLOS inspector (S6), trace/untrace (S7), who-tests (S8), run-impacted (S9)
**Prerequisites:** Slice 013 complete, 695/695 tests passing

## Back-link

Slice: product/slice/014-mcp-xref-inspector-trace/slice.md

## Prior phases

None — this is the first and only phase.

## Project Knowledge Applied

- **PAT-16 (pure tools pattern):** All tools follow child-worker data function + thin MCP wrapper. Zero SWANK reworks for 3 consecutive slices using this pattern.
- **PAT-15 (CL package lock on tool names):** Check `(find-symbol NAME :cl)` for every tool name. Checked: `who-calls`, `who-references`, `who-binds`, `who-specializes`, `who-macroexpands`, `inspect-class`, `trace-function`, `untrace-function`, `who-tests`, `run-impacted` — none shadow CL exports.
- **INV-4:** Regression verification in fresh SBCL subprocess.
- **INV-14:** Every `define-tool` traces to exported symbol with acceptance criterion.

## Risk Register

| # | Risk | Category | Mitigation |
|---|------|----------|-----------|
| R1 | `sb-introspect:who-*` returns `definition-source` objects that may have nil pathnames | API uncertainty | Return empty list when pathname is nil; test with built-in function |
| R2 | `who-tests` transitive closure may be expensive on large systems | Performance | Limit depth; accept that first version returns direct callers with testcase property only (document limitation) |
| R3 | `trace`/`untrace` are macros, not functions — cannot call from SWANK eval directly | CL semantics | Use `(eval (list 'trace sym))` pattern in child-worker |
| R4 | CLOS `class-direct-subclasses` may include anonymous classes | Data shape | Filter to named classes only |
| R5 | Tool count assertions in 4 test files need updating (30 → 39) | Documentation drift (PAT-2) | Update all count assertions in same step as tool registration |

## OSS Components

None — all functionality uses SBCL built-ins (`sb-introspect`, `trace`/`untrace`) and `closer-mop` (already a dependency).

## Phase Scope

**Stories covered:** S1–S9 (all)
**Stories deferred:** None

## File Organisation

```
src/child-worker/
  introspection.lisp          [modify] — add xref-data, inspect-class-data, trace/untrace-data, who-tests-data
  package.lisp                [modify] — export new data functions

src/mcp/tools/
  who-calls.lisp              [new] — who-calls tool
  who-references.lisp         [new] — who-references tool
  who-binds.lisp              [new] — who-binds tool
  who-specializes.lisp        [new] — who-specializes tool
  who-macroexpands.lisp       [new] — who-macroexpands tool
  inspect-class.lisp          [new] — inspect-class tool
  trace-function.lisp         [new] — trace-function tool
  untrace-function.lisp       [new] — untrace-function tool
  who-tests.lisp              [new] — who-tests tool
  run-impacted.lisp           [new] — run-impacted tool

testsuite/mcp/
  xref-tools-tests.lisp       [new] — tests for S1–S5, S8, S9
  inspect-trace-tests.lisp    [new] — tests for S6, S7
  registry-counts.lisp        [modify] — update counts 30 → 39 (tools was 30, now 39 with 9 new; +1 concrete resource: none; +0 templates)
  child-tests.lisp            [modify] — update tool registration count 30 → 39
  asdf-tools-tests.lisp       [modify] — update count assertion
  documentation-tools-tests.lisp [modify] — update count assertion
  entrypoint.lisp             [modify] — wire new test runners
  package.lisp                [modify] — export new test runner if needed (not needed, uses run-mcp-tests)

org.melusina.atelier.asd      [modify] — add new tool files and test files
CLAUDE.md                     [modify] — update tool count 30 → 39
```

Note: 9 new tools (who-calls, who-references, who-binds, who-specializes, who-macroexpands, inspect-class, trace-function, untrace-function, who-tests) plus run-impacted which calls who-tests + run-testcase internally = 10 new tools. Wait — let me recount: S1 who-calls, S2 who-references, S3 who-binds, S4 who-specializes, S5 who-macroexpands, S6 inspect-class, S7 trace-function + untrace-function (2 tools), S8 who-tests, S9 run-impacted = **10 new tools, total 40**.

Correction: 10 new tools, 30 + 10 = 40 total.

## Build System Changes

Add to `org.melusina.atelier/child-worker` — no new files, just `introspection.lisp` modifications.

Add to `org.melusina.atelier/mcp` tools module:
```lisp
(:file "who-calls")
(:file "who-references")
(:file "who-binds")
(:file "who-specializes")
(:file "who-macroexpands")
(:file "inspect-class")
(:file "trace-function")
(:file "untrace-function")
(:file "who-tests")
(:file "run-impacted")
```

Add to `org.melusina.atelier/testsuite` mcp module:
```lisp
(:file "xref-tools-tests")
(:file "inspect-trace-tests")
```

## Package / Module Architecture

**New exports from `atelier/child-worker`:**
- `who-calls-data`
- `who-references-data`
- `who-binds-data`
- `who-specializes-data`
- `who-macroexpands-data`
- `inspect-class-data`
- `trace-function-data`
- `untrace-function-data`
- `who-tests-data`
- `run-impacted-data`

**No new exports from `atelier/mcp`** — tools are registered via `define-tool`, no new protocol-level exports needed.

## Type / Class Hierarchy

No new types. All tools return alists.

## Protocol Definitions

All child-worker functions follow the established pattern:
```lisp
(defun <name>-data (designator) → alist)
```

## Error / Condition Types

No new conditions. Reuse `mcp-error` for invalid inputs.

## Test Plan

| Story | Test name | Category | Skip condition |
|-------|-----------|:--------:|----------------|
| S1 | `validate-who-calls` | slow | `#-sbcl` |
| S1 | `validate-who-calls-unknown` | slow | `#-sbcl` |
| S2 | `validate-who-references` | slow | `#-sbcl` |
| S3 | `validate-who-binds` | slow | `#-sbcl` |
| S4 | `validate-who-specializes` | slow | `#-sbcl` |
| S5 | `validate-who-macroexpands` | slow | `#-sbcl` |
| S6 | `validate-inspect-class` | slow | `#-sbcl` |
| S6 | `validate-inspect-class-non-class` | slow | `#-sbcl` |
| S7 | `validate-trace-function` | slow | `#-sbcl` |
| S7 | `validate-untrace-function` | slow | `#-sbcl` |
| S7 | `validate-trace-output-in-eval` | slow | `#-sbcl` |
| S8 | `validate-who-tests` | slow | `#-sbcl` |
| S8 | `validate-who-tests-no-callers` | slow | `#-sbcl` |
| S9 | `validate-run-impacted` | slow | `#-sbcl` |
| S9 | `validate-run-impacted-no-tests` | slow | `#-sbcl` |
| all | `validate-xref-tool-count` | fast | — |

## Implementation Order (Step Table)

| Step | File | Action | Form(s) | Test name | Category |
|------|------|--------|---------|-----------|:--------:|
| 1 | `src/child-worker/introspection.lisp` | modify | `who-calls-data`, `who-references-data`, `who-binds-data`, `who-specializes-data`, `who-macroexpands-data` | — (tested via tools) | — |
| 2 | `src/child-worker/introspection.lisp` | modify | `inspect-class-data` | — | — |
| 3 | `src/child-worker/introspection.lisp` | modify | `trace-function-data`, `untrace-function-data` | — | — |
| 4 | `src/child-worker/introspection.lisp` | modify | `who-tests-data`, `run-impacted-data` | — | — |
| 5 | `src/child-worker/package.lisp` | modify | export all new symbols | — | — |
| 6 | `src/mcp/tools/who-calls.lisp` | new | `define-tool who-calls` | — | — |
| 7 | `src/mcp/tools/who-references.lisp` | new | `define-tool who-references` | — | — |
| 8 | `src/mcp/tools/who-binds.lisp` | new | `define-tool who-binds` | — | — |
| 9 | `src/mcp/tools/who-specializes.lisp` | new | `define-tool who-specializes` | — | — |
| 10 | `src/mcp/tools/who-macroexpands.lisp` | new | `define-tool who-macroexpands` | — | — |
| 11 | `src/mcp/tools/inspect-class.lisp` | new | `define-tool inspect-class` | — | — |
| 12 | `src/mcp/tools/trace-function.lisp` | new | `define-tool trace-function` | — | — |
| 13 | `src/mcp/tools/untrace-function.lisp` | new | `define-tool untrace-function` | — | — |
| 14 | `src/mcp/tools/who-tests.lisp` | new | `define-tool who-tests` | — | — |
| 15 | `src/mcp/tools/run-impacted.lisp` | new | `define-tool run-impacted` | — | — |
| 16 | `org.melusina.atelier.asd` | modify | add tool files + test files to ASDF | — | — |
| 17 | `testsuite/mcp/xref-tools-tests.lisp` | new | all xref + who-tests + run-impacted tests | `run-xref-tools-tests` | slow |
| 18 | `testsuite/mcp/inspect-trace-tests.lisp` | new | inspect-class + trace/untrace tests | `run-inspect-trace-tests` | slow |
| 19 | `testsuite/mcp/registry-counts.lisp` | modify | update 30 → 40 | `validate-registry-counts` | fast |
| 20 | `testsuite/mcp/child-tests.lisp` | modify | update count 30 → 40 | — | fast |
| 21 | `testsuite/mcp/asdf-tools-tests.lisp` | modify | update count 30 → 40 | — | fast |
| 22 | `testsuite/mcp/documentation-tools-tests.lisp` | modify | update count 30 → 40 | — | fast |
| 23 | `testsuite/mcp/entrypoint.lisp` | modify | wire `run-xref-tools-tests`, `run-inspect-trace-tests`, `validate-xref-tool-count` | — | — |
| 24 | `CLAUDE.md` | modify | update tool count 30 → 40 | — | — |

## Invariants

Carry forward all prior invariants (INV-1 through INV-35).

New:
- **INV-36:** `sb-introspect:who-*` results with nil pathname are filtered out (empty list, not error).
- **INV-37:** `trace-function`/`untrace-function` use `(eval '(trace SYM))` pattern because `trace` is a macro.
- **INV-38:** `who-tests` performs direct-caller intersection with testcase registry, not full transitive closure. Documented limitation.

## Test Fixtures

No new fixture files. All tests use live child evaluation.

## References to Create

None needed — `sb-introspect` API is stable and well-documented.

## Acceptance Criteria

1. `who-calls` with `"atelier:lint-system"` returns non-empty list with source file information
2. `who-calls` with undefined symbol returns empty list
3. `who-references` with known special variable returns referencing functions
4. `who-binds` with known special variable returns binding sites
5. `who-specializes` with a known class returns method information
6. `who-macroexpands` with a known macro returns expansion sites
7. `inspect-class` with a class name returns superclasses, subclasses, slots, methods
8. `inspect-class` with a non-class symbol returns error
9. `trace-function` toggles tracing; trace output appears in `eval-form` stdout
10. `untrace-function` removes tracing
11. `who-tests` for a function called by testcases returns testcase names
12. `run-impacted` discovers and runs impacted tests, returns per-testcase results
13. All 40 tools registered
14. Full test suite passes in fresh SBCL
15. CLAUDE.md updated with correct tool count
16. No orphan child processes

## Phase Closure Conditions

- All 16 test cases pass
- All 24 implementation steps complete
- Full test suite (`run-all-tests`) passes
- `validate-mcp-system-loads-cleanly` passes (fresh SBCL)
- CLAUDE.md tool count updated to 40
- All count assertions updated (4 files)
