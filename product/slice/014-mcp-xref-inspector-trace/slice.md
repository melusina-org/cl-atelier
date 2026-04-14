# Slice 014: MCP xref, CLOS inspector, trace

**Status:** Complete
**Type:** New capability
**Goal addressed:** G5
**Backlog items:** #10
**Planned start / end:** 2026-04-13 / 2026-04-13
**Actual end:** 2026-04-14
**Implementation phases:**
  - Phase 1: product/slice/014-mcp-xref-inspector-trace/implementation-1.md — Complete

---

## What changes for users

An AI agent can now ask "who calls this function?", "who references this variable?",
"who specializes on this class?", and "what tests cover this function?" — then run
exactly those impacted tests. The agent can also inspect CLOS class hierarchies in
detail (superclasses, subclasses, slots with accessors, methods) and toggle tracing on
functions to observe runtime behavior. This completes the introspection surface needed
for safe refactoring.

## Specification references

- SBCL `sb-introspect` package: `who-calls`, `who-references`, `who-binds`,
  `who-specializes`, `who-macroexpands`
- SBCL `trace`/`untrace` (CL standard, SBCL extensions for `:methods`)
- MOP via `closer-mop`: `class-direct-superclasses`, `class-direct-subclasses`,
  `class-slots`, `slot-definition-name`, `slot-definition-readers`,
  `slot-definition-writers`, `specializer-direct-generic-functions`

## Stories

### S1: Who-calls
**In order to** understand function usage before renaming or removing it,
**a** developer **can** query who calls a given function.
**Acceptance criteria:**
- Given a loaded system, when `who-calls` is invoked with `"atelier:lint-system"`, then result lists callers with source file and package
- Given an undefined symbol, when `who-calls` is invoked, then result is empty list (not error)

### S2: Who-references
**In order to** understand variable usage,
**a** developer **can** query who references a given global variable.
**Acceptance criteria:**
- Given a loaded system, when `who-references` is invoked with a known special variable, then result lists referencing functions
- Given an unknown symbol, when `who-references` is invoked, then result is empty list

### S3: Who-binds
**In order to** find all rebinding sites of a special variable,
**a** developer **can** query who binds a given variable.
**Acceptance criteria:**
- Given a loaded system, when `who-binds` is invoked with a known special variable, then result lists binding sites

### S4: Who-specializes
**In order to** understand method coverage on a class before modifying its protocol,
**a** developer **can** query which methods specialize on a given class.
**Acceptance criteria:**
- Given a loaded system, when `who-specializes` is invoked with a class name, then result lists method names with qualifiers and specializer lists

### S5: Who-macroexpands
**In order to** find all call sites of a macro before changing its expansion,
**a** developer **can** query who macroexpands a given macro.
**Acceptance criteria:**
- Given a loaded system, when `who-macroexpands` is invoked with a known macro, then result lists expansion sites

### S6: CLOS class inspector
**In order to** understand a class hierarchy before refactoring,
**a** developer **can** inspect a CLOS class in detail.
**Acceptance criteria:**
- Given a class name, when `inspect-class` is invoked, then result includes: class name, direct superclasses, direct subclasses, slots (name, readers, writers, initargs, type, initform), direct methods (name, qualifiers, specializers)
- Given a non-class symbol, when `inspect-class` is invoked, then error message returned

### S7: Trace/untrace
**In order to** observe runtime behavior of functions during debugging,
**a** developer **can** toggle tracing on functions and retrieve trace output.
**Acceptance criteria:**
- Given a function name, when `trace-function` is invoked, then the function is traced in the child and confirmation returned
- Given a traced function, when `untrace-function` is invoked, then tracing is removed
- Given traced functions, when `eval-form` is called with code exercising them, then trace output appears in the eval result's stdout

### S8: Who-tests
**In order to** run only the tests affected by a code change,
**a** developer **can** query which Confidence testcases transitively call a given function.
**Acceptance criteria:**
- Given a function used by at least one testcase, when `who-tests` is invoked, then result lists testcase names that transitively call the function
- Given a function not called by any testcase, when `who-tests` is invoked, then result is empty list

### S9: Run-impacted
**In order to** verify a change quickly without running the full suite,
**a** developer **can** run exactly the testcases identified by `who-tests`.
**Acceptance criteria:**
- Given a function name, when `run-impacted` is invoked, then it discovers impacted testcases via who-tests, runs each, and returns per-testcase pass/fail results
- Given a function with no impacted tests, when `run-impacted` is invoked, then result indicates no tests found

## Quality Criteria

- [ ] All tools follow child-worker pattern (child-worker data function + thin MCP wrapper)
- [ ] All `#+sbcl` guards present for sb-introspect calls
- [ ] Full test suite passes (base atelier + MCP + editor) in fresh SBCL
- [ ] CLAUDE.md tool/resource counts updated
- [ ] No orphan child processes after test suite

## Definition of Ready

- [x] Stories traceable to backlog items
- [x] Stories sized <= 2 days each
- [x] Acceptance criteria written
- [x] Quality criterion defined
- [x] Spec references identified

## Definition of Done

- [ ] All stories complete with acceptance criteria passing
- [ ] Quality criteria passing
- [ ] Full test suite passes
- [ ] All implementation phases have completion notes
- [ ] `product/slice/014-mcp-xref-inspector-trace/retrospective.md` created
- [ ] `product/backlog.md` updated
- [ ] `product/roadmap.md` updated
