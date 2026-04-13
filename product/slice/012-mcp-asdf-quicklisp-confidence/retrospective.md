# Retrospective: Slice 012 — MCP ASDF, Quicklisp, and Confidence

**Recorded:** 2026-04-13
**Implementation phases:** 1

## Delivery Summary

- Stories delivered: 6 of 6
- Acceptance criteria: all passing
- Quality criteria: all passing
- Full test suite: 659/659 (100%)

## Phase Notes Summary

| Phase | Key deliverables | Deferred to backlog |
|-------|-----------------|---------------------|
| 1 | 5 new tools (quickload, system-info, system-apropos, list-testcases, run-testcase), 5 child-worker functions, Confidence testcase discovery via `:org.melusina.confidence/testcase` property, system-apropos runs in parent. 12 new assertions. | None |

## Goal Progress

- **G5**: closer — ASDF operations, Quicklisp, and Confidence integration delivered. 23 tools total.

## What we learned

1. **Confidence uses `:ORG.MELUSINA.CONFIDENCE/TESTCASE` as its property key**, not `confidence::testcase`. The `define-testcase` macro calls `set-testcase-properties` which sets this keyword property. Discovery by macroexpanding `define-testcase` and inspecting `symbol-plist`.

2. **System search must cover both `*source-registry*` and `registered-systems`.** The ASDF source registry (`asdf/source-registry:*source-registry*`) only contains systems found by scanning directories. Systems loaded via `*central-registry*` are found by `asdf:registered-systems` instead. Both must be searched for complete results.

3. **Pure tools slice went smoothly.** No protocol changes, no SWANK surprises. All 5 tools are thin wrappers over child-worker functions called via `connection-eval`. The pattern is now well-established: child-worker function returns an alist, `connection-eval` sends via SWANK, tool reads the result with `read-from-string`.

## User feedback since delivery

*(to be filled 2–4 weeks after release)*

## Backlog and roadmap changes

- Completed: backlog item #8
- No new items added
- No reprioritization
