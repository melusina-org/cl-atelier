# Slice 012: MCP ASDF, Quicklisp, and Confidence integration

**Status:** Complete
**Type:** New capability
**Goal addressed:** G5
**Backlog items:** #8
**Planned start / end:** 2026-04-13 / TBD
**Actual end:** 2026-04-13
**Implementation phases:**
  - Phase 1: `product/slice/012-mcp-asdf-quicklisp-confidence/implementation-1.md` — Planned

---

## What changes for users

After this slice, an MCP client connected to `atelier-mcp` can:

- **Load systems via Quicklisp.** `atelier:quickload` loads a system in the child image, handling missing dependencies via Quicklisp. This is the primary way an agent loads code.
- **Get system details.** `atelier:system-info` returns a system's dependencies, components, version, author, license, and source directory.
- **Search for systems.** `atelier:system-apropos` finds systems whose names match a substring.
- **Discover testcases.** `atelier:list-testcases` returns all Confidence testcases defined in a package, leveraging the `define-testcase` symbol property.
- **Run individual testcases.** `atelier:run-testcase` runs a specific Confidence testcase by name and returns structured pass/fail/error counts.

**What this slice does not ship:**

- `asdf-operate` as a generic tool — `quickload` and `run-tests-*` cover the common operations. A generic `asdf-operate` would need careful sandboxing.
- `confidence.run-impacted` — requires xref from slice 014.

## Stories

### S1 — `atelier:quickload` tool

**In order to** load a system and its dependencies in the child image, **an** agent **can** call `atelier:quickload` with a system name.

**Acceptance criteria:**
- Given a system available via Quicklisp, when `quickload` is called, then the system is loaded in the child and the tool returns `{system, success, output, duration-ms}`.
- Given a system that fails to load, when `quickload` is called, then the response has `success: false` with the error captured in output.
- Given the child has no active debugger, when `quickload` is called and loading enters the debugger, then the debug state is auto-aborted (quickload is not an interactive eval — it should not leave the agent in the debugger).

### S2 — `atelier:system-info` tool

**In order to** understand a system's structure before modifying it, **an** agent **can** call `atelier:system-info` with a system name.

**Acceptance criteria:**
- Given a system registered with ASDF, when `system-info` is called, then the response includes: `name`, `version`, `author`, `license`, `description`, `source-directory`, `depends-on` (list of dependency names), `components` (list of component names with types).
- Given a system that is not registered, when `system-info` is called, then the response is an error with "system not found".

### S3 — `atelier:system-apropos` tool

**In order to** find systems matching a pattern, **an** agent **can** call `atelier:system-apropos` with a search string.

**Acceptance criteria:**
- Given a search string like `"atelier"`, when `system-apropos` is called, then the response is a list of matching system names from the ASDF source registry.
- Given a search string with no matches, when called, then the response is an empty list.

### S4 — `atelier:list-testcases` tool

**In order to** discover what tests exist for a system, **an** agent **can** call `atelier:list-testcases` with a package name.

**Acceptance criteria:**
- Given a package with Confidence testcases (symbols with the `confidence::testcase` property), when `list-testcases` is called, then the response is a list of testcase names with their documentation strings.
- Given Atelier's own testsuite package, when called, then the list includes `run-all-tests` and other known testcases.
- Given a package with no testcases, when called, then the response is an empty list.

### S5 — `atelier:run-testcase` tool

**In order to** run a specific test rather than the whole suite, **an** agent **can** call `atelier:run-testcase` with a testcase designator.

**Acceptance criteria:**
- Given a valid testcase designator like `"atelier/testsuite:validate-parameter-replace"`, when `run-testcase` is called, then the tool evaluates `(TESTCASE-SYMBOL)` in the child and returns `{name, total, success, failure, condition, outcome}`.
- Given a non-existent testcase, when called, then the response is an error.

### S6 — Tests

**Acceptance criteria:**
- Full regression passes: 647 + new assertions.
- No orphan SBCL processes.
- Tool count increases from 18 to 23 (5 new tools).

## Quality Criteria

- [ ] Full regression passes in fresh SBCL subprocess
- [ ] No orphan SBCL processes
- [ ] No new ASDF dependencies
- [ ] All new tools run in the child via `connection-eval` — no direct SWANK calls

## Definition of Ready

- [x] Stories traceable to backlog item #8
- [x] Stories sized ≤ 2 days each
- [x] Acceptance criteria written
- [x] Quality criteria defined
- [x] Spec references identified

## Definition of Done

- [ ] All stories complete with acceptance criteria passing
- [ ] Quality criteria passing
- [ ] Full test suite passes in fresh SBCL subprocess
- [ ] `product/slice/012-mcp-asdf-quicklisp-confidence/retrospective.md` created
- [ ] `product/backlog.md` updated
- [ ] `product/roadmap.md` updated
- [ ] `CLAUDE.md` updated
