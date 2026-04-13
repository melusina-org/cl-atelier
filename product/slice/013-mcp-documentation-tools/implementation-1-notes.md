# Implementation Phase 1 Notes: Slice 013 — MCP Documentation Tools

**Phase:** 1
**Plan:** product/slice/013-mcp-documentation-tools/implementation-1.md
**Recorded:** 2026-04-13
**Status:** Complete

## Stories delivered in this phase

- S1: Apropos search across all packages — all acceptance criteria passed
- S2: HyperSpec symbol lookup — all acceptance criteria passed
- S3: HyperSpec symbol lookup as resource — all acceptance criteria passed
- S4: X3J13 issue lookup — all acceptance criteria passed
- S5: X3J13 issues as resources — all acceptance criteria passed
- S6: Macroexpand — all acceptance criteria passed
- S7: Disassemble — all acceptance criteria passed
- S8: Compile with notes — all acceptance criteria passed

## Acceptance criteria results

| # | Criterion | Result | Verified by |
|---|-----------|:------:|-------------|
| AC1 | apropos-search no filter returns multi-package results | Pass | validate-apropos-no-filter |
| AC2 | apropos-search with package filter returns CL-only | Pass | validate-apropos-package-filter |
| AC3 | hyperspec-lookup MAPCAR returns HTML | Pass | validate-hyperspec-lookup-known |
| AC4 | hyperspec-lookup unknown symbol returns not-found | Pass | validate-hyperspec-lookup-unknown |
| AC5 | lisp://hyperspec/symbol/{name} resource returns HTML | Pass | validate-hyperspec-symbol-resource |
| AC6 | hyperspec-issue ADJUST-ARRAY-DISPLACEMENT returns HTML | Pass | validate-hyperspec-issue-known |
| AC7 | hyperspec-issue unknown returns not-found | Pass | validate-hyperspec-issue-unknown |
| AC8 | lisp://hyperspec/issues returns 365 issue names | Pass | validate-hyperspec-issues-list |
| AC9 | macroexpand-form single step | Pass | validate-macroexpand-form |
| AC10 | macroexpand-form fully | Pass | validate-macroexpand-form-fully |
| AC11 | disassemble-symbol CL:CAR returns assembly | Pass | validate-disassemble-symbol |
| AC12 | disassemble-symbol unknown returns error | Pass | validate-disassemble-symbol-unknown |
| AC13 | compile-form clean returns 0 diagnostics | Pass | validate-compile-form-clean |
| AC14 | compile-form with type issue returns diagnostics | Pass | validate-compile-form-with-warning |
| AC15 | Full test suite passes in fresh SBCL | Pass | 695/695 |
| AC16 | CLAUDE.md updated | Pass | 30 tools, 4 concrete, 7 template |

## Test results

- Fast: ~660 passed
- Slow: ~35 passed, 0 skipped
- Snail: N/A

## Invariants established or confirmed

- INV-33: HyperSpec tools read only from the local filesystem (confirmed)
- INV-34: HyperSpec tools gracefully unavailable when not installed (confirmed)
- INV-35: Tool names never shadow CL exports (confirmed — apropos renamed)
- Invariants 1–32: confirmed

## Deferred items (for next phase)

None. All stories delivered.

## Reworks performed

- **apropos → apropos-search**: CL package lock violation on `CL:APROPOS`
  when interning `APROPOS-TOOL`. Renamed tool to `apropos-search`.
  Trigger: PAT-11 (CL package lock on define-tool names). Impact: minor
  (file rename + test update, ~2 minutes).

## New risks discovered

None.

## Technical decisions made

- **Tool name `apropos-search`**: chosen over `symbol-apropos` or
  `find-symbols` to match the CL function name while avoiding the
  package lock. The `-search` suffix is unambiguous.
- **HyperSpec HTML served raw**: no HTML-to-text stripping. MCP clients
  (AI agents) handle HTML extraction better than a lossy server-side
  transform would.
- **Map files parsed lazily**: `*hyperspec-symbol-map*` and
  `*hyperspec-issue-map*` loaded on first use, not at system load time.
  Avoids startup cost when HyperSpec tools aren't used.

## Notes for Strategist retrospective

- Stories remaining: none
- Snail tests requiring confirmation: none
- Total tools: 30 (was 23). Total resources: 11 (was 8).
- Test count: 695 (was 659, delta +36).
- Pure-tools pattern confirmed again: zero SWANK reworks for
  child-worker tools, matching slice 012 experience.
