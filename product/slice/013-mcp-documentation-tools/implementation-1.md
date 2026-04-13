# Implementation Phase 1: Slice 013 — MCP Documentation Tools

**Phase:** 1
**Slice:** product/slice/013-mcp-documentation-tools/slice.md
**Scope:** All 8 stories — 6 new tools, 3 new resources, 4 child-worker functions, 1 HyperSpec infrastructure module.

## Prior phases

None. This is phase 1.

## Project Knowledge Applied

- **PAT-11 (CL package lock on define-tool names):** Tool names
  `macroexpand` and `disassemble` would create classes
  `MACROEXPAND-TOOL` and `DISASSEMBLE-TOOL`. While the `-TOOL` suffix
  avoids direct shadowing, we use `macroexpand-form` and
  `disassemble-symbol` to eliminate any ambiguity with CL exports.
- **PAT-12 (Pure tools pattern):** Child tools (apropos, macroexpand,
  disassemble, compile-form) follow child-worker function + thin MCP
  wrapper. Zero SWANK reworks expected.
- **PAT-6 (Test-registry pollution):** New tool registrations in tests
  use `with-isolated-registries` (INV-16).
- **PAT-1 (Stale-fasl masking):** Final verification in fresh SBCL
  subprocess (INV-4).
- **PAT-7 (Skim-then-code):** Maker must re-read MEMORY.md and
  knowledge files before Phase 2.

## Risk Register

| # | Risk | Category | Mitigation |
|---|------|----------|-----------|
| R1 | HyperSpec not installed on target machine | Test dependency | Skip HyperSpec tests when `Map_Sym.txt` absent. Guard with a `hyperspec-available-p` check. |
| R2 | HTML content too large for MCP response | Scope boundary | Body pages can be large. Return raw HTML; the MCP client handles rendering. If problematic, truncate with a note. |
| R3 | Multiple symbols per HyperSpec page (e.g. MAPC/MAPCAR share `f_mapc_.htm`) | Library API | Map_Sym.txt maps each symbol to the correct page. Multiple symbols mapping to the same file is fine — the page covers all of them. |
| R4 | `compile-form` side-effects in child image | State/lifecycle | Use `compile nil (lambda () FORM)` pattern to avoid defining functions. Compiler notes are captured via handler-bind. |
| R5 | CL package lock on tool names | Phase boundary | Already mitigated: use `macroexpand-form`, `disassemble-symbol`, `compile-form` (none shadow CL exports). |

## OSS Components

None. All functionality uses SBCL built-ins and local HyperSpec files.

## Phase Scope

**Stories covered:** S1 through S8 (all).

**Stories deferred:** None.

## File Organisation

```
src/child-worker/
  documentation.lisp           [new]  — apropos-data, macroexpand-data,
                                        disassemble-data, compile-form-data

src/mcp/
  hyperspec.lisp               [new]  — Map file parsing, lookup functions,
                                        *hyperspec-root*, hyperspec-available-p

  tools/
    apropos.lisp               [new]  — S1: apropos tool (child)
    hyperspec-lookup.lisp      [new]  — S2+S3: hyperspec-lookup tool + resource
    hyperspec-issue.lisp       [new]  — S4+S5: hyperspec-issue tool + resource
    hyperspec-issues.lisp      [new]  — S5: hyperspec issues list (concrete resource)
    macroexpand-form.lisp      [new]  — S6: macroexpand-form tool (child)
    disassemble-symbol.lisp    [new]  — S7: disassemble-symbol tool (child)
    compile-form.lisp          [new]  — S8: compile-form tool (child)

testsuite/mcp/
  documentation-tools-tests.lisp [new] — tests for all 8 stories

org.melusina.atelier.asd       [modify] — add new components
CLAUDE.md                      [modify] — update tool/resource counts
```

## Build System Changes

### org.melusina.atelier/child-worker

Add after `(:file "introspection")`:
```lisp
(:file "documentation")
```

### org.melusina.atelier/mcp

Add before the `tools` module:
```lisp
(:file "hyperspec")
```

Add to the `tools` module after `(:file "run-testcase")`:
```lisp
(:file "apropos")
(:file "hyperspec-lookup")
(:file "hyperspec-issue")
(:file "hyperspec-issues")
(:file "macroexpand-form")
(:file "disassemble-symbol")
(:file "compile-form")
```

### org.melusina.atelier/testsuite

Add to the `mcp` submodule after `(:file "asdf-tools-tests")`:
```lisp
(:file "documentation-tools-tests")
```

## Package / Module Architecture

### atelier/child-worker (child image)

New exports from `src/child-worker/documentation.lisp`:
```lisp
apropos-data               ; (search-string &optional package-name) -> list of alists
macroexpand-data           ; (form-string &key fully) -> alist
disassemble-data           ; (designator) -> alist
compile-form-data          ; (form-string) -> alist
```

### atelier/mcp (parent image)

New exports from `src/mcp/hyperspec.lisp`:
```lisp
*hyperspec-root*           ; pathname — /opt/local/share/doc/lisp/HyperSpec-7-0/HyperSpec/
hyperspec-available-p      ; () -> boolean
hyperspec-symbol-lookup    ; (symbol-name) -> (values html-content relative-path) or NIL
hyperspec-issue-lookup     ; (issue-name) -> html-content or NIL
hyperspec-issue-names      ; () -> list of strings
```

No new exports needed for tool files (tools self-register via `define-tool`).

## Type / Class Hierarchy

No new classes beyond the auto-generated `*-TOOL` classes from `define-tool`.

## Protocol Definitions

All tools follow existing `define-tool` → `handle-tool-call` protocol.
No new generic functions.

## Error / Condition Types

No new condition types. Tools use existing `mcp-error` for failures.

## Test Plan

| Story | Test name | Category | Skip condition |
|-------|-----------|:--------:|----------------|
| S1 | `validate-apropos-no-filter` | fast | `#-sbcl` |
| S1 | `validate-apropos-package-filter` | fast | `#-sbcl` |
| S2 | `validate-hyperspec-lookup-known` | slow | `(not (hyperspec-available-p))` |
| S2 | `validate-hyperspec-lookup-unknown` | slow | `(not (hyperspec-available-p))` |
| S3 | `validate-hyperspec-symbol-resource` | slow | `(not (hyperspec-available-p))` |
| S4 | `validate-hyperspec-issue-known` | slow | `(not (hyperspec-available-p))` |
| S4 | `validate-hyperspec-issue-unknown` | slow | `(not (hyperspec-available-p))` |
| S5 | `validate-hyperspec-issues-list` | slow | `(not (hyperspec-available-p))` |
| S5 | `validate-hyperspec-issue-resource` | slow | `(not (hyperspec-available-p))` |
| S6 | `validate-macroexpand-form` | fast | `#-sbcl` |
| S6 | `validate-macroexpand-form-fully` | fast | `#-sbcl` |
| S7 | `validate-disassemble-symbol` | fast | `#-sbcl` |
| S7 | `validate-disassemble-symbol-unknown` | fast | `#-sbcl` |
| S8 | `validate-compile-form-clean` | fast | `#-sbcl` |
| S8 | `validate-compile-form-with-warning` | fast | `#-sbcl` |

Child-based tests (S1, S6-S8) are "fast" because the child is already
started by the shared test fixture — no new I/O. HyperSpec tests (S2-S5)
are "slow" because they read from the filesystem.

## Implementation Order (Step Table)

| Step | File | Action | Form(s) | Test name | Category |
|------|------|--------|---------|-----------|:--------:|
| 1 | `src/child-worker/documentation.lisp` [new] | Implement | `apropos-data` | `validate-apropos-no-filter`, `validate-apropos-package-filter` | fast |
| 2 | `src/child-worker/documentation.lisp` [modify] | Implement | `macroexpand-data` | `validate-macroexpand-form`, `validate-macroexpand-form-fully` | fast |
| 3 | `src/child-worker/documentation.lisp` [modify] | Implement | `disassemble-data` | `validate-disassemble-symbol`, `validate-disassemble-symbol-unknown` | fast |
| 4 | `src/child-worker/documentation.lisp` [modify] | Implement | `compile-form-data` | `validate-compile-form-clean`, `validate-compile-form-with-warning` | fast |
| 5 | `src/mcp/hyperspec.lisp` [new] | Implement | `*hyperspec-root*`, `hyperspec-available-p`, `%parse-map-file`, `hyperspec-symbol-lookup`, `hyperspec-issue-lookup`, `hyperspec-issue-names` | `validate-hyperspec-lookup-known`, `validate-hyperspec-lookup-unknown` | slow |
| 6 | `src/mcp/tools/apropos.lisp` [new] | Implement | `define-tool apropos` | `validate-apropos-no-filter` | fast |
| 7 | `src/mcp/tools/hyperspec-lookup.lisp` [new] | Implement | `define-tool hyperspec-lookup` + resource | `validate-hyperspec-lookup-known`, `validate-hyperspec-symbol-resource` | slow |
| 8 | `src/mcp/tools/hyperspec-issue.lisp` [new] | Implement | `define-tool hyperspec-issue` + resource | `validate-hyperspec-issue-known`, `validate-hyperspec-issue-resource` | slow |
| 9 | `src/mcp/tools/hyperspec-issues.lisp` [new] | Implement | `define-tool hyperspec-issues` (concrete resource + tool) | `validate-hyperspec-issues-list` | slow |
| 10 | `src/mcp/tools/macroexpand-form.lisp` [new] | Implement | `define-tool macroexpand-form` | `validate-macroexpand-form` | fast |
| 11 | `src/mcp/tools/disassemble-symbol.lisp` [new] | Implement | `define-tool disassemble-symbol` | `validate-disassemble-symbol` | fast |
| 12 | `src/mcp/tools/compile-form.lisp` [new] | Implement | `define-tool compile-form` | `validate-compile-form-clean` | fast |
| 13 | `testsuite/mcp/documentation-tools-tests.lisp` [new] | Implement | `run-documentation-tools-tests`, all 15 test cases | all | fast+slow |
| 14 | `org.melusina.atelier.asd` [modify] | Wire | Add components to child-worker, mcp, testsuite systems | (load test) | fast |
| 15 | `CLAUDE.md` [modify] | Update | Tool count 23→29, resource count 8→11 | — | — |

**Notes on step ordering:**
- Steps 1-4 build the child-worker functions. Each is testable in
  isolation via `connection-eval` once the test harness is up.
- Step 5 builds the parent-side HyperSpec infrastructure. Independent
  of steps 1-4.
- Steps 6-12 are thin tool wrappers — each depends on either steps 1-4
  (child tools) or step 5 (HyperSpec tools).
- Step 13 is the test file — written progressively alongside steps 1-12,
  but listed last because the full runner depends on all tools existing.
- Step 14 wires everything into ASDF.
- Step 15 updates documentation.

**Practical execution:** Steps 1-4 and 5 are independent tracks.
Steps 6-12 each depend on their respective infrastructure step.
The Maker should implement in table order, writing tests in step 13
as each tool is completed.

## Invariants

Carried forward from prior slices (all confirmed):

- **INV-1:** Every finding subclass has exactly one inspector that produces it.
- **INV-2:** Per-maintainer N=1 self-idempotency.
- **INV-3:** Pretty-printer is the single authority on canonical Lisp text.
- **INV-4:** Regression verification must use fresh SBCL subprocess.
- **INV-5:** Atelier does not police line length.
- **INV-6:** One finding subclass per inspector category.
- **INV-7:** Atelier does not police line length (design principle).
- **INV-8:** File write-back is atomic (tmpize + rename).
- **INV-9:** Templates are loaded from `*resourcedir*` at initialize time.
- **INV-10:** License repository keyed by keyword.
- **INV-11:** SPDX identifiers are the only license identification scheme.
- **INV-12:** MCP protocol version is `"2024-11-05"`.
- **INV-13:** Tool names are `"atelier:<kebab>"` for Atelier tools, `"lisp:<kebab>"` for Lisp-generic tools.
- **INV-14:** Every MCP tool is traceable to an acceptance criterion.
- **INV-15:** Transcript entries are append-only plists.
- **INV-16:** Tests calling `define-tool` must use `with-isolated-registries`.
- **INV-17:** Editor loads without MCP dependencies.
- **INV-18:** `eval-form` captures stdout and stderr separately.
- **INV-19:** Child-worker functions return alist structures.
- **INV-20:** `connection-eval` returns a string; caller uses `read-from-string`.
- **INV-21:** Debug state is per-connection, not global.
- **INV-22:** `canonicalize-form` runs in parent, no child needed.
- **INV-23–INV-32:** (Various from slices 005–012.)

New invariants for this phase:

- **INV-33:** HyperSpec tools read only from the local filesystem; they
  never make network requests.
- **INV-34:** HyperSpec tools are gracefully unavailable when the local
  HyperSpec installation is absent (`hyperspec-available-p` returns NIL).
- **INV-35:** Tool names never shadow symbols exported from the
  `COMMON-LISP` package (confirmed by PAT-11).

## Test Fixtures

No fixture files needed. HyperSpec tests read from the installed
HyperSpec. Child tests use the shared `with-test-child` fixture.

## References to Create

None. The HyperSpec is self-documenting; the SWANK and MCP patterns are
established in prior slice references.

## Acceptance Criteria

| # | Criterion | Story | Verified by |
|---|-----------|-------|-------------|
| AC1 | `apropos` with no package filter returns symbols from multiple packages | S1 | `validate-apropos-no-filter` |
| AC2 | `apropos` with package filter returns only symbols from that package | S1 | `validate-apropos-package-filter` |
| AC3 | `hyperspec-lookup` for `"MAPCAR"` returns non-empty HTML content from local file | S2 | `validate-hyperspec-lookup-known` |
| AC4 | `hyperspec-lookup` for unknown symbol returns not-found indication | S2 | `validate-hyperspec-lookup-unknown` |
| AC5 | `lisp://hyperspec/symbol/{name}` resource returns HTML content | S3 | `validate-hyperspec-symbol-resource` |
| AC6 | `hyperspec-issue` for `"ADJUST-ARRAY-DISPLACEMENT"` returns issue HTML | S4 | `validate-hyperspec-issue-known` |
| AC7 | `hyperspec-issue` for unknown issue returns not-found indication | S4 | `validate-hyperspec-issue-unknown` |
| AC8 | `lisp://hyperspec/issues` resource returns list of 365 issue names | S5 | `validate-hyperspec-issues-list` |
| AC9 | `macroexpand-form` with `fully: false` returns single-step expansion | S6 | `validate-macroexpand-form` |
| AC10 | `macroexpand-form` with `fully: true` returns full expansion | S6 | `validate-macroexpand-form-fully` |
| AC11 | `disassemble-symbol` for `"CL:CAR"` returns assembly output | S7 | `validate-disassemble-symbol` |
| AC12 | `disassemble-symbol` for nonexistent function returns error indication | S7 | `validate-disassemble-symbol-unknown` |
| AC13 | `compile-form` for clean form returns no diagnostics | S8 | `validate-compile-form-clean` |
| AC14 | `compile-form` for form with type issue returns compiler notes | S8 | `validate-compile-form-with-warning` |
| AC15 | Full test suite passes in fresh SBCL subprocess | All | `run-tests-fresh` equivalent |
| AC16 | CLAUDE.md reflects updated tool count (29) and resource count (11) | All | Manual inspection |

## Phase Closure Conditions

- All 15 test cases pass (fast tests unconditionally; slow tests pass
  when HyperSpec is available, skip gracefully when absent).
- Full test suite (base atelier + MCP + editor) passes in fresh SBCL
  subprocess.
- CLAUDE.md tool and resource counts updated.
- No regressions in existing 659 assertions.
