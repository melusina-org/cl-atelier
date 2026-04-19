# Implementation Phase 1: Remove line-length inspector

**Slice:** `product/slice/008-remove-line-length-inspector/slice.md`
**Phase:** 1 (single-phase slice)
**Scope:** Delete `check-line-length`, `line-too-long-finding`, `fix-line-too-long`, their tests and fixtures, and all documentation mentions.
**Prerequisites:** None beyond a clean working tree.

## Prior phases

None — first and only phase.

## Project Knowledge Applied

No `product/knowledge/` files exist yet. The only carried-forward context is from the slice 007 retrospective: `*current-line-vector*` was a live-image-masked load-order bug, so any deletion that changes the ASDF load order must be verified in a **fresh SBCL subprocess**, not just the REPL image in which development happens. This slice removes two files from the load order (`check-line-length.lisp` and `fix-line-too-long.lisp`) and one test file — the risk is low but the verification must still happen in a fresh process.

## Risk Register

| # | Risk | Category | Mitigation |
|---|------|----------|-----------|
| 1 | Hidden reference to `check-line-length`, `line-too-long-finding`, or `fix-line-too-long` in a file not yet surveyed causes a compile or load failure | load-time | Re-run `grep -r` across the entire tree (excluding `product/`) as the **first step** of execution. Any hit outside the enumerated files requires either adding a new step or stopping to escalate. |
| 2 | Live-image state masks a load-order problem (slice 007 precedent) | stale-fasl | Final verification is a clean subprocess run: `sbcl --non-interactive --eval '(asdf:test-system "org.melusina.atelier")'`. Do **not** trust REPL results alone. |
| 3 | The `test/utilities.lisp:119` fixture-skip block turns out to be needed by other future directories | scope-boundary | The block's docstring says "allows the in-progress fix-line-too-long directory to be carried along." Read the code around line 119 carefully: if the skip is generic (filters any unknown symbol), leave the code and only update the docstring. If the skip is specific to `fix-line-too-long`, delete it. |
| 4 | `linter-configuration.sexp` examples in README.md use `(check-line-length)` as a `:disabled-inspectors` example | doc | README.md line 130 needs a different inspector name in the example. Use `check-earmuffs` or `check-constant-naming` as the replacement example. |
| 5 | Downstream consumers of atelier (none known, but the package exports are public) break when `line-too-long-finding`, `check-line-length`, `fix-line-too-long`, or `*default-maximum-line-length*` disappear from the `atelier` package | API | This is an accepted breaking change — the slice type is Maintenance and the research reference explains the rationale. No mitigation beyond honest documentation in the retrospective. |
| 6 | The two modified-but-uncommitted files from session start (`defun-key-params.text`, `long-loop.text`) are about to be deleted by this slice | trivial | `git rm` will drop both tracked and modified state in one operation. No special handling. |

## OSS Components

None.

## Phase Scope

- S1, S2, S3, S4 — all four slice stories are in this phase. The slice is pure deletion and single-phase.

## File Organisation

Files deleted:

```
src/inspectors/check-line-length.lisp              DELETE
src/maintainers/fix-line-too-long.lisp             DELETE
test/inspectors/check-line-length.lisp        DELETE
test/fixtures/autofix/fix-line-too-long/      DELETE (20 .text files + directory)
```

Files edited:

```
src/finding.lisp                                   EDIT — remove one define-findings row
src/package.lisp                                   EDIT — remove 3 exports + 1 defparameter export if present
org.melusina.atelier.asd                           EDIT — remove 2 :file components (main system + testsuite)
test/entrypoint.lisp                          EDIT — remove one call
test/utilities.lisp                           EDIT — update docstring at ~line 119 (see Risk 3)
CLAUDE.md                                          EDIT — inspector count 16→15; remove check-line-length from line 88
README.md                                          EDIT — remove line 47 bullet; replace inspector name in line 130 example
product/backlog.md                                 EDIT — reword item 16; remove "fix-line-too-long (Later)" reference
product/roadmap.md                                 EDIT — remove row 40 (fix-line-too-long), append revision history
```

Files explicitly retained:

```
product/reference/line-length-research.md          KEEP UNCHANGED — rationale record
```

## Build System Changes

`org.melusina.atelier.asd` loses two `(:file …)` entries. No dependency reordering, no new modules. Load order of surviving files is unchanged.

## Package / Module Architecture

No new exports. Three removals from `atelier` package (`src/package.lisp`):

- `#:fix-line-too-long` (line 184)
- `#:line-too-long-finding` (line 198)
- `#:check-line-length` (line 216)

Verify whether `*default-maximum-line-length*` is exported. If yes, remove. If no (internal only), no action.

## Type / Class Hierarchy

Remove one class: `line-too-long-finding`, which is a direct subclass of `line-finding`. Removed by deleting one row in the `define-findings` form in `src/finding.lisp`. No other class depends on it.

## Protocol Definitions

None changed. `inspect-line`, `maintain`, `apply-resolutions` are untouched.

## Error / Condition Types

None changed.

## Test Plan

| Story | Test name | Category | Skip condition |
|-------|-----------|:--------:|---------------|
| S1 | `(asdf:load-system "org.melusina.atelier")` in fresh SBCL | slow | — |
| S1 | `(find-symbol "CHECK-LINE-LENGTH" :atelier)` → NIL verification | fast | — |
| S1, S2, S3 | `(asdf:test-system "org.melusina.atelier")` in fresh SBCL — full regression | slow | — |
| S1, S2 | `(length (atelier:list-inspectors))` decreased by exactly 1 from baseline | fast | — |
| S1, S2 | `(length (atelier:list-maintainers))` decreased by exactly 1 from baseline | fast | — |
| S3 | Pass count equals baseline minus exactly 3 (the removed `validate-check-line-length-*` testcases) | slow | — |
| All | `grep -rn 'line-too-long\|check-line-length\|fix-line-too-long' src/ test/ CLAUDE.md README.md product/backlog.md product/roadmap.md` returns no hits | fast | — |

No new tests are written. The baseline pass count before this slice is **299** (from slice 007 retrospective). Expected post-slice count: **296**.

## Implementation Order (Step Table)

The order is designed so the system compiles cleanly after every step. Deletions happen in reverse load-dependency order: test entrypoint → test fixtures → test file → maintainer → inspector → finding class → package exports → ASDF components → documentation.

| Step | File | Action | Form(s) | Test name | Category |
|------|------|--------|---------|-----------|:--------:|
| 1 | *(survey)* | Read-only re-grep | `grep -rn 'line-too-long\|check-line-length\|fix-line-too-long' src/ test/ CLAUDE.md README.md product/backlog.md product/roadmap.md org.melusina.atelier.asd` | confirm-no-new-references | fast |
| 2 | `test/entrypoint.lisp` | edit | Remove the `(testsuite-check-line-length)` call (line 30) | — | — |
| 3 | `test/inspectors/check-line-length.lisp` | delete | Whole file | — | — |
| 4 | `org.melusina.atelier.asd` | edit | Remove `(:file "check-line-length")` from the **testsuite** inspectors module (line 95) | — | — |
| 5 | *(verify)* | fresh SBCL | `(asdf:test-system "org.melusina.atelier")` — must still pass with the testsuite check-line-length file gone but source still present | regression-after-test-deletion | slow |
| 6 | `test/fixtures/autofix/fix-line-too-long/` | delete | Whole directory, all 20 `.text` files | — | — |
| 7 | `test/utilities.lisp` | edit | Update `discover-autofix-cycle-fixtures` docstring — remove the "allows the in-progress fix-line-too-long directory to be carried along" sentence. Verify the skip logic itself is generic (filters any unknown symbol) and leave it alone if so. | — | — |
| 8 | *(verify)* | fresh SBCL | `(asdf:test-system "org.melusina.atelier")` — must pass, no fixture-loader complaints | regression-after-fixture-deletion | slow |
| 9 | `src/maintainers/fix-line-too-long.lisp` | delete | Whole file | — | — |
| 10 | `org.melusina.atelier.asd` | edit | Remove `(:file "fix-line-too-long")` from the **main** system maintainers module (line 68) | — | — |
| 11 | `src/package.lisp` | edit | Remove `#:fix-line-too-long` export (line 184) | — | — |
| 12 | *(verify)* | fresh SBCL | `(asdf:test-system "org.melusina.atelier")` | regression-after-maintainer-deletion | slow |
| 13 | `src/inspectors/check-line-length.lisp` | delete | Whole file | — | — |
| 14 | `org.melusina.atelier.asd` | edit | Remove `(:file "check-line-length")` from the **main** system inspectors module (line 41) | — | — |
| 15 | `src/finding.lisp` | edit | Remove the `(line-finding line-too-long-finding …)` row (line 264) from `define-findings` | — | — |
| 16 | `src/package.lisp` | edit | Remove `#:line-too-long-finding` export (line 198) and `#:check-line-length` export (line 216). Check whether `#:*default-maximum-line-length*` appears and remove if so. | — | — |
| 17 | *(verify)* | fresh SBCL | `(asdf:test-system "org.melusina.atelier")` — full regression, pass count = baseline − 3 | regression-after-inspector-deletion | slow |
| 18 | *(verify)* | fresh SBCL | `(length (atelier:list-inspectors))` and `(length (atelier:list-maintainers))` each decreased by 1 from baseline; `(find-symbol "CHECK-LINE-LENGTH" :atelier)` returns `NIL` for the symbol part | list-counts-and-symbol-intern-check | fast |
| 19 | `CLAUDE.md` | edit | Line 9: `16 inspectors` → `15 inspectors`. Line 88: remove `, check-line-length` from the Line inspectors list. Leave the maintainer count of 10 unchanged — it was already correct. | — | — |
| 20 | `README.md` | edit | Line 47: delete the `check-line-length` bullet. Line 130: replace `(check-line-length)` with `(check-earmuffs)` as the disabled-inspectors example. | — | — |
| 21 | `product/backlog.md` | edit | Item 16: reword to "Linter: line-level inspectors for CL (trailing whitespace, mixed indentation)" — remove "line length". Add a Revision History row. | — | — |
| 22 | `product/roadmap.md` | edit | Remove the `fix-line-too-long maintainer` row (line 40). Append a Revision History row for 2026-04-09 stating the removal rationale with a reference to `product/reference/line-length-research.md`. | — | — |
| 23 | *(final verification)* | fresh SBCL | `sbcl --non-interactive --eval '(progn (asdf:test-system "org.melusina.atelier") (sb-ext:exit :code 0))'` — full green run in a clean subprocess | final-clean-subprocess-regression | slow |
| 24 | *(final verification)* | shell | `grep -rn 'line-too-long\|check-line-length\|fix-line-too-long\|line_too_long\|LINE-TOO-LONG' src/ test/ CLAUDE.md README.md org.melusina.atelier.asd product/backlog.md product/roadmap.md` — must return empty | final-grep-audit | fast |

## Invariants

Invariants carried forward from prior slices (slice 007):

- **I1** — Maintainers must not reparse files; use `cst-node` and `cst-root` from the finding.
- **I2** — Inspection is fast and frequent; resolution is slow and rare.
- **I3** — The pretty-printer is the single authority on canonical Lisp text for syntax-level maintainers.
- **I4** — Per-maintainer self-idempotency at N=1 is required.
- **I5** — Discoverable fixtures are a convenience, not a goal.
- **I6** — Regression verification must run in a fresh SBCL subprocess, not only the development REPL (slice 007 `*current-line-vector*` lesson).

New invariant introduced by this slice:

- **I7** — Atelier does not police line length. The pretty-printer's `*print-right-margin*` is the only mechanism that influences line length, and it is advisory. A form that remains long after pretty-printing is accepted as-is.

## Test Fixtures

None added. Twenty fixtures removed (step 6).

## References to Create

None. `product/reference/line-length-research.md` already exists and is retained unchanged.

## Acceptance Criteria

1. `(asdf:load-system "org.melusina.atelier")` succeeds in a fresh SBCL with zero warnings about undefined functions, classes, or exported symbols. *(Verified by step 23.)*
2. `(asdf:test-system "org.melusina.atelier")` runs to completion in a fresh SBCL with **zero failures**. *(Verified by step 23.)*
3. The test pass count is **exactly 3 less** than the slice 007 baseline of 299 — i.e., **296**. *(Verified by step 17.)*
4. `(find-symbol "CHECK-LINE-LENGTH" :atelier)` returns `NIL` for both the symbol and status values. *(Verified by step 18.)*
5. `(length (atelier:list-inspectors))` has decreased by exactly 1 from the pre-slice baseline, and `(length (atelier:list-maintainers))` has decreased by exactly 1 from the pre-slice baseline. *(Verified by step 18.)*
6. `grep -rn 'line-too-long\|check-line-length\|fix-line-too-long\|LINE-TOO-LONG' src/ test/ CLAUDE.md README.md org.melusina.atelier.asd product/backlog.md product/roadmap.md` returns **zero matches**. The file `product/reference/line-length-research.md` is explicitly excluded from this audit and is retained unchanged. *(Verified by step 24.)*
7. `test/fixtures/autofix/fix-line-too-long/` does not exist as a directory. *(Verified by step 24 implicitly.)*
8. `CLAUDE.md` line 9 reads "15 inspectors, 10 automatic maintainers." Line 88 does not contain `check-line-length`. *(Verified by step 19.)*
9. `README.md` does not contain the string `check-line-length` anywhere. *(Verified by step 20.)*

## Phase Closure Conditions

For the Maker to write `implementation-1-notes.md` and close the phase, all of the following must be true:

- All 24 steps above are complete.
- Acceptance criteria 1–9 all pass.
- The final clean-subprocess regression run (step 23) produced green output and exited 0.
- The final grep audit (step 24) produced zero matches.
- Reviewer has cleared the phase completion review.

---

## Notes for the Maker

This is a mechanical deletion slice. No new code, no architectural decisions, no new forms. The single non-obvious point is **Risk 3** (the fixture-skip docstring at `test/utilities.lisp:119`): read the surrounding code before editing, decide whether the skip logic is generic or specific to `fix-line-too-long`, and act accordingly. If in doubt, leave the code alone and only update the docstring.

The **load-order verification discipline** from slice 007 is mandatory: every `*(verify)*` step must be a fresh SBCL subprocess, **not** a REPL reload in the development image. The Maker should use `sbcl --non-interactive --eval ...` or `asdf:test-system` from a cold image. The slice 007 retrospective's "all prior slice regressions had been running against stale fasls" lesson is the reason this discipline exists.

The two modified-but-uncommitted files at session start (`defun-key-params.text`, `long-loop.text`) are both inside the doomed `test/fixtures/autofix/fix-line-too-long/` directory. `git rm -r` or plain filesystem `rm -r` followed by `git add -A` will handle both. No special care needed for the modifications.
