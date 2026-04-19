# Implementation Phase 1 Notes: Slice 011 — Linter API Cleanup
**Phase:** 1
**Plan:** product/slice/011-linter-api-cleanup/implementation-1.md
**Recorded:** 2026-04-19
**Status:** Complete

## Stories delivered in this phase

- **Story 1 — Rename `LINT-SYSTEM` to `LINT`** — all acceptance criteria passed.
  `atelier:lint` is exported and callable; `atelier:lint-system` is
  unbound. README, CLAUDE.md, libexec/lisp/development.lisp,
  src/git.lisp, and resource/template/LISP-DEVELOPMENT*.text all
  updated to the new name.

- **Story 2 — Replace boolean flags with keyword-valued options** — all
  acceptance criteria passed. `:action :inspect|:preview|:fix` and
  `:scope :system|:project` are accepted; unrecognised values signal
  `TYPE-ERROR` via `ECASE` and `(declare (type (member …)))`.

- **Story 3 — Expose collect/inspect/plan/apply primitives** — all
  acceptance criteria passed. `atelier:collect-lint-files`,
  `atelier:inspect-lint-files`, `atelier:plan-resolutions`, and
  `atelier:apply-lint-resolutions` are exported. The composition
  invariant (INV-15) is enforced by
  `validate-lint-composes-primitives`.

- **Story 4 — Rename `LINTER-OP` to `LINT-OP`** — all acceptance criteria
  passed. `atelier:linter-op` is unbound; `atelier:lint-op` inherits
  `asdf:non-propagating-operation` and runs without the ASDF 3.1
  propagation-scheme warning.

## Acceptance criteria results

| # | Criterion | Result | Verified by |
|---|-----------|:------:|-------------|
| 1 | `atelier:lint` exported and callable. | ✓ | `validate-lint-action-inspect`, `validate-lint-action-preview`, manual `(atelier:lint "org.melusina.atelier")` run. |
| 2 | Four primitives exported, each with fast testcase. | ✓ | `validate-collect-lint-files-*`, `validate-inspect-lint-files-*`, `validate-plan-resolutions-*`, `validate-apply-lint-resolutions-*`. |
| 3 | `LINT-SYSTEM` symbol absent. | ✓ | `validate-lint-system-symbol-absent`. |
| 4 | `LINTER-OP` symbol absent. | ✓ | `validate-linter-op-symbol-absent`. |
| 5 | `(atelier:lint …)` runs, no warnings. | ✓ | Manual cold-subprocess run. |
| 6 | `:action :inspect` returns findings, no mutation. | ✓ | `validate-lint-action-inspect`. |
| 7 | `:action :preview` returns resolutions, no mutation. | ✓ | `validate-lint-action-preview`. |
| 8 | `(asdf:operate 'atelier:lint-op …)` no warnings. | ✓ | Manual cold-subprocess run. |
| 9 | `run-all-tests` ≥ 474 passing. | ✓ | 495/495 passing. |
| 10 | No `lint-system`/`linter-op` references in production tree. | ✓ | `grep -rn` across src/, test/, libexec/, README, CLAUDE.md, resource/ — zero hits outside the intentional absent-symbol tests. |

## Test results

- Fast: 150 passed (baseline 136, +14 new `validate-lint-fast` tests).
- Slow: 345 passed (baseline 338, +7 new `validate-lint-slow` tests).
- Snail: none introduced.

All verification run in a fresh `sbcl --non-interactive` subprocess
per INV-4.

## Invariants established or confirmed

- **INV-10**: Annotated as superseded by INV-15. The original "autofix
  is opt-in" invariant is retired because `lint :action :fix` is now
  the DWIM default; the "no side effects" contract is expressed by
  `:action :inspect` or `:action :preview` explicitly.
- **INV-15** (new): LINT decomposes into four composable primitives.
  `(lint system :action :fix :scope :system)` is semantically
  equivalent to the ordered composition
  `(apply-lint-resolutions (plan-resolutions (inspect-lint-files
  (collect-lint-files system :scope :system))))` at convergence. The
  equivalence is enforced by `validate-lint-composes-primitives`.
- Invariants 1–9, 11–14: confirmed, no interaction with this slice.

## Deferred items (for next phase)

None. This phase closes the slice.

## Reworks performed

- **Initial `assert-t` on generalised-boolean return values** — first
  pass of `validate-collect-lint-files-project-scope` used `(assert-t
  has-test-file)` where `has-test-file` came from `some` with
  `search`, which returns an integer (the match position), not `T`.
  Per the project note "assert-t is strict", changed to `assert-t*`.
  Surfaced by the first full test run; corrected in one edit.

## New risks discovered

None.

## Technical decisions made

- **`plan-resolutions` is non-signalling.** The signalling/restart
  machinery for interactive acceptance was extracted into
  `interactively-accept-resolutions`, called only from `lint :action
  :fix`. This keeps `plan-resolutions` pure so `:action :preview`
  never produces side effects and composes cleanly into
  non-orchestrator pipelines.
- **`apply-lint-resolutions` does not perform component renames.**
  File-on-disk renames for `fix-deprecated-component-name` require a
  system context that the primitive does not have. The rename step
  lives in `lint :action :fix` via the helper
  `perform-component-renames-from-resolutions`. Documented in the
  primitive's docstring.
- **`collect-lint-files :scope` uses `ecase`.** A typo on the keyword
  signals `sb-kernel:case-failure` which SBCL reports with the list
  of accepted values — the debugger experience is better than a
  custom condition. Same policy for `lint :action` and `lint :scope`.
- **The old `LINTER-OP` alias was removed outright** per explicit
  user direction mid-slice. The slice was originally planned with
  a one-release deprecated alias; the revised scope deletes the
  symbol with no migration shim because no production consumer
  references it (only the README did, already updated).

## Notes for Strategist retrospective

- Stories remaining: none.
- Snail tests requiring confirmation: none.
- Recommended: Close slice 011 and update backlog/roadmap. Slice 010
  retrospective is still outstanding — flagging separately.
