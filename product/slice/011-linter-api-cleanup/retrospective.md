# Retrospective: Slice 011 — Linter API Cleanup
**Recorded:** 2026-04-19
**Implementation phases:** 1 total — `implementation-1.md`

## Delivery Summary

- Stories delivered: 4 of 4 (LINT rename, keyword-valued options,
  primitives exposure, LINTER-OP removal + non-propagating mixin).
- Acceptance criteria: all 10 passed.
- Quality criteria: all passed.
- Full test suite: 495 passed / 495 total. Fresh SBCL subprocess,
  INV-4 respected.

## Phase Notes Summary

| Phase | Key deliverables | Deferred to backlog |
|-------|-----------------|---------------------|
| 1 | `atelier:lint` DWIM entry point with `:action` / `:scope` keyword-valued options; four exported primitives (`collect-lint-files`, `inspect-lint-files`, `plan-resolutions`, `apply-lint-resolutions`); `lint-op` inherits `asdf:non-propagating-operation`; old `lint-system` and `linter-op` symbols removed outright; README, CLAUDE.md, libexec, hook template, resource templates, and 21 new tests. | None. |

## Goal Progress

No definition goal directly names linter-API ergonomics, but this
slice materially improved a quality attribute — API composability —
that every downstream consumer (CI scripts, MCP tools, future LSP
endpoints) depends on. No goal moves to Reached; no goal regresses.

## What we learned

- **Keyword-valued options beat boolean flags once you have more than
  one independent decision axis.** `:autofix t :sibling-systems t` is
  a three-bit encoding of an outcome; `:action :fix :scope :project`
  spells the outcome out. Call sites read as sentences and grep for
  a specific value narrows the search to intentional uses.
- **DWIM and composable primitives can coexist.** Splitting a
  monolithic pipeline into `collect / inspect / plan / apply` does
  not force callers to always wire them up by hand — the orchestrator
  remains the one-liner. But the primitives are there for the CI
  dry-run reporter and the MCP tool that need only one step.
- **A composition invariant is worth writing down.** INV-15 makes
  explicit that LINT = (apply ∘ plan ∘ inspect ∘ collect) at
  convergence. Without this invariant in the knowledge base, a
  future refactor could easily make LINT diverge from the primitive
  chain and nothing would catch it until a consumer failed.
- **Removing the deprecated alias was the right call.** The one-cycle
  deprecation we had planned in the original scope would have left
  `linter-op` lingering in the codebase with no production consumer.
  Deleting it in the same commit kept the diff coherent.

## User feedback since delivery

*Filled 2–4 weeks after release — leave blank initially.*

## Backlog and roadmap changes

- Completed: nothing on the numbered backlog (this slice was surfaced
  during slice 010 work; tracked in `discovery-log.md`). The
  `delivered` table in `backlog.md` should gain a line for the new
  linter API.
- Added: none.
- Reprioritized: none.

## Follow-ups flagged (separate from slice 011)

- **Slice 010 retrospective is missing.** The pretty-printer /
  Emacs-compatibility work landed in two commits but no
  `retrospective.md` was written. Two options: (a) write a
  retrospective now covering phase 1 and phase 2, or (b) treat the
  recent pretty-printer fix for the LOOP fixture as a tail
  continuation of slice 010 and close it properly.
