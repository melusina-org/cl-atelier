# Implementation Phase 1: Slice 011 — Linter API Cleanup

**Phase:** 1
**Plan:** product/slice/011-linter-api-cleanup/implementation-1.md
**Scope:** Rename `LINT-SYSTEM` to `LINT`, replace its two booleans with
enumerable `:action` and `:scope` keywords, expose the underlying
`collect / inspect / plan / apply` primitives, and remove the obsolete
`LINTER-OP` symbol.

**Prerequisites:**
- ASDF 3.1+ (provides `asdf:non-propagating-operation`).
- `atelier:lint-op` already inherits the non-propagating mixin (landed
  in preparation for this slice, covered by story 4 acceptance).

**Slice:** product/slice/011-linter-api-cleanup/slice.md

## Prior phases

None — this is the first phase of slice 011.

Preparatory work that landed outside the delivery protocol in the
session that surfaced the slice:

- `sibling-systems` default flipped from `t` to `nil` in
  `lint-system`.
- `lint-op` class created, inheriting `non-propagating-operation`.
- `linter-op` defined as a one-release-cycle alias (to be removed in
  this phase, per revised scope).
- Inspector file-exclusion mechanism added to `linter-configuration`
  to shield test data files from the new
  `check-testsuite-package-name` inspector.
- README updated to reference `lint-op`.

These are baseline for phase 1: the new names exist but the old names
still shadow them, and the underlying pipeline is still monolithic.

## Project Knowledge Applied

- **INV-3 (pretty-printer is the single authority on canonical Lisp
  text)** — unchanged by this slice; no text-generating change.
- **INV-4 (regression verification must use a fresh SBCL subprocess)**
  — every acceptance check below runs in a `sbcl --non-interactive`
  subprocess.
- **Patterns: "Bulk text replacement corrupts fixtures that
  deliberately contain the pattern"** — `lint-system` appears inside
  test-data strings in `test/git.lisp` and inside docstrings of
  `src/git.lisp`'s pre-commit hook template. Step 11 explicitly greps
  `test/` and `resource/` before any bulk rename.
- **Patterns: "Documentation count drift from live registry"** —
  the `resource/template/*.text` files contain generated-project
  scaffolding that mentions `atelier:lint-system`. Step 12 updates
  them in the same commit as the rename.

## Risk Register

| # | Risk | Category | Mitigation |
|---|------|----------|-----------|
| 1 | Bulk replacement of `lint-system` catches references inside test data strings in `test/git.lisp` (the test literally verifies the rendered hook contains "lint-system"). | Bulk replacement | Step 11 updates `test/git.lisp` first (change the test assertion to match the new content) before touching the source in `src/git.lisp`. |
| 2 | The `resource/template/LISP-DEVELOPMENT-LINT.text` and `resource/template/LISP-DEVELOPMENT.text` templates are copied into generated projects. Renaming them here changes what new projects get, but projects already scaffolded still call `lint-system`. | Compatibility | Templates are the source of truth for *new* projects; existing generated projects are not this system's responsibility. Note the break in `retrospective.md`. |
| 3 | The internal `lint-system` has implicit dynamic bindings (`*project-configuration*`, `*linter-configuration*`, `*current-pathname*`, `*current-line-vector*`) that callers of the new primitives may miss. | Protocol boundary | Each primitive that consumes dynamic state asserts or binds it internally. `collect-lint-files` reads no dynamic state. `inspect-lint-files` binds configuration from the system designator. `plan-resolutions` reads configuration from a bound dynamic, with a clear docstring. `apply-lint-resolutions` touches no configuration. |
| 4 | `testsuite-autofix` testcases reference `lint-system` by name (`validate-lint-system-autofix`, etc.). Renaming them in the same phase keeps diffs consistent, but a test failure mid-rename could leave half-renamed symbols. | Atomic rename | Commit the test rename and source rename together; verify full suite passes before reporting phase closure. |
| 5 | `ecase` over `:action` values means any typo on a call site signals an unrecoverable error — which is the intended behaviour but produces a confusing message. | Ergonomics | Use `ecase` so SBCL lists all accepted values in the restart. |
| 6 | Composing the four primitives must produce the same result as `lint :action :fix` at convergence. A subtle mismatch in ordering or dynamic binding would be hard to spot. | Invariant | Story 3 adds a fast testcase that runs both paths on the same synthetic source and asserts the resulting content matches. |

## OSS Components

None new. The slice uses only `asdf`, `alexandria`, `uiop`,
`cl-ppcre`, and `eclector.concrete-syntax-tree`, all already present.

## Phase Scope

**Stories covered:**
- Story 1 — Rename `LINT-SYSTEM` to `LINT` (Steps 5–11).
- Story 2 — Replace boolean flags with `:action` / `:scope` (Steps 4–5).
- Story 3 — Expose `collect-lint-files`, `inspect-lint-files`,
  `plan-resolutions`, `apply-lint-resolutions` (Steps 1–4, 14–17).
- Story 4 — Remove `LINTER-OP` and confirm `LINT-OP` operates
  cleanly (Steps 6, 8).

**Stories deferred:** None — this phase closes the slice.

## File Organisation

Unchanged — no new file created. All edits live in:

```
src/
├── package.lisp       modify: exports add 4 primitives + `lint`; remove lint-system and linter-op
├── asdf.lisp          modify: new primitives + `lint`, update lint-op, delete old
├── git.lisp           modify: pre-commit hook template
├── runner.lisp        modify: header comment referencing `lint-system`
libexec/lisp/
└── development.lisp   modify: `(atelier:lint …)` call
resource/template/
├── LISP-DEVELOPMENT.text      modify: template call site
└── LISP-DEVELOPMENT-LINT.text modify: template call site
test/
├── autofix.lisp              modify: rename testcases, update assertions
└── git.lisp                  modify: update hook-content assertion string
README.md                     modify: user-facing examples
CLAUDE.md                     modify: architecture-doc references
product/knowledge/invariants.md add: INV-44 (or next) — composition invariant
```

## Build System Changes

None. The load order of the existing files is unchanged.

## Package / Module Architecture

**New exports from `atelier`:**
- `lint` — public entry point (orchestrator).
- `collect-lint-files`
- `inspect-lint-files`
- `plan-resolutions`
- `apply-lint-resolutions`

**Removed exports from `atelier`:**
- `lint-system`
- `linter-op`

**Retained exports from `atelier`:**
- `lint-op`, `*linter-findings*`, `apply-resolutions`,
  `apply-resolutions-to-file`, `collect-all-source-files`,
  `collect-sibling-systems`, everything else as-is.

## Type / Class Hierarchy

No change. The four primitives are functions, not classes.

## Protocol Definitions

No new generic functions. The primitives are ordinary functions; the
existing generics (`inspect-file`, `prepare-resolution`,
`apply-resolutions`, `resolve-finding`) are unchanged.

## Error / Condition Types

No new conditions. The `:action` / `:scope` validation uses `ecase`,
which signals `sb-kernel:case-failure` (a `type-error` subtype). No
custom condition is warranted for a one-off typo at a call site.

## Test Plan

| Story | Testcase | Category |
|-------|----------|:--------:|
| 1 | `validate-lint-returns-findings` — `(atelier:lint "atelier" :action :inspect)` returns a list of `finding` instances and writes nothing. | fast |
| 1 | `validate-lint-system-symbol-absent` — `(find-symbol "LINT-SYSTEM" :atelier)` returns `nil`. | fast |
| 2 | `validate-action-inspect` — `:action :inspect` returns findings, no file mutation. | fast |
| 2 | `validate-action-preview` — `:action :preview` returns resolutions, no file mutation. | fast |
| 2 | `validate-action-fix-default` — default `:action` matches `:action :fix`. | fast |
| 2 | `validate-action-rejects-bad-keyword` — `:action :bogus` signals `type-error`. | fast |
| 2 | `validate-scope-rejects-bad-keyword` — `:scope :bogus` signals `type-error`. | fast |
| 2 | `validate-scope-system-excludes-siblings` — `:scope :system` file set does not include test-system files. | fast |
| 2 | `validate-scope-project-includes-siblings` — `:scope :project` file set includes sibling-system files. | fast |
| 3 | `validate-collect-lint-files` — returns expected pathnames for both scopes. | fast |
| 3 | `validate-inspect-lint-files` — returns findings for a known synthetic file. | slow (touches files via Atelier's own source dir) |
| 3 | `validate-plan-resolutions` — returns resolutions of the expected type for findings produced from a synthetic string. | fast |
| 3 | `validate-apply-lint-resolutions` — writes back expected content to a temp file, returns the pathname. | slow |
| 3 | `validate-lint-composes-primitives` — running the four primitives manually and running `lint :action :fix` on the same temp source produce the same final content. | slow |
| 4 | `validate-lint-op-operates-cleanly` — `(asdf:operate 'atelier:lint-op …)` completes without signalling a warning. | slow |
| 4 | `validate-linter-op-symbol-absent` — `(find-symbol "LINTER-OP" :atelier)` returns `nil`. | fast |

All fast tests live in a new section of `test/autofix.lisp` (or a new
`test/lint.lisp` — decided at step 3). Slow tests reuse the temp-file
idiom from `test/autofix.lisp`.

## Implementation Order (Step Table)

| Step | File | Action | Form(s) | Test name | Category |
|------|------|--------|---------|-----------|:--------:|
| 1 | `src/asdf.lisp` | modify | Rename helper `collect-scoped-source-files` to `collect-lint-files`; make it accept `:scope` keyword with `:system`/`:project` values; move docstring to describe the public API. | `validate-collect-lint-files` | fast |
| 2 | `src/asdf.lisp` | add | Define `inspect-lint-files (pathnames &key system-designator)` — binds project/linter configuration, then loops `perform-inspection` over pathnames. Extracted from the `inspect-system` flet inside `lint-system`. | `validate-inspect-lint-files` | slow |
| 3 | `src/asdf.lisp` | add | Define `plan-resolutions (findings)` — extracts the `production-resolution-p` + `accept-resolution-p` + by-file grouping from `lint-system`'s `resolutions-for-findings` flet. Returns a flat list sorted by file, preserving finding order. Does not signal RESOLUTION-PROPOSED (that belongs to the interactive :fix path). | `validate-plan-resolutions` | fast |
| 4 | `src/asdf.lisp` | add | Define `apply-lint-resolutions (resolutions)` — groups resolutions by `finding-file`, delegates to `apply-resolutions-to-file` for each. Returns the list of written pathnames. | `validate-apply-lint-resolutions` | slow |
| 5 | `src/asdf.lisp` | add | Define `lint (system-designator &key (action :fix) (scope :system))` — `ecase` on `:action`, `ecase` on `:scope`. For `:inspect` and `:preview`, delegate directly to the primitives. For `:fix`, keep the existing convergence loop but call the primitives internally so the contract is unified. | `validate-action-*`, `validate-scope-*`, `validate-lint-returns-findings` | fast + slow |
| 6 | `src/asdf.lisp` | remove | Delete the old `lint-system` defun. Delete the `linter-op` defclass (already removed in preparation — verify). | `validate-lint-system-symbol-absent`, `validate-linter-op-symbol-absent` | fast |
| 7 | `src/asdf.lisp` | modify | Update `lint-op`'s `asdf:perform` method to call `lint` rather than `lint-system`. | `validate-lint-op-operates-cleanly` | slow |
| 8 | `src/package.lisp` | modify | Add exports for `lint`, `collect-lint-files`, `inspect-lint-files`, `plan-resolutions`, `apply-lint-resolutions`. Remove `lint-system` (already removed `linter-op`). | (covered by step 6 tests) | fast |
| 9 | `src/runner.lisp` | modify | Update the header comment that references `lint-system` (line 56) to reference `lint`. | — | — |
| 10 | `src/git.lisp` | modify | Update the pre-commit hook template string to `(atelier:lint ~S :action :fix)`. | `validate-install-pre-commit-hook` (existing) | slow |
| 11 | `test/git.lisp` | modify | Update the assertion that checks the rendered hook content to match the new call (the `(search "lint-system" content)` check becomes `(search "(atelier:lint " content)`). | (covered by test rewrite) | slow |
| 12 | `libexec/lisp/development.lisp` | modify | Rename the call in `atelier/development:lint` helper to `(atelier:lint "org.melusina.atelier")`. | — | — |
| 13 | `resource/template/LISP-DEVELOPMENT-LINT.text` | modify | Template string becomes `(atelier:lint …)`. | — | — |
| 14 | `resource/template/LISP-DEVELOPMENT.text` | modify | Same — generated projects use the new name. | — | — |
| 15 | `test/autofix.lisp` | modify | Rename `validate-lint-system-autofix` → `validate-lint-fix`, `validate-lint-system-no-autofix` → `validate-lint-inspect`, `validate-lint-system-partial-autofix` → `validate-lint-partial-fix`. Update call sites inside each testcase to use `(atelier:lint … :action :fix|:inspect)` instead of `(atelier:lint-system … :autofix t|nil)`. Update the file's header comment. Add the new testcases from the Test Plan. Update `testsuite-autofix` aggregator to call the renamed tests. | `validate-lint-fix`, `validate-lint-inspect`, `validate-lint-partial-fix` + new tests | mixed |
| 16 | `README.md` | modify | Update user-facing examples to `atelier:lint` with `:action` / `:scope`. | — | — |
| 17 | `CLAUDE.md` | modify | Update architecture-doc references from `lint-system` to `lint`, and update exported-symbols list. | — | — |
| 18 | `product/knowledge/invariants.md` | modify | Append INV-NN recording the "inspector/fix pipeline composes cleanly from four exported primitives" invariant so subsequent refactors do not collapse them. | — | — |

## Invariants

Existing invariants carry forward (INV-1 through INV-43). New
invariant added in step 18:

- **INV-NN (composition invariant):** `LINT :action :fix` must be
  semantically equivalent to the ordered composition
  `(apply-lint-resolutions (plan-resolutions (inspect-lint-files
  (collect-lint-files system :scope :system))))` applied to
  convergence. Any future refactor must preserve this equivalence.
  The invariant is enforced by `validate-lint-composes-primitives`.

## Test Fixtures

No new fixture files. Tests use synthetic strings and temp files
constructed in-test; consistent with the existing `test/autofix.lisp`
pattern.

## References to Create

None. The ASDF manual reference cited in `slice.md` is external.

## Acceptance Criteria

1. `atelier:lint` is exported and callable with both the default
   arguments and the full `:action` / `:scope` keyword set.
2. `atelier:collect-lint-files`, `atelier:inspect-lint-files`,
   `atelier:plan-resolutions`, `atelier:apply-lint-resolutions` are
   exported and each has at least one fast testcase.
3. `(find-symbol "LINT-SYSTEM" :atelier)` returns `nil`.
4. `(find-symbol "LINTER-OP" :atelier)` returns `nil`.
5. `(atelier:lint "org.melusina.atelier")` runs to completion and
   returns the findings list (default `:fix` semantics, `:system`
   scope); no warnings of any class.
6. `(atelier:lint "org.melusina.atelier" :action :inspect)` writes no
   files and returns findings.
7. `(atelier:lint "org.melusina.atelier" :action :preview)` writes no
   files and returns a list of resolution instances.
8. `(asdf:operate 'atelier:lint-op "org.melusina.atelier")` completes
   without signalling any warning.
9. `atelier/test:run-all-tests` reports at least 474 passing (no
   regressions), with the new primitive tests included in the count.
10. `README.md`, `CLAUDE.md`, `libexec/lisp/development.lisp`,
    `src/git.lisp`, `resource/template/LISP-DEVELOPMENT*.text`, and
    `test/**/*.lisp` contain no reference to `lint-system` or
    `linter-op`.

## Phase Closure Conditions

- All 10 acceptance criteria verified in a fresh SBCL subprocess per
  INV-4.
- `product/slice/011-linter-api-cleanup/implementation-1-notes.md`
  written with story-delivery table, test counts, and any deviation
  notes.
- No deferred items (this phase closes the slice).
