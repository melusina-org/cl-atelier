Slice 011: Linter API Cleanup

**Status:** Complete
**Type:** Improvement
**Goal addressed:** None — maintenance / API ergonomics
**Backlog items:** N/A (surfaced during slice 010 work; see `discovery-log.md` 2026-04-19)
**Planned start / end:** 2026-04-19 / 2026-04-19
**Actual end:** 2026-04-19
**Implementation phases:**
  - Phase 1: product/slice/011-linter-api-cleanup/implementation-1.md — Complete

---

## What changes for users

Before this slice, the Atelier linter presents a DWIM entry point that
conflates three orthogonal concerns behind boolean flags:

```lisp
(atelier:lint-system "my-project" :autofix t :sibling-systems t)
```

The flags are hard to compose (what does `:autofix nil :sibling-systems t`
mean at a glance?), and the function's internal pipeline is monolithic
— a caller who wants "just findings" must still go through
`lint-system`; a caller who wants to lint a single file falls off the
API entirely. The ASDF operation is also misnamed (`LINTER-OP` rather
than ASDF's verb-op convention) and lacks the ASDF 3.1 propagation
mixin, producing a plan-walker warning on every invocation.

After this slice, the public entry point is:

```lisp
(atelier:lint "my-project")                          ; DWIM: inspect + fix, system scope
(atelier:lint "my-project" :action :inspect)         ; findings only
(atelier:lint "my-project" :action :preview)         ; planned resolutions, no write-back
(atelier:lint "my-project" :scope :project)          ; walk every sibling in the .asd
```

Two keyword-valued options replace the two booleans:

- `:action` — `:inspect` | `:preview` | `:fix` (default). Each value
  names a specific outcome; no call site carries two independent
  booleans that together encode an outcome.
- `:scope` — `:system` (default) | `:project`. `:system` inspects the
  requested system's source files plus its `.asd`. `:project` adds
  every sibling system defined in the same `.asd`.

Underneath, the monolithic `lint-system` is decomposed into four pure
primitives — `collect-files`, `inspect-files`, `plan-resolutions`,
`apply-resolutions` — and `lint` becomes a thin orchestrator. Callers
who need to compose the linter into a larger pipeline (CI dry-run,
MCP tool, LSP endpoint) can skip the orchestrator and reach for the
primitives directly.

The ASDF operation class is renamed `ATELIER:LINT-OP` to match
ASDF's verb-op convention (`LOAD-OP`, `COMPILE-OP`, `TEST-OP`) and
inherits from `ASDF:NON-PROPAGATING-OPERATION` so `(asdf:operate
'atelier:lint-op …)` runs cleanly on ASDF 3.1+.

The deprecated `LINTER-OP` is removed outright; no alias. Callers
that referenced it were limited to the README.

## Specification references

- CLHS `pprint-logical-block` / `pprint-newline` (unchanged behaviour,
  referenced because the pretty-printer sits under the same API).
- ASDF manual §7.2 "Defining new operations" — motivation for the
  non-propagating mixin and the verb-op naming convention.

## Stories

### Story 1: Rename `LINT-SYSTEM` to `LINT`

**In order to** call the linter by the shortest meaningful verb,
**a** developer **can** write `(atelier:lint "my-project" …)` at the
REPL and in CI scripts without reading a function named after an
internal design choice (`lint-system` implied a coupling to ASDF
systems that the new `:scope` keyword makes explicit).

**Acceptance criteria:**
- Given a loaded Atelier image, when a developer evaluates
  `(atelier:lint "org.melusina.atelier")`, then the expression returns
  a list of remaining findings and fixes automatically-resolvable
  findings in place (the old DWIM default).
- Given a loaded Atelier image, when a developer evaluates the symbol
  `atelier:lint-system`, then the symbol is unbound (the old name is
  removed, not aliased).
- Given the README and `libexec/lisp/development.lisp`, when a reader
  opens them, then every reference uses `atelier:lint` (never
  `lint-system`).

### Story 2: Replace boolean flags with keyword-valued options

**In order to** read call sites as sentences that describe the
intended outcome rather than decode two independent booleans,
**a** developer **can** pass `:action :inspect|:preview|:fix` and
`:scope :system|:project` to `atelier:lint`.

**Acceptance criteria:**
- Given `:action :inspect`, when `lint` runs, then it returns the
  full findings list and writes no files.
- Given `:action :preview`, when `lint` runs, then it returns the
  planned resolutions (a list of `resolution` instances) without
  writing files and without iterating the autofix loop.
- Given `:action :fix` (or default), when `lint` runs, then it
  applies automatic resolutions, writes files, and returns the
  remaining findings after convergence.
- Given `:scope :system`, when `lint` runs on `"foo"`, then the
  inspected file set is exactly `foo`'s source files plus `foo`'s
  `.asd`.
- Given `:scope :project`, when `lint` runs on `"foo"`, then the
  inspected file set is every source file of every system declared
  in `foo`'s `.asd`.
- Given any other value for `:action` or `:scope`, when `lint` runs,
  then it signals an error naming the unrecognised value (no silent
  fallback).

### Story 3: Expose the collect/inspect/plan/apply primitives

**In order to** compose the linter into a larger pipeline (CI
dry-run reporter, MCP tool, LSP endpoint) without reimplementing
file traversal, finding collection, or resolution synthesis,
**a** developer **can** call four exported primitives that each do
one thing.

**Acceptance criteria:**
- Given `atelier:collect-lint-files` with a system designator and a
  `:scope` keyword, when called, then it returns the list of pathnames
  the linter would inspect under that scope.
- Given `atelier:inspect-lint-files` with a list of pathnames, when
  called, then it returns the list of findings without writing files.
- Given `atelier:plan-resolutions` with a list of findings, when
  called, then it returns the list of resolutions that the
  disposition policy would apply (respecting `:skip`, `:interactive`,
  `:auto` and maintainer superseding).
- Given `atelier:apply-lint-resolutions` with a list of resolutions,
  when called, then it writes each file's resolutions back atomically
  and returns the list of written pathnames.
- Given these four primitives, when a caller composes them in the
  documented order, then the result matches
  `atelier:lint :action :fix` on the same system.

### Story 4: Rename `LINTER-OP` to `LINT-OP` and fix ASDF 3.1 propagation

**In order to** run `(asdf:operate 'atelier:lint-op "my-project")`
without an ASDF plan-walker warning,
**a** developer **can** use `atelier:lint-op`, which inherits from
`asdf:non-propagating-operation`.

**Acceptance criteria:**
- Given a loaded Atelier image, when a developer evaluates
  `(asdf:operate 'atelier:lint-op "org.melusina.atelier")`, then the
  operation completes without signalling `"No dependency
  propagating scheme specified for operation class …"`.
- Given a loaded Atelier image, when a developer evaluates the symbol
  `atelier:linter-op`, then the symbol is unbound.
- Given the README, when a reader opens it, then the ASDF example
  references `atelier:lint-op`.

## Quality Criteria

- [ ] `atelier/test:run-all-tests` reports 474 passing or higher (no
      regressions relative to the baseline captured in slice 010).
- [ ] `(asdf:operate 'atelier:lint-op "org.melusina.atelier")` emits no
      warnings of any class.
- [ ] No call site in `src/`, `test/`, `libexec/`, or README references
      the removed symbols `atelier:lint-system` or `atelier:linter-op`.
- [ ] The four primitives are each covered by at least one fast
      testcase exercising the happy path and one verifying that
      composing them reproduces the DWIM result.

## Definition of Ready

- [x] Stories traceable to backlog items (added 2026-04-19)
- [x] Stories sized ≤ 2 days each
- [x] Acceptance criteria written
- [x] Quality criterion defined
- [x] Spec references identified

## Definition of Done

- [ ] All stories complete with acceptance criteria passing
- [ ] Quality criteria passing
- [ ] Full test suite passes
- [ ] All implementation phases have completion notes
- [ ] `product/slice/011-linter-api-cleanup/retrospective.md` created
- [ ] `product/backlog.md` updated (entry closed)
- [ ] `product/roadmap.md` updated
