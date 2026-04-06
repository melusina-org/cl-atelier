# Slice 003: Line-Level Inspectors

**Status:** Planned
**Type:** Bet
**Goal:** G2 — Linter covers file, line, region, and CST level with ASDF integration
**OKR contribution:** G2 — Line-level inspectors produce structured `line-finding` instances via the same pipeline as file-level inspectors
**Hypothesis assumption:** Line-level inspectors (trailing whitespace, line length, mixed indentation) operating on all languages through the same `inspect-file` / `run-file-inspectors` pipeline will validate that the linter architecture handles multi-level inspection without requiring runner changes.
**Hypothesis prediction:** A developer running `linter-op` on a system with mixed-quality source files receives `line-finding` instances pinpointing trailing whitespace, overly long lines, and mixed indentation — all through the same ASDF operation and policy system established in Slice 002.
**Hypothesis disposition:** ✅ Validated — the pipeline is proven (Slice 002 ✅), and line-level inspection is a straightforward extension.
**Leading indicator:** Number of line-level inspectors registered and producing findings — baseline 0, target ≥ 3 by slice completion.
**Kill criterion:** If line-level inspectors cannot produce `line-finding` instances through `run-file-inspectors` without runner modifications, the inspector level abstraction must be revisited.
**Planned start / end:** 2026-04-06 / 2026-04-10
**Actual end:**
**Implementation phases:**
  - Phase 1: product/slice/003-line-level-inspectors/implementation-1.md — Planned

---

## Customer Journey

Before this slice: The linter pipeline handles file-level inspectors (encoding, SPDX license header) but has no line-level analysis. Common quality issues like trailing whitespace, overly long lines, and inconsistent indentation are not detected.

After this slice: A developer runs `(asdf:operate 'atelier:linter-op :my-system)` and receives `line-finding` instances for trailing whitespace, lines exceeding the configured maximum length, and files mixing tabs and spaces. Each finding includes the file, line number, and source text. The mixed indentation inspector defaults to spaces-only but can be configured per-project via `linter-configuration`. The legacy line-length inspector (codestyle-0002) is superseded and removed.

## Personas served
- End user: Common Lisp developer — receives precise line-level diagnostics for common code quality issues across all source file types in the project.

## Stories

### S1: Trailing whitespace inspector
**As a** developer, **I want** an inspector that detects trailing whitespace on source lines, **so that** I can keep my source files clean.
**Acceptance criteria:**
- Given a file with no trailing whitespace, when the inspector runs, then no finding is produced.
- Given a file with trailing spaces on line 5, when the inspector runs, then a `line-finding` is produced with the correct line number, column pointing to the first trailing space, and source text.
- Given the inspector is registered, when I call `(atelier:list-inspectors)`, then `atelier:check-trailing-whitespace` appears.

### S2: Line length inspector
**As a** developer, **I want** an inspector that detects overly long lines, **so that** my source files remain readable.
**Acceptance criteria:**
- Given a file where all lines are ≤ 100 characters, when the inspector runs, then no finding is produced.
- Given a file with a line of 142 characters on line 10, when the inspector runs, then a `line-finding` is produced with the correct line number.
- Given a line that is a Lisp definition form (starting with `(def`), when the inspector runs, then no finding is produced regardless of length.
- Given a line that contains a single word (no spaces), when the inspector runs, then no finding is produced regardless of length.
- Given the inspector is registered, when I call `(atelier:list-inspectors)`, then `atelier:check-line-length` appears.

### S3: Mixed indentation inspector
**As a** developer, **I want** an inspector that detects files mixing tabs and spaces for indentation, **so that** my project uses consistent indentation.
**Acceptance criteria:**
- Given a file using only spaces for indentation, when the inspector runs with default configuration (spaces), then no finding is produced.
- Given a file using tabs on line 3, when the inspector runs with default configuration (spaces), then a `line-finding` is produced for line 3.
- Given a linter-configuration that sets indentation style to `:tabs`, when the inspector runs on a file using spaces, then a `line-finding` is produced.
- Given the inspector is registered, when I call `(atelier:list-inspectors)`, then `atelier:check-mixed-indentation` appears.

### S4: Linter-configuration extension for indentation style
**As a** system author, **I want to** declare the project's indentation style in `linter-configuration`, **so that** the mixed indentation inspector uses the right default.
**Acceptance criteria:**
- Given a linter-configuration with `(:indentation-style :tabs)`, when the mixed indentation inspector runs, then it flags spaces instead of tabs.
- Given a linter-configuration with no indentation-style setting, when the mixed indentation inspector runs, then it defaults to `:spaces`.

### S5: Remove legacy codestyle-0002
**As a** maintainer, **I want** the legacy line-length inspector removed, **so that** there is one canonical inspector for line length.
**Acceptance criteria:**
- Given the legacy codestyle-0002 files are deleted, when the full test suite runs, then all tests pass.
- Given `atelier/development:lint` runs on the project, then it completes without error.

## Quality Attribute Acceptance Criteria
- [ ] All fast tests execute in under 2 seconds total.
- [ ] All slow tests (fixture file I/O) execute in under 10 seconds total.
- [ ] Each inspector is exported and documented with docstrings.
- [ ] Each inspector produces its own finding subclass (`trailing-whitespace-finding`, `line-too-long-finding`, `mixed-indentation-finding`).

## Capability Maturity Transitions
- Line-level Inspection (G2): Not started → Foundation

## Definition of Ready
- [x] Hypothesis disposition ✅
- [x] Stories sized ≤ 2 days each
- [x] Acceptance criteria written for all stories
- [x] QA acceptance criterion defined
- [x] Leading indicator baseline established: 0 line-level inspectors on 2026-04-06
- [x] Dependencies clear — Slice 002 complete (ASDF integration, runner, file inspectors)

## Definition of Done
- [ ] All stories complete with acceptance criteria passing
- [ ] Quality attribute criteria passing
- [ ] Leading indicator being measured
- [ ] All implementation phases have completion notes
- [ ] `product/slice/003-line-level-inspectors/retrospective.md` created
- [ ] `product/maturity-tracker.md` updated
