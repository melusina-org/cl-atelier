# Slice 006: Linter Refinements

**Status:** In Progress
**Type:** Bet
**Goal:** G2 — Linter covers file, line, region, and CST level with ASDF integration
**Hypothesis assumption:** Three refinements — fixture auto-discovery, SPDX-driven header simplification, and autofix signalling — can be delivered as a single coherent slice because they share the inspector/maintainer/resolution infrastructure.
**Hypothesis prediction:** After this slice: (1) adding a `.text` fixture file is sufficient to create a new test; (2) files with SPDX identifiers have their verbose license headers stripped; (3) `lint-system :autofix t` signals `resolution-proposed` conditions with accept/skip restarts, allowing selective intervention and maturity-gated application.
**Planned start / end:** 2026-04-07 / 2026-04-10
**Implementation phases:**
  - Phase 1: product/slice/006-linter-refinements/implementation-1.md

---

## Stories

### S1: Fixture auto-discovery
**As a** tool author, **I want** test fixtures to be auto-discovered from the filesystem, **so that** adding a fixture file creates a test without code changes.
**Acceptance criteria:**
- Given a `.text` file in `testsuite/fixtures/maintainer/<name>/`, when the test suite runs, then the fixture is loaded and compared automatically.
- Given `(maintainer-fixture 'atelier:fix-bare-lambda)`, then the baseline fixture pathname is returned.
- Given `(pretty-printer-fixture "flet-single-binding")`, then the fixture pathname is returned.
- Given `(inspector-fixture 'atelier:check-bare-lambda)`, then the fixture pathname is returned.

### S2: SPDX license header simplification
**As a** developer, **I want** the linter to replace verbose license headers with SPDX identifiers, **so that** file headers are concise and machine-readable.
**Acceptance criteria:**
- Given a license resource, when loaded, then its `spdx-identifier` slot is populated from front matter.
- Given a file with both an SPDX identifier and a verbose license block, when the maintainer runs, then the verbose block is removed.
- Given a proprietary file, when the maintainer runs, then the verbose block is preserved (no SPDX replacement).
- Given `LicenseRef-Proprietary` as the SPDX identifier for proprietary files, when the SPDX inspector checks, then it accepts this identifier.

### S3: Autofix signalling and maintainer maturity
**As a** developer, **I want** `lint-system :autofix t` to signal conditions before applying resolutions, **so that** I can selectively accept or skip fixes.
**Acceptance criteria:**
- Given a finding with a resolution, when autofix runs, then `resolution-proposed` is signalled with `apply-resolution` and `skip-resolution` restarts.
- Given a maintainer with `:maturity :experimental`, when autofix runs, then the resolution is signalled as a warning (not applied silently).
- Given a linter-configuration with `:maintainer-overrides`, when autofix runs, then the override disposition is respected.
- Given batch mode, when autofix runs, then stable maintainers auto-apply and experimental ones are skipped with a warning.

---

## Definition of Done
- [ ] All stories complete with acceptance criteria passing
- [ ] All tests passing
- [ ] `product/slice/006-linter-refinements/retrospective.md` created
