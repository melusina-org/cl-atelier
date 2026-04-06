# Retrospective: Slice 001 — Finding/Resolution Schema and Inspector/Maintainer Registry
**Recorded:** 2026-04-06
**Delivered by:** Maintainer + Claude Code
**Implementation phases:** 1 total — implementation-1.md

## Hypothesis Outcome
- Assumption tested: A unified finding/resolution class hierarchy with a single inspector/maintainer registry will allow all Atelier reporting tools to share one protocol, eliminating per-tool adapters for human and AI consumers.
- Leading indicator: Public API symbols — baseline 0 → result 95 (target ≥ 25)
- Verdict: ✅ Supported
- Kill criterion: 3rd-party companion system cannot define an inspector and maintainer using only the public API — not triggered. `define-inspector` and `define-maintainer` are exported macros usable from any package.
- Stories delivered: 8 of 8
- Quality criteria: all passed (fast tests < 2s, all exports documented, no SBCL-specific code without guard)

## Phase Delivery Summary
| Phase | Plan | Key deliverables | Deferred items |
|-------|------|-----------------|----------------|
| 1 | implementation-1.md | Finding hierarchy (5 classes), resolution hierarchy (5 classes), inspector registry via `define-named-class`, maintainer registry with `defmethod`-like `define-maintainer` macro and CLOS dispatch, legacy linter moved to `org.melusina.atelier/legacy`, bridge layer (`hint-to-finding`, `lint-with-findings`), Eclector CST hard dependency | None |

## What we learned
- The rework cycle was valuable: testing `make-instance` directly is useless — custom constructors (`make-file-finding`, etc.) are the right API surface to test. Short identifiers in tests obscure intent; descriptive names make tests self-documenting.
- Multi-language inspectors were a hallucination — inspectors don't need a `languages` slot. Language dispatch belongs elsewhere.
- The maintainer protocol evolved significantly: `reacts-to` → `resolves` → removed entirely. Maintainers are now independent from finding types — CLOS method dispatch on the finding type in `prepare-resolution` is cleaner than a registry-level filter. The `define-maintainer` macro with a `defmethod`-like body is the right ergonomics.
- Template files (`resource/template/`) are a hidden coupling surface — they generate code referencing Atelier symbols and must be updated when the public API changes.

## Impact on next prioritization
- Schema stability is confirmed. Successor slices (ASDF integration, first inspectors) can proceed with full confidence in the finding/resolution protocol.
- The `define-maintainer` macro's `defmethod`-like syntax is a validated pattern that should inform how future extensibility macros are designed.
- The legacy linter bridge validates that old and new can coexist — the migration path is proven.

## Maturity tracker changes
- Diagnostic Schema (G7): Not started → Foundation
