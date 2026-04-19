# Implementation Phase 1: Slice 006 — Linter Refinements

**Phase:** 1 of 1
**Slice:** product/slice/006-linter-refinements/slice.md
**Scope:** All three stories (S1 fixture auto-discovery, S2 SPDX simplification, S3 autofix signalling)

## Prior phases
- Slice 005 complete: 322/322 tests, 10 inspectors, 10 maintainers, write-back engine, pretty-printer, batch autofix.

## Implementation Order

| Step | File | Action | Description |
|------|------|--------|-------------|
| 1 | `test/utilities.lisp` | Modify | Add `maintainer-fixture`, `pretty-printer-fixture`, `inspector-fixture` accessor functions |
| 2 | `test/fixtures/` | Reorganize | Move existing fixtures to per-inspector/maintainer subdirectories |
| 3 | `test/utilities.lisp` | Modify | Add `discover-maintainer-fixtures`, `discover-pretty-printer-fixtures` auto-discovery |
| 4 | `test/entrypoint.lisp` | Modify | Add auto-discovered fixture testcase |
| 5 | `resource/license/*.text` | Modify | Add `spdx:` field to YAML front matter for all license resources |
| 6 | `src/license.lisp` | Modify | Add `spdx-identifier` slot to `license` class, update loader |
| 7 | `src/package.lisp` | Modify | Export `license-spdx-identifier` |
| 8 | `src/inspectors/check-spdx-license-header.lisp` | Modify | Use `license-spdx-identifier` when available |
| 9 | `src/maintainer.lisp` | Modify | Add `:maturity` slot (`:stable` / `:experimental`) to `maintainer` class |
| 10 | `src/asdf.lisp` | Modify | Add `resolution-proposed` condition, restarts, `maintainer-overrides` in linter-configuration |
| 11 | Tests | Create/modify | Tests for all new behavior |

## Acceptance Criteria
1. `(maintainer-fixture 'atelier:fix-bare-lambda)` returns a valid pathname
2. Auto-discovered fixtures produce passing tests
3. License resources have `spdx-identifier` populated
4. `resolution-proposed` is signalled during autofix with `apply-resolution` / `skip-resolution` restarts
5. `:maturity :experimental` maintainers warn instead of auto-applying in batch
6. `:maintainer-overrides` in linter-configuration respected
7. All tests passing
