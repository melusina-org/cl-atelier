# Maturity Tracker: Atelier
**Last updated:** 2026-04-09

## Capability Maturity Levels

| Level | Meaning |
|-------|---------|
| Not started | No implementation work done |
| Foundation | Core abstractions and first concrete instances delivered; architecture proven |
| Capable | Multiple concrete instances; pipeline end-to-end for this level |
| Mature | Stable API; used in production; covered by fast and slow tests |

## Current State

| Capability | Goal | Level | Last changed | Slice |
|------------|------|-------|--------------|-------|
| Diagnostic Schema (finding/resolution hierarchy) | G7 | Foundation | 2026-04-06 | 001 |
| Linter ASDF Integration | G2 | Foundation | 2026-04-06 | 002 |
| File-level Inspection | G2 | Foundation | 2026-04-06 | 002 |
| Line-level Inspection | G2 | Foundation | 2026-04-07 | 003 |
| CST-level Inspection | G2 | Foundation | 2026-04-09 | 007 |
| Autofix / Write-back | G2 | Foundation | 2026-04-09 | 007 |
| Pretty-printer | G4 | Foundation | 2026-04-07 | 005 |

## Transition History

| Date | Capability | From | To | Slice |
|------|-----------|------|-----|-------|
| 2026-04-06 | Diagnostic Schema | Not started | Foundation | 001 |
| 2026-04-06 | Linter ASDF Integration | Not started | Foundation | 002 |
| 2026-04-06 | File-level Inspection | Not started | Foundation | 002 |
| 2026-04-07 | Line-level Inspection | Not started | Foundation | 003 |
| 2026-04-07 | CST-level Inspection | Not started | Foundation | 004 |
| 2026-04-07 | Autofix / Write-back | Not started | Foundation | 005 |
| 2026-04-07 | Pretty-printer | Not started | Foundation | 005 |
| 2026-04-09 | CST-level Inspection | Foundation | Foundation (refined) | 007 — three new diagnostic-only inspectors (check-single-branch-if, check-single-form-progn, check-when-not); 16 total. No level transition. |
| 2026-04-09 | Autofix / Write-back | Foundation | Foundation (refined) | 007 — test protocol sharpened: `(inspector, finding, maintainer, resolution)` quadruple now explicit in every fixture; N=1 self-idempotency enforced; pretty-printer fixed-point cross-population for syntax-inspector fixtures; zero write-back engine changes. Latent `*current-line-vector*` defvar bug fixed. No level transition. |
