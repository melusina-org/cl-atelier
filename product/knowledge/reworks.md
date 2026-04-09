# Rework Log

What was reworked, why, and what could have prevented it. The most direct learning signal a project produces.

---

## Slice 008, Phase 1: two ASDF-file Edit retries

**What was reworked:** Two `Edit` tool calls on `org.melusina.atelier.asd` failed because the `old_string` did not match the file's actual whitespace. First attempt used the Read output's displayed indentation verbatim and failed; second attempt copied the exact tab/space sequence from a narrower `Read` slice and succeeded.
**Trigger:** Indentation in the ASDF file uses tabs, and the two `(:module "inspectors" …)` blocks — one in the main system, one in the testsuite system — nest at different tab depths. The Maker assumed symmetry.
**Effort cost:** Minor — under a minute of friction, one extra `Read` per edit.
**Preventable?** Yes, trivially: when editing a file with mixed or tab-based indentation, read the exact byte sequence before the first edit attempt, not the summarised Read output.
**Lesson:** *Read the bytes, not the render.* The Read tool displays tabs in its own way; for whitespace-sensitive edits, a quick `awk 'NR>=X && NR<=Y' file` or `Grep` on the exact surrounding lines is the safer source of truth.

## Slice 008, Phase 1: acceptance criterion 3 not met literally

**What was reworked:** Nothing in the code — but the acceptance criterion was re-interpreted at review time rather than met as written. The plan said "pass count = 296"; reality was 295.
**Trigger:** Plan counted testcases; reality measured assertions. See `calibration.md` and `patterns.md`.
**Effort cost:** Zero code rework. The friction was in the phase-review phase, where the Reviewer had to mark AC3 as "met in spirit, literal value corrected."
**Preventable?** Yes: acceptance criteria on pass counts should be a *range* or *relative invariant* ("pass count decreases by at most N"), not a tight integer that assumes the plan's counting unit is correct.
**Lesson:** *Tight numeric acceptance criteria are only as good as the unit you counted.* Prefer "no new failures, no new skips, pass count does not decrease by more than X" over "pass count = Y."
