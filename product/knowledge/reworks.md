# Rework Log

What was reworked, why, and what could have prevented it. The most direct learning signal a project produces.

---

## Slice 001, Phase 1: maintainer protocol evolved three times

**What was reworked:** The maintainer-to-finding coupling mechanism was redesigned twice before landing on CLOS method dispatch on the finding class.
**Effort cost:** Significant — three passes over the core protocol.
**Lesson:** *When designing a protocol between two types in a CLOS codebase, start with method dispatch on one type and ask "why not this?" before introducing any registry-level indirection.*

## Slice 002, Phase 1: finding subclasses were too generic

**What was reworked:** Both inspectors emitting plain `file-finding` instances. Rework added per-inspector finding subclasses.
**Lesson:** *"Agreed in planning" is not the same as "encoded in a step." Every architectural agreement must become a concrete acceptance criterion.*

## Slice 003, Phase 1: pipeline staging added after the fact

**What was reworked:** The staged `file inspectors -> read lines once -> line inspectors` runner design was acknowledged in discussion but not recorded in the plan.
**Lesson:** *A design decision that lives only in a conversation does not exist for the Maker.*

## Slice 004, Phase 1: three small reworks around the CST stage

**What was reworked:** (1) `define-syntax-inspector` generated wrong generic methods. (2) Redundant configuration parameter passing. (3) Awkward ASDF dependency inversion.
**Lesson:** *Functions inside a dynamic-binding context should read the bindings, not take them as parameters.*

## Slice 005, Phase 1: SBCL-pprint mismatch in a fixture

**What was reworked:** Expected output fixture didn't match SBCL's pretty-printer behavior for `flet`.
**Lesson:** *When the pretty-printer is in the loop, it is the only valid source of expected output. Never hand-write a fixture — derive it.*

## Slice 006, Phase 1: SPDX bulk replacement corrupted fixtures

**What was reworked:** Bulk SPDX header replacement also rewrote two test fixtures that deliberately contained the verbose block.
**Lesson:** *Bulk replacements are never safe without a post-pass `git diff` review of the test-fixture paths.*

## Slice 008, Phase 1: two ASDF-file Edit retries

**What was reworked:** Two `Edit` tool calls on `org.melusina.atelier.asd` failed due to whitespace mismatch.
**Lesson:** *Read the bytes, not the render. For whitespace-sensitive edits, use exact byte sequences.*

## Slice 008, Phase 1: acceptance criterion 3 not met literally

**What was reworked:** Pass-count prediction was off by 1 (counted testcases, not assertions).
**Lesson:** *Tight numeric acceptance criteria are only as good as the unit you counted.*

## Slice 009, Phase 1: Finding hierarchy changed from file-finding to line-finding

**What was reworked:** The implementation plan specified file-finding subclasses for system naming findings. The first test run crashed because text-resolution called finding-line on file-findings.
**Trigger:** The write-back engine's resolution-text-span method requires line/column positions that only line-finding (and its subclasses) provide.
**Effort cost:** minor — changed 3 subclass parents in finding.lisp and added line/column computation to the inspector.
**Preventable?** yes — reading resolution-text-span during planning would have revealed the constraint.
**Lesson:** When designing maintainers that produce text-resolutions, verify the finding class provides the slots that resolution-text-span reads.

## Rework: check-single-form-progn false positive on splat unquote

**What was reworked:** The inspector reported `(progn ,@body)` as a single-form progn. A splicing unquote may expand to zero or many forms at macro-expansion time, so this is a false positive.
**Trigger:** User reported the bug after linting macro-heavy code.
**Effort cost:** minor — added a `splicing-unquote-p` predicate and an extra guard in `single-body-p`.
**Preventable?** partially — the original inspector was written before quasiquote-aware testing existed. A fixture with `` `(progn ,@body) `` would have caught it.
**Lesson:** Syntax inspectors that walk CST trees must account for quasiquote constructs (`eclector.reader:unquote-splicing`, `eclector.reader:unquote`) as special cases.

## Rework: missing autofix for when-not and single-branch-if

**What was reworked:** Added `fix-when-not` and `fix-single-branch-if` maintainers. These inspectors existed since slice 004 but had no corresponding maintainers.
**Trigger:** User reported that these findings should be autofixable.
**Effort cost:** minor — straightforward syntax-resolution transforms.
**Preventable?** yes — every syntax inspector should ship with a maintainer when the transform is mechanical.
**Lesson:** When adding a syntax inspector, always ask: is the fix a mechanical AST transform? If yes, ship the maintainer in the same slice.

## Rework: describe-object on finding lacks autofix information

**What was reworked:** Added a `:after` method on `describe-object` for `finding` that calls `resolve-finding` and displays whether the finding is autofixable and which maintainers can handle it.
**Trigger:** User requested visibility into the finding-maintainer relationship.
**Effort cost:** minor — single `:after` method in `maintainer.lisp`.
**Preventable?** no — this is a usability improvement, not a bug.
**Lesson:** The finding→maintainer relationship is runtime-discoverable via `resolve-finding`. Surfacing it in `describe-object` makes the protocol self-documenting.

## Slice 011, Phase 1: `assert-t` on generalised-boolean return value

**What was reworked:** First draft of `validate-collect-lint-files-project-scope` used `(assert-t has-test-file)` where `has-test-file` came from `(some ... files)` with a `search` predicate. `some` returns the first non-nil value from the predicate, and `search` returns the integer position of the match — not `T`.
**Trigger:** Full test run surfaced 494/495 with one failure; the assertion failed with `EXPR: 31`.
**Effort cost:** minor — one edit to switch to `assert-t*`.
**Preventable?** yes — the project memory note explicitly flags that `assert-t` is strict. Reading that before writing the first assertion would have caught it.
**Lesson:** When asserting "this expression is truthy but not necessarily T", use `assert-t*`. When asserting "this expression is exactly T", use `assert-t`. `(some ...)` and `(search ...)` are generalised-boolean contexts.
