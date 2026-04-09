# Project Invariants

Project-wide constraints that are not in any single plan but apply across the project. Discovered through experience; violations have historically caused rework or incidents.

Numbering is global and continuous across all slices.

---

## INV-1: Discoverable fixtures are a convenience, not a goal

**Discovered:** slice 007, phase 1
**Invariant:** Autofix-cycle fixtures and inspector fixtures under `testsuite/fixtures/` are a testing convenience. If a maintainer's expected output does not fit the fixture format (e.g., semantically meaningful whitespace, or content the pretty-printer would canonicalise differently), the right answer is an ad-hoc `define-testcase` in `testsuite/maintainers/`, **not** a special case in the fixture loader.
**Rationale:** During slice 007, `fix-mixed-indentation` was force-fit into the discoverable format and produced a test that could never round-trip through the pretty-printer. Removing the fixture and moving the maintainer to an ad-hoc testcase was the clean answer. The principle is now in `CLAUDE.md`.

## INV-2: Per-maintainer self-idempotency at N=1 is required

**Discovered:** slice 007, phase 1
**Invariant:** Every registered maintainer must satisfy the property that re-running the `(inspector, maintainer)` pair on the result of a single fix yields the same result. This is enforced by `validate-one-autofix-cycle-fixture` at N=1. Pipeline idempotency (whole-file `lint-system :autofix t` reaching a fixed point) is strictly stronger and is a separate goal on the roadmap.
**Rationale:** Without N=1 self-idempotency, any maintainer can trivially diverge when applied in a loop. Ruff and ESLint both use hard iteration caps (`MAX_ITERATIONS` / `MAX_AUTOFIX_PASSES = 10`) rather than break cycles — Atelier's position is that per-maintainer N=1 is enforced first, pipeline idempotency is built on top. See `product/slice/007-maintainer-and-inspector-expansion/references/linter-convergence.md`.

## INV-3: The pretty-printer is the single authority on canonical Lisp text

**Discovered:** slice 007, phase 1
**Invariant:** For any syntax-level maintainer that emits text, the emitted text must be a `read ⟫ pretty-print-form` fixed point. The pretty-printer owns the one canonical form of any AST, and text-resolution maintainers at the syntax level must match it.
**Rationale:** Slice 007 enforced this as a cross-population test on all syntax-inspector fixtures. It keeps the inspector/maintainer pair from drifting away from the pretty-printer's output, which would make subsequent formatter passes undo the maintainer's work. Line-level fixtures are excluded because their expected whitespace is semantic.

## INV-4: Regression verification must use a fresh SBCL subprocess

**Discovered:** slice 007, phase 1 (root-cause of the `*current-line-vector*` bug)
**Invariant:** Any claim of "tests passing" that supports a slice closure, a release, or a merge must come from a fresh `sbcl --non-interactive` subprocess, **not** from the development REPL image. Live-image reloads mask load-order bugs because the order in which files happen to have been recompiled across editing sessions rarely matches the cold ASDF `:serial` order.
**Rationale:** Slice 007 surfaced the `*current-line-vector*` defvar-in-wrong-file bug that had been present since slices 003–004 but was masked for *months* because every development session had `write-back.lisp` (which held the defvar) already loaded before `runner.lisp` was recompiled. At cold start, `runner.lisp` saw the variable as lexical, not special, and `*current-line-vector*` was NIL inside `let` bindings that should have been dynamic. One fresh subprocess run would have caught it on day one. The discipline is now applied at every slice-level checkpoint.

## INV-5: Atelier does not police line length

**Discovered:** slice 008, phase 1
**Invariant:** Atelier does not report, warn about, or attempt to fix lines that exceed any nominal maximum length. The pretty-printer's `*print-right-margin*` is the only mechanism that influences line length, and its effect is advisory — a form that remains long after pretty-printing is accepted as-is. No `check-line-length`-style inspector and no `fix-line-too-long`-style maintainer should be reintroduced.
**Rationale:** The research in `product/reference/line-length-research.md` surveyed ESLint, Ruff, Black, Prettier, gofmt, rustfmt, and clang-format. The decisive finding was Black's explicit escape hatch — *"in those rare cases, auto-formatted code will exceed your allotted limit"* — which showed that even the most principled structural formatter cannot guarantee line-length satisfaction. gofmt refuses the rule entirely (*"Go has no line length limit"*). Atelier's pretty-printer is Oppen-style and already sophisticated enough to handle structural wrapping via `*print-right-margin*`; adding a separate reporter on top adds noise without adding correctness, and slice 007 had already established that the pretty-printer is the single authority on canonical Lisp text (INV-3). Slice 008 deleted the inspector, the maintainer, the finding class, three testcases, and twenty fixtures. This invariant is a candidate for promotion to a design principle in `product/definition.md` at the next Steward revision, parallel to how slice 007's idempotency invariant was promoted to design principle #7.
