# Line-Length Auto-Fix: How Other Linters and Formatters Handle It

**Recorded:** 2026-04-09
**Author:** Strategist (pre-slice research for slice 008)
**Purpose:** Survey the design space for automatic line-too-long fixes in mature linters and formatters, so the Atelier `fix-line-too-long` maintainer can be designed from an informed baseline rather than reinvented.

This is a lean research pass, not an exhaustive survey. Primary sources were fetched where available; where fetches failed or coverage was thin, the author's prior knowledge is flagged explicitly.

---

## 1. ESLint — `max-len`

**Does it auto-fix?** No.

The official rule page at <https://eslint.org/docs/latest/rules/max-len> describes the rule purely diagnostically:

> "This rule enforces a maximum line length to increase code readability and maintainability. The length of a line is defined as the number of Unicode characters in the line."

The rule page carries no "🔧 fixable" marker (ESLint uses a standard icon in its rule index to indicate auto-fixable rules). No reformulation of the rule as a fixer has been merged upstream despite being a recurring request in the issue tracker over the years.

**Author's knowledge (not freshly cited):** The maintainers' consistent position is that safe automatic line-breaking requires language-level understanding that belongs in a formatter (Prettier), not a linter. This is the same position Ruff now takes.

**Plugins / codemods:** None that the author is aware of attempt a general `max-len` fixer. The community answer is "use Prettier."

---

## 2. Ruff — `E501` (`line-too-long`)

**Does `ruff check --fix` fix it?** No.

The rule page at <https://docs.astral.sh/ruff/rules/line-too-long/> states:

> "Checks for lines that exceed the specified maximum character length."
>
> "Overlong lines can hurt readability. PEP 8, for example, recommends limiting lines to 79 characters. By default, this rule enforces a limit of 88 characters for compatibility with Black and the Ruff formatter, though that limit is configurable via the line-length setting."

The rule page carries no "Fix" section and no "Availability" section indicating autofix support. Ruff's convention is to list fix safety explicitly on rules that have fixes; its absence on E501 is the documentation's way of saying "no fix."

**The Ruff formatter is the answer.** Ruff ships a separate `ruff format` command, which is a Black-compatible reimplementation. The documented expectation is that users run `ruff format` to get line-wrapping behavior, and `ruff check` (with or without `--fix`) will report remaining E501 issues the formatter could not resolve. This splits cleanly into "linter reports; formatter fixes."

---

## 3. Black — the reference implementation of structural line-wrapping

Quotes below are from the canonical `current_style.md` in `psf/black` on GitHub.

**Default line length:**

> "_Black_ defaults to 88 characters per line, which happens to be 10% over 80."

**Splitting strategy — a recursive bracket-peeling algorithm:**

> "_Black_ will look at the contents of the first outer matching brackets and put that in a separate indented line."
>
> "[If further splitting is needed,] it will decompose the internal expression further using the same rule, indenting matching brackets every time."
>
> "For comma-separated contents, _Black_ will first try to keep them on the same line with the matching brackets. If that doesn't work, it will put all of them in separate lines."

So Black's algorithm is roughly:
1. Try the whole expression on one line.
2. If it doesn't fit, break inside the outermost bracket pair.
3. Recurse.
4. For comma-separated content, try "all on one line inside brackets" first; if that doesn't fit, explode to "one element per line with trailing comma."

**The magic trailing comma — user override:**

> "When you do [add a trailing comma], _Black_ will know to always explode your collection into one item per line."

The trailing comma is a user-controlled instruction that *forces* the exploded form even when the collapsed form would fit. Removing it allows Black to collapse again. This is a small but important design point: the formatter respects a user-visible intent marker embedded in the source.

**Explicit acknowledgement that Black sometimes fails:**

> "However, sometimes it won't be able to without breaking other rules. In those rare cases, auto-formatted code will exceed your allotted limit."

This is the most important quote for our design. Black *does not guarantee* that no line exceeds the limit. Long strings, long identifiers, long URLs in comments, and atomic expressions are allowed to overflow rather than be corrupted. **Correctness outranks the line-length constraint.**

**Known failure modes (author's prior knowledge, not freshly cited):** By default Black does not split string literals. There is an opt-in `--preview` / `--experimental-string-processing` mode that adds string splitting. Long single identifiers (e.g., a deeply qualified name) are atomic and will overflow. Long URLs in comments are atomic.

---

## 4. Prettier — Wadler's "A Prettier Printer"

**Author's prior knowledge, not freshly cited in this research pass.**

Prettier's algorithm is the canonical implementation of Philip Wadler's *A Prettier Printer* functional pearl. The source text is parsed into an intermediate **doc tree** built from a small vocabulary of constructors:

- `text "..."` — raw text that must appear
- `line` — a mandatory line break
- `softline` — a break that becomes a space when the group fits and a newline when it doesn't
- `hardline` — always a newline
- `indent n doc` — wraps a sub-doc in extra indentation
- `group doc` — a unit that is either flattened (all breaks become spaces) or exploded (all breaks become newlines), atomically
- `fill` — a fill-style group that breaks only when necessary

The rendering algorithm walks the doc tree with the current column position and the target `printWidth`. At each `group`, it asks: "If I flatten this group onto the remaining line, does it fit?" If yes, it flattens. If no, it explodes every `line`/`softline` in the group to a newline with indentation. This is a greedy-with-lookahead strategy; Wadler proved it runs in linear time when memoised correctly.

**Why this matters for Atelier:** Common Lisp's standard `pprint-logical-block` is Oppen-style (Derek Oppen 1980), which is the direct ancestor of Wadler's algorithm. An Oppen printer also has a "does this fit?" decision at logical block boundaries, with a similar fits-or-breaks effect. The vocabulary is different (`pprint-newline :fill`, `:linear`, `:miser`, `:mandatory`) but the semantics are cousins.

---

## 5. gofmt — deliberate non-policy

From *Effective Go*, <https://go.dev/doc/effective_go>:

> "Go has no line length limit. Don't worry about overflowing a punched card. If a line feels too long, wrap it and indent with an extra tab."

This is a deliberate design position: Go's formatter refuses to enforce a line length *at all*. The rationale, consistent with Rob Pike's repeated public statements, is that an automatic line-length enforcer produces worse output than a human would, so the language simply doesn't have the rule.

**This is a real option for Atelier.** "Don't implement fix-line-too-long; leave check-line-length as a reporter only" is a position with industry precedent.

---

## 6. rustfmt, clang-format, checkstyle (brief)

**rustfmt** — has `max_width = 100` by default and attempts to wrap at expression boundaries. Known to leave lines over the limit when it cannot find a split, emitting a warning. (Author's prior knowledge; doc fetch not performed this pass.)

**clang-format** — uses a *penalty-based* optimization model. Options like `PenaltyExcessCharacter`, `PenaltyBreakString`, `PenaltyBreakAssignment` let users tune a dynamic-programming search over possible break positions. Conceptually close to Knuth-Plass line breaking. The fetch of the options reference confirmed:

> "The column limit. A column limit of `0` means that there is no column limit. In this case, clang-format will respect the input's line breaking decisions within statements unless they contradict other rules."

The penalty details were not in the fetched excerpt; author's prior knowledge is that the model minimises a weighted sum of break costs and overflow characters.

**checkstyle `LineLength`** — report-only. The Java ecosystem delegates fixing to google-java-format / Spotless, which implement their own structural formatter. (Author's prior knowledge.)

---

## 7. Prose/comment wrappers (brief)

**Emacs `fill-paragraph` / `auto-fill-mode`** — character-level prose wrapping, driven by `fill-column` and `adaptive-fill-regexp`. Universally used for comments and docstrings in Emacs Lisp development. Paredit, Lispy, SLIME, and SLY do *not* offer structural line wrapping for code; they all fall back to `fill-paragraph` for comments and leave code untouched.

**`fmt(1)`, `par(1)`** — classical Unix prose wrappers. `fmt` is greedy; `par` is more sophisticated (balanced lines, prefix-aware). Both operate on characters, not on syntax, and are still used for wrapping comments and docstrings inside editors.

These are relevant because **wrapping a comment or docstring is a genuinely different sub-problem** from wrapping code. A comment wrapper can use greedy character-based wrapping safely. A code wrapper cannot.

---

## 8. Taxonomy of approaches

| Approach | Examples | Essence | Cost |
|---|---|---|---|
| **Report only** | ESLint `max-len`, Ruff `E501` check, checkstyle `LineLength`, gofmt (no check at all) | The linter never edits code; humans or formatters fix. | Low implementation cost. Users must run a separate tool. |
| **Pretty-printer owns canonical text** | Black, `ruff format`, Prettier, google-java-format, rustfmt, clang-format | The formatter re-emits the whole form (or file) and the line-length constraint is one input to its layout algorithm. | High implementation cost. Requires a complete, correct emitter. Can guarantee stability (idempotent) but not line-length satisfaction. |
| **IR / doc-tree algorithms** | Prettier (Wadler), Oppen-style printers (CL `pprint-logical-block`), clang-format (Knuth-Plass-ish penalties) | The formatter builds an intermediate document with break opportunities, then searches for a layout that fits. | The most principled approach. Extensible to new constructs. Line-length is a soft constraint. |
| **Prose wrapping** | Emacs `fill-paragraph`, `par`, `fmt` | Character-level greedy wrapping for text. Knows nothing about syntax. | Cheap and adequate for comments and docstrings; wrong for code. |
| **Hybrid** | (none in common use the author knows of) | Mix strategies per finding. | Not in the literature. |

---

## 9. Open design questions for Atelier's `fix-line-too-long`

These are the questions the slice has to answer before any code is written.

**Q1. Maintainer or formatter pass?**
Is `fix-line-too-long` an ordinary maintainer emitting a `syntax-resolution` on a single diagnostic, or is it a formatter pass that re-emits the whole enclosing top-level form through the pretty printer at the configured `*print-right-margin*`? Slice 007's retrospective already points out that "the pretty-printer is the single authority on canonical Lisp text for syntax-level maintainers." This argues for formatter-pass, but formatter-pass has convergence implications with other maintainers.

**Q2. Scope: code, comments, strings, or all three?**
The three sub-problems are genuinely different:
- **Code** — structural: split at logical break points using the pretty printer.
- **Line comments** (`;`, `;;`, `;;;`, `;;;;`) — prose: `fill-paragraph`-style wrapping, respecting the comment prefix.
- **String literals** — rare; Black explicitly does not do this by default, and the author recommends Atelier also punt on it.

Which of these does slice 008 cover? Most-ambitious-first: code only; comments only; both; neither (report-only)?

**Q3. What do we do when the pretty printer cannot shorten the line?**
Black's explicit "auto-formatted code will exceed your allotted limit" escape hatch is the right model. The maintainer must be allowed to *skip* a finding without failing, and the inspector must not re-report the same unsplittable line on the next pass (or the pipeline will diverge). This is exactly the "fix-point requires the inspector to recognise 'as short as possible'" problem and it ties into the linter-convergence work from slice 007.

**Q4. How does wrapping interact with comments on the same logical line?**
`(foo bar baz) ; trailing comment` — is the comment protected? Does wrapping preserve it? The CL pretty printer has limited comment support; `trivial-formatter` and community pretty printers typically strip or mangle comments. This is a real implementation risk.

**Q5. Reader conditionals and block comments.**
`#+sbcl`, `#-ecl`, `#|...|#` — these are outside the standard CLtL2 pretty printer's model. If we re-emit a whole top-level form that contains reader conditionals, we risk reordering or corrupting them. Does the Eclector CST path give us enough fidelity to preserve them, or is this a hard limit?

**Q6. Cost of re-emitting the whole enclosing form.**
If the inspector reports "line 47 is too long" inside a 200-line `defun`, and the maintainer re-emits the whole `defun` through `pretty-print-form`, every whitespace and reader macro in the defun goes through the printer. Slice 007 emphasises that "inspection is fast; resolution is slow and rare" — this is fine in principle, but the maintainer must not silently rewrite the other 199 lines cosmetically. Either we accept the rewrite as a deliberate side effect, or we scope the resolution to the *smallest* enclosing CST node that contains the offending line.

**Q7. Convergence with other maintainers.**
If `fix-line-too-long` re-indents a form and `fix-mixed-indentation` has opinions about that same form, we risk a cycle. Slice 007's `linter-convergence.md` already notes that Ruff and ESLint both use hard iteration caps. Does `fix-line-too-long` need to run *last* in the maintainer order, after all pure textual fixes have stabilised?

**Q8. Configuration surface.**
Black chose 88. ESLint defaults to 80. Ruff defaults to 88. Go refuses to choose. Atelier's current `check-line-length` default should be confirmed (author believes it is 80 — verify in the slice), and the configuration path (`linter-configuration.sexp`) should be part of scope.

---

## 10. Recommended reading of this report

The single most consequential finding is **Black's explicit escape hatch**: *"auto-formatted code will exceed your allotted limit"* in rare cases. Any Atelier `fix-line-too-long` design that treats line-length as a hard invariant — i.e., that refuses to converge until the line fits — is fighting industry practice. The right invariant is:

> The line was split *as much as the structural rules allow*, and further splitting would violate correctness. If the result still exceeds the limit, the finding is marked *unfixable* and suppressed.

Slice 008 should probably begin by deciding between three shapes:

- **Shape A — Report-only, no fix.** Follow gofmt's lead. Drop the 20 staged fixtures. Minimal work, honest signal. Unblocks nothing but closes the question.
- **Shape B — Code-only structural maintainer.** Re-emit the smallest enclosing CST node through the pretty printer at `*print-right-margin*`, with an "unfixable" escape hatch. Does not touch comments or strings.
- **Shape C — Code + comment wrapping.** Adds a line-comment wrapper (prose-style, `fill-paragraph`-like) as a second maintainer kind under the same inspector.

The Strategist's reading of the 20 staged fixtures in `testsuite/fixtures/autofix/fix-line-too-long/` is that they are all *code*, not comment cases — `defun-key-params.text`, `long-loop.text`, `make-instance-many.text`, etc. This suggests Shape B was the original intent. Shape A and Shape C are real alternatives that deserve an explicit rejection in the slice definition.
