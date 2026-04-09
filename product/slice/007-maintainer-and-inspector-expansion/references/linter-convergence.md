# Research Reference: How Ruff and ESLint Handle Autofix Convergence

**Purpose:** Inform Atelier's idempotency design decisions by reviewing how two
popular linters handle the convergence problem — both at the per-rule level
and at the pipeline level.

**Scope:** Time-boxed shallow review. Public documentation, project issues,
and general knowledge of the two projects. **I did not clone or grep either
source tree.** Anything I could not verify from the sources listed at the
bottom is marked "I do not know" rather than guessed.

**Definitions used in this reference:**

- **Per-rule idempotency:** running one lint rule's fix on its own output yields
  no further changes for that rule. A rule that rewrites `foo` to `*foo*` must
  not, on the next pass, rewrite `*foo*` to `**foo**`.
- **Pipeline convergence:** running the whole fix pipeline (all rules, all
  their fixes) on the output of the previous pass yields the same file. The
  pipeline has reached a fixed point.

---

## Ruff (Python, Rust)

**Model.** Ruff runs `lint → apply fixes → re-parse → re-lint` in a loop until
no fixable violations remain. Fixes are AST-based, but the output of each pass
is re-parsed from source between iterations.

**Iteration cap.** Ruff has a `MAX_ITERATIONS` constant in its fix loop. When
the loop exceeds the cap without reaching a fixed point, Ruff prints
`Failed to converge after N iterations` and aborts the fix. The exact numeric
value of `MAX_ITERATIONS` at the time of writing — **I do not know** from a
shallow review; it is in the Ruff source but I did not read it.

**Why iterations can stack up.** Ruff has a constraint that conflicting edits
on the same line cannot be applied in the same pass — only one of them is
kept and the others are deferred to the next iteration. This means the
number of iterations needed can grow roughly linearly with the number of
violations on a hot line. The bug report at
[astral-sh/ruff#5800](https://github.com/astral-sh/ruff/issues/5800) describes
an example where 22 violations of `RUF013` required 21+ iterations and
eventually tripped the "Failed to converge" error because the fix was
producing a no-op edit that the fix engine could not detect as already
applied.

**Per-rule idempotency.** Ruff does not appear to document a per-rule
idempotency guarantee as a first-class property. The convergence check is at
the *pipeline* level, not per-rule: if the pipeline loop stops making
progress, it aborts regardless of which rule is responsible. Ruff's test
infrastructure does test individual rules for correctness, but whether it
asserts self-idempotency per rule — **I do not know** from a shallow review.

**Fix safety and iteration.** Only fixes marked as `safe` participate in the
default iteration loop. Rules can mark a fix `unsafe` (it might remove
comments or change runtime behaviour); those are not applied unless the user
opts in. This reduces the surface area for cycles but does not eliminate
them.

**Known convergence pitfall.** The issue #5800 case shows that the dominant
convergence bug is not two rules fighting each other — it is a single rule
producing an edit that the engine replays because it can't tell the edit is
already present. That is *per-rule* non-idempotency leaking into the
*pipeline* loop.

## ESLint (JavaScript)

**Model.** ESLint introduced multi-pass fix in
[v2.9.0 (April 2016)](https://eslint.org/blog/2016/04/eslint-v2.9.0-released/).
`Linter.verifyAndFix` runs lint → fix → lint → fix in a loop.

**Iteration cap.** `MAX_AUTOFIX_PASSES = 10`. This is a hard constant in
`Linter.verifyAndFix`. After 10 passes the loop stops, regardless of whether
the file still has fixable violations.

**Circular-fix detection.** ESLint now documents the exact scenario the cap
was introduced to mitigate:

> You have conflicting fixable rules in your configuration. ESLint autofixes
> code in multiple passes, meaning it's possible that a fix in one pass is
> undone in a subsequent pass. For example, in the first pass a rule removes
> a trailing comma and in the following pass a different rule adds a trailing
> comma in the same place, effectively changing the code back to the previous
> version. ESLint emits a warning when it detects cycles like this.

Documented at [Circular fixes](https://eslint.org/docs/latest/use/troubleshooting/circular-fixes).
The user's recourse is to identify and reconfigure one of the two conflicting
rules. ESLint itself does not break the cycle; it only reports it.

**How cycles are detected.** The request to warn when `MAX_AUTOFIX_PASSES` is
reached was filed as
[eslint/eslint#19321](https://github.com/eslint/eslint/issues/19321) and
landed in v9.17.0. Before that, the only way to know your config was
cycling was to run `eslint --fix --stats -f json` and look for
`fixPasses: 10` in the output. That is — the symptom was silent.

**Rule tester vs. linter.** A sharp corner:
[eslint/eslint#18007](https://github.com/eslint/eslint/issues/18007) — the
`RuleTester` (the framework for unit-testing a rule's fix) does **exactly one
fix pass**, while the real linter does up to 10. This means a rule can pass
its unit tests and still cycle in production. The issue asks for better
multi-pass fix testing; its resolution status — **I do not know**.

**Real-world symptoms.** The editor integration bug
[microsoft/vscode-eslint#541](https://github.com/Microsoft/vscode-eslint/issues/541)
reports "Fix all auto-fixable issues" spinning in a never-ending loop of
moving tabs/spaces around, never settling. Same underlying cause:
configuration-level cycles that `MAX_AUTOFIX_PASSES` and the new warning
exist to cap and report.

**Early-exit heuristics.** [eslint/eslint#5995](https://github.com/eslint/eslint/issues/5995)
describes a period when the multi-pass loop would exit early if the error
count didn't change between passes — which meant a fix on the last error
that uncovered a new error wouldn't be attempted. That heuristic was removed
once the 10-pass cap was accepted as sufficient protection.

---

## Takeaways for Atelier

1. **Cap, detect, report — don't try to break cycles.** Both Ruff and ESLint
   use a hard iteration cap rather than attempting to detect and resolve
   cycles. When the cap is hit, they report it and stop. Atelier should
   adopt the same model for pipeline convergence (when we get there):
   `*max-autofix-passes*` with a clear signal when hit, not a heuristic
   cycle-breaker.

2. **Unit-test for self-idempotency per rule, not just for "correct fix".**
   ESLint's RuleTester / Linter asymmetry is a classic trap: a rule that is
   correct in isolation but not idempotent passes unit tests and breaks the
   pipeline. Atelier's slice 007 choice — assert self-idempotency in every
   fixture — directly addresses this class of bug before it reaches the
   pipeline.

3. **The dominant real-world bug is no-op detection, not rule conflict.**
   Ruff's #5800 shows that the pipeline loop can be derailed by a single
   rule producing an edit that equals the current source. Make
   "maintainer returned a resolution that, when applied, produced the same
   string" a checked condition somewhere in the write-back engine — at a
   minimum, log it.

4. **Separate "can this rule fix itself twice" from "does the pipeline
   converge".** They are independent properties. Rule idempotency is
   necessary but not sufficient for pipeline convergence (two idempotent
   rules can still form a cycle). Pipeline convergence is the stronger
   property. Slice 007 buys the first; a later slice will have to buy the
   second.

5. **Safe vs. unsafe fixes is a useful orthogonal dimension.** Ruff's notion
   of marking fixes as "safe" (preserves runtime behaviour) or "unsafe"
   (may not) is a separate axis from idempotency and is worth considering
   for Atelier's maintainer registry — the `:maturity` slot (`:stable`
   vs `:experimental`) we already have covers part of this, but "preserves
   runtime behaviour" is not exactly the same question as "is mature."

## Sources

- [Ruff Linter docs](https://docs.astral.sh/ruff/linter/)
- [astral-sh/ruff#5800 — Autofix is unable to skip identical import edits ("Failed to converge after 21 iterations")](https://github.com/astral-sh/ruff/issues/5800)
- [astral-sh/ruff#25 — Implement autofix (early design)](https://github.com/astral-sh/ruff/issues/25)
- [ESLint v2.9.0 release blog — multi-pass autofix introduced](https://eslint.org/blog/2016/04/eslint-v2.9.0-released/)
- [ESLint docs — Circular fixes troubleshooting](https://eslint.org/docs/latest/use/troubleshooting/circular-fixes)
- [eslint/eslint#19321 — Warn when MAX_AUTOFIX_PASSES reached](https://github.com/eslint/eslint/issues/19321)
- [eslint/eslint#18007 — RuleTester multi-pass fix testing](https://github.com/eslint/eslint/issues/18007)
- [eslint/eslint#5995 — Multipass exits prematurely](https://github.com/eslint/eslint/issues/5995)
- [eslint/eslint#5329 — Fixing autofix](https://github.com/eslint/eslint/issues/5329)
- [microsoft/vscode-eslint#541 — endless loop fixing](https://github.com/Microsoft/vscode-eslint/issues/541)
