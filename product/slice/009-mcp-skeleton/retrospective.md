# Retrospective: Slice 009 — MCP server skeleton

**Recorded:** 2026-04-11
**Implementation phases:** 1 total — `implementation-1.md`

## Delivery Summary

- **Stories delivered: 8 of 8** (S1 ASDF system + package, S2 MCP handshake, S3 tool registry + 6 tools, S4 resource registry + Atelier registry resources, S5 `lisp://systems`, S6 transcript subsystem, S7 abstract `image-connection` class, S8 testsuite).
- **Acceptance criteria: 12 of 12 pass.** Every AC verified by an automated testcase; the Reviewer audit found zero orphan exported symbols and zero SWANK references in live code.
- **Quality criteria: all passing.** Fresh-SBCL subprocess load test is *inside* the suite now (automates INV-4 from discipline to enforced property). Atomic transcript writes verified. Registry counts drift-checked on every run.
- **Full test suite: 479/479 in a fresh SBCL subprocess — 295 base atelier assertions (zero regression) plus 184 new MCP assertions — all in one `(atelier/testsuite:run-all-tests)` call.** The consolidated `.asd` places `testsuite/mcp/` as a module inside `org.melusina.atelier/testsuite`, so the MCP tests are reachable through the canonical project entry point and not through a separate test system.

## Phase Notes Summary

| Phase | Key deliverables | Deferred to backlog |
|-------|------------------|---------------------|
| 1 | Full MCP skeleton: system, package (85 exports), JSON utilities, `define-tool` macro with 17 helpers (each ≤25 lines), 7 message classes, 7 dispatcher methods, 7 MCP method handlers, 6 tools, 8 resources (3 concrete + 5 templated), transcript subsystem (sexp/JSON/Markdown), abstract `image-connection` with three generics, testsuite with 184 assertions, plus five reference documents and one plan amendment. | None. Single-phase slice. |

## Hypothesis Outcome

This is an OSS-mode slice (no formal hypothesis canvas), but the delivery shape maps to:

- **Assumption tested:** Atelier's finding/resolution schema and tool/resource registry can be exposed over MCP without a redesign, and the slice-010 SWANK bridge can be scaffolded as a pure class abstraction before any transport code is written.
- **Evidence:** verdict ✅. The abstract `image-connection` class accommodates a socketpair transport, a TCP transport, or a "skip SWANK entirely" transport without changing its three-generic signature. The unified `define-tool` macro handles tool-only, concrete-resource, templated-resource, and resource-only definitions through one syntactic form — the amendment needed to split `resources/list` from `resources/templates/list` did not change the macro, only the dispatcher's output path.
- **Verdict:** ✅ Supported.
- **Kill criterion:** not applicable (OSS-mode).
- **Stories delivered:** 8/8.
- **Quality criteria:** all passed.

## Goal Progress

- **G5 (MCP server)** — moved from **Not started** to **In Progress**. Slice 009 is the foundation slice; G5 does not reach **Reached** until the user-facing capability (eval, debugger, refactoring) is delivered, which begins in slice 010 and continues through slice 016. Slice 009's delivery removes the single biggest risk on the G5 path — architectural coherence — by exercising the end-to-end protocol surface in its skeleton shape. Every subsequent slice adds handlers to the registry and methods on existing generics; none should require reshaping the schema.
- **No other goal moves.** Slice 009 touches G7 (diagnostic schema) indirectly by making the finding/resolution shape visible as a resource, but the schema itself is unchanged.

## What we learned

**The foundation-slice anti-pattern did not materialize.** R1 — "hallucinated requirements from surface intuition" — was the #1 risk going in. Every exported symbol in `#:atelier/mcp` traces to a specific acceptance criterion, verified at phase closure. The discipline of asking "which story needs this symbol?" at every slot-and-slot-accessor definition kept the surface area honest. **Eighty-five exports is a lot** for a single slice, but every single one is load-bearing. The risk mitigation (audit at phase closure) is what made the big surface safe.

**`define-tool` at 17 helpers, each ≤25 lines, is a case study in macro decomposition.** My first instinct was to write the macro as one large function that parses, validates, and expands. The user's "keep everything under 25 lines" guidance forced a different shape: parser → validator → class-name derivation → metadata methods emitter → handler-call method emitter → registration emitter, each testable in isolation. The macro body itself became a four-line `progn` over helper calls. This is the shape I'd choose next time even without the line-length constraint — every helper has a name that matches what it does, and every rework during Phase 2 touched exactly one helper.

**CL surprises cost more than they should have.** Nine reworks in Phase 2 — five of them were CL-level surprises documented in `MEMORY.md` or in the Common Lisp skill reference: `nil`/`false`/`null` overloading in jzon, `assert-t` strictness vs. `assert-t*`, plist-reversal via `nreverse`, `defclass` slot initarg rejection, and `prin1` case on keywords. **I had the knowledge; I didn't re-read it before starting Phase 2.** The lesson for future slices is concrete: before the Maker begins execution, re-read `product/knowledge/` *and* the relevant skill references, even when the phase plan says "refer to them as needed." This is a candidate for a new pattern entry.

**Plan amendments during execution are real, and the append-don't-rewrite protocol works.** During step 2 (writing `references/mcp-protocol.md`), the MCP spec revealed that `resources/list` and `resources/templates/list` are distinct methods — the plan had assumed a single method for all 8 resources. I stopped, wrote an "Amendment 1" section at the bottom of `implementation-1.md`, described the trigger and the impact, and then continued. The original plan's narrative stays intact; the amendment is the durable record of the deviation. **This is the first time I've used the amendment workflow and it handled the situation cleanly** — no slice-level rework, no confused plan-vs-reality drift, and the Reviewer had a single place to audit the divergence. Recommend formalising this as the standard deviation-during-execution protocol.

**Slow tests don't have to be skipped — they can be fast enough to always run.** Six slow tests (3 filesystem + 2 torn-write + 1 fresh-SBCL subprocess) all run on every `asdf:test-system` invocation and add ~3 seconds total to the suite. The fresh-SBCL subprocess test is the single biggest slowdown (~2.5s for a cold SBCL spawn), but the cost is well-spent: **the test turns INV-4 from "the maintainer has to remember to run a cold test" into "the suite cannot pass without a cold test."** This is the kind of discipline that pays forever. The pattern is worth generalising: any invariant about *how tests were run* is better enforced by a test that checks the invariant than by a process rule.

**The `with-isolated-registries` macro is the hard fix for test pollution.** Slice 005's retrospective noted the `*inspectors*`/`*maintainers*` registry-pollution problem and deferred the hard fix. Slice 009 took it as a given that tests that define tools must not pollute the production registries, and wrote the macro from day one. The result: every test that calls `define-tool` at runtime is wrapped in `with-isolated-registries`, the production registries stay clean across test runs, and no "TEST" package heuristic is required. **The hard fix is one macro and five lines of documentation.** Recommend the inspector and maintainer registries adopt the same pattern in a future maintenance slice.

## User feedback since delivery

*(to be filled 2–4 weeks after release — leave blank initially)*

## Backlog and roadmap changes

- **Completed (this slice):** backlog item #5 (MCP server skeleton), slice.md stories S1–S8, acceptance criteria 1–12.
- **Added:** the five reference documents (`jzon-round-trip.md`, `mcp-protocol.md`, `define-tool-macro.md`, `message-hierarchy.md`, `python-prototype-notes.md`) are permanent — they're the shared understanding slice 010+ will read.
- **Reprioritized:**
  - Backlog item #6 (MCP child image + eval) moves to *next* slice. The Tactician should re-read `python-prototype-notes.md` §"Useful patterns to revisit in slice 010" before planning.
  - No changes to slice 010–016 scope from the slice 009 planning.
- **Not reprioritized, noted for Reviewer knowledge consolidation:**
  - Invariants INV-12 through INV-16 proposed in the implementation notes are ready for promotion to `product/knowledge/invariants.md`.
  - Calibration data point: 184 assertions actual vs. 50–100 predicted range is additional evidence for the "counting the wrong unit" hypothesis. Suggest future plans for test-heavy slices use a 3–5× wider range.
  - Rework count (9) and rework pattern (5 of 9 were CL-surprise fixes documented in existing knowledge) suggests a new pattern entry: *"Skim-then-code does not work for CL surprises — re-read before Phase 2."*

## Verdict

**✅ Supported. Delivered as scoped plus one spec-compliance amendment (`resources/list` / `resources/templates/list` split).**

The slice is a clean example of the foundation-slice delivery shape working through the five-role protocol: planning caught most of the real risks before code was written (8 risks × clear mitigations), the Maker execution hit 9 small reworks of which all 9 were caught within one test cycle and none required plan revision, the Reviewer audit confirmed zero scope creep and zero orphan symbols, and the test suite now contains automated INV-4 enforcement that benefits every slice going forward. The amendment workflow handled a mid-execution spec discovery without disrupting the plan narrative. Combined, this is the kind of foundation slice that makes subsequent slices cheap.

**Close the slice. Slice 010 can begin.**
