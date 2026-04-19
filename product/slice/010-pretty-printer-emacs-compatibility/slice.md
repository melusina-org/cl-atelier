# Slice 010: Pretty-Printer / Emacs Compatibility

**Status:** Planned
**Type:** Improvement
**Goal addressed:** G2, G4
**Backlog items:** #1
**Planned start / end:** 2026-04-16 / 2026-04-20
**Actual end:**
**Implementation phases:**
  - Phase 1: product/slice/010-pretty-printer-emacs-compatibility/implementation-1.md — Planned

---

## What changes for users

After this slice, Atelier's pretty-printer and Emacs (SLIME/SLY) agree
on how Common Lisp code is formatted. A developer can run
`atelier:lint-system :autofix t`, open the result in Emacs, and see no
indentation fights. The `.dir-locals.el` or Emacs configuration needed
to achieve this agreement is documented and provided.

## Specification references

- ANSI CL §22.2 (Pretty Printer)
- SLIME `contrib/slime-indentation.el` and `slime-cl-indent.el`
- SLY `lib/sly-indentation.el`
- Emacs `cl-indent.el` (built-in Common Lisp indentation)

## Stories

### S1: Representative pretty-printer fixture
**In order to** have a concrete, comprehensive reference for canonical
formatting, **a** maintainer **can** point to a single `.lisp` file
that exercises all formatting decisions.
**Acceptance criteria:**
- Given a file `test/fixtures/pretty-print/reference.lisp`,
  when it contains representative Common Lisp constructs (see list below),
  then it covers at minimum:
  - Constants and parameters initialized with complex property-lists
  - Package definitions with `:import-from`, `:export`, etc.
  - Class definitions with complex slot specs and numerous options
  - Functions/methods with lambda-lists having many keyword arguments
  - Declare forms spanning several lines
  - FLET and LABELS with long function names
  - Constant strings (docstrings, format control strings) spanning multiple lines
  - CONCAT, COERCE statements
  - Type definitions (DEFTYPE, DEFSTRUCT)
  - Forms guarded by reader features (`#+`, `#-`)
  - Simple, straightforward forms (defun, let, cond, loop, etc.)

### S2: REFORMAT-FILE-WITH-EMACS function
**In order to** mechanically compare Emacs formatting with Atelier
formatting, **a** maintainer **can** call a Lisp function that
reformats a file using Emacs as an external process.
**Acceptance criteria:**
- Given `(reformat-file-with-emacs pathname)`, when called,
  then Emacs opens the file in batch mode, reformats it, and writes it back.
- Given a parameter `*emacs-pretty-printer-configuration*` holding an
  Elisp s-expression, when `reformat-file-with-emacs` is called,
  then that expression is `eval`'d before reformatting.
- Given an explicit `:elisp-setup` keyword argument,
  when provided, then it overrides `*emacs-pretty-printer-configuration*`.

### S3: Fixture is a pretty-printer fixed point
**In order to** establish baseline formatting, **a** maintainer **can**
verify the fixture is stable under Atelier's pretty-printer.
**Acceptance criteria:**
- Given `reference.lisp`, when `pretty-print-form` is applied to each
  toplevel form (read then print), then the output equals the input.

### S4: Emacs indentation research and configuration
**In order to** understand how to align Emacs and Atelier, **a**
maintainer **can** read a reference document explaining SLIME/SLY
indentation and the configuration needed.
**Acceptance criteria:**
- Given online research into SLIME/SLY indentation, when divergences
  between Atelier and Emacs default indentation are identified,
  then each divergence is documented with: form, Atelier output,
  Emacs output, and proposed resolution (adjust Atelier / adjust Emacs / accept).

### S5: Manual fixture adjustment and convergence
**In order to** achieve formatting agreement, **a** maintainer **can**
adjust the fixture, the Atelier pretty-printer, and the Emacs
configuration until the fixture is a fixed point for both.
**Acceptance criteria:**
- Given the adjusted fixture, when reformatted by Atelier's
  pretty-printer, then the output equals the input.
- Given the adjusted fixture, when reformatted by Emacs with the
  documented configuration, then the output equals the input.
- Given the Emacs configuration, when side-effects on other forms
  are identified, then they are either acceptable or the change is
  renegotiated.
- Given the Emacs configuration, when measured in lines of Elisp,
  then it is at most 10 lines, stretching to 25 if needed. If a
  formatting decision requires complex Elisp beyond this budget,
  the Atelier pretty-printer adapts instead, or the decision is
  renegotiated.

### S6: Documented Emacs configuration
**In order to** adopt Atelier-compatible formatting, **a** user **can**
add the provided configuration to their Emacs setup.
**Acceptance criteria:**
- Given a `.dir-locals.el` or Emacs snippet, when added to a project,
  then Emacs indentation matches Atelier's output for all forms in
  the fixture.
- Given the documentation, when a user reads it, then they understand
  which forms diverge from Emacs defaults and why.

## Quality Criteria
- [ ] The reference fixture covers at least 15 distinct Common Lisp
  form types (defun, defmethod, defmacro, defclass, defgeneric,
  defvar, defparameter, deftype, defstruct, let/let*, flet, labels,
  loop, cond, case, when/unless, lambda, eval-when, in-package,
  defpackage, declare, handler-case/handler-bind, restart-case, etc.)
- [ ] `reformat-file-with-emacs` works with both `emacs` and `emacs-nw`
  (batch mode — no display needed)
- [ ] No existing autofix-cycle or pretty-printer fixture breaks

## Definition of Ready
- [x] Stories traceable to backlog items
- [x] Stories sized ≤ 2 days each
- [x] Acceptance criteria written
- [x] Quality criterion defined
- [x] Spec references identified

## Definition of Done
- [ ] All stories complete with acceptance criteria passing
- [ ] Quality criteria passing
- [ ] Full test suite passes
- [ ] All implementation phases have completion notes
- [ ] `product/slice/010-pretty-printer-emacs-compatibility/retrospective.md` created
- [ ] `product/backlog.md` updated
- [ ] `product/roadmap.md` updated
