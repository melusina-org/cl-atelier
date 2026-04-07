# Reference: The Common Lisp Pretty-Printer

This reference covers the pretty-printer as used for code emission in Atelier.
Focus is on explicit pretty-printing functions (`pprint-logical-block`,
`pprint-newline`, `pprint-indent`). Format control strings are shown for
cross-reference only.

## Table of Contents

1. [The Core Idea](#1-the-core-idea)
2. [Logical Blocks](#2-logical-blocks)
3. [Conditional Newlines](#3-conditional-newlines)
4. [Sections](#4-sections)
5. [Indentation](#5-indentation)
6. [Iterating Over Lists](#6-iterating-over-lists)
7. [Worked Examples](#7-worked-examples)
8. [Dispatch Tables](#8-dispatch-tables)
9. [Variables That Control Layout](#9-variables-that-control-layout)
10. [Format Directive Equivalents](#10-format-directive-equivalents)

---

## 1. The Core Idea

The pretty-printer answers one question: **does this chunk of output fit on
the remainder of the current line?** If yes, print it flat. If no, insert
line breaks at positions you have marked in advance.

You express layout intent by:
- Wrapping output in **logical blocks** (the chunks),
- Placing **conditional newlines** where line breaks may appear,
- Setting **indentation** for continuation lines.

The pretty-printer makes all line-breaking decisions. You never compute
column positions or count characters yourself.

---

## 2. Logical Blocks

A logical block is a region of output the pretty-printer treats as a unit.

```lisp
(pprint-logical-block (stream list &key prefix per-line-prefix suffix)
  body)
```

- `stream` — a stream variable. Inside the body, this variable is rebound
  to a pretty-printing stream that intercepts output for layout decisions.
- `list` — the list being printed. If not a list, it is printed with `write`
  and the body is skipped. This handles atoms, `*print-level*` truncation
  (`#`), and `*print-circle*` references (`#1#`) automatically.
- `:prefix` — printed once before the block. Typically `"("`.
- `:suffix` — printed once after the block. Typically `")"`.
- `:per-line-prefix` — printed before the block AND at the start of every
  continuation line. Mutually exclusive with `:prefix`.

**Example — printing a simple list with parens:**

```lisp
(defun print-list (stream list)
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (loop
      (write (pprint-pop) :stream stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (pprint-newline :fill stream))))
```

Output of `(print-list *standard-output* '(a b c d e f g h))`:
```
;; If it fits:
(a b c d e f g h)

;; If it doesn't fit on one line, :fill breaks where needed:
(a b c d e
 f g h)
```

Logical blocks nest. An inner block is a single unit from the outer block's
perspective — the outer block does not look inside when deciding whether a
section fits.

---

## 3. Conditional Newlines

A conditional newline marks a position where a line break *may* be inserted.
The pretty-printer decides at print time whether to break there, based on the
available space.

```lisp
(pprint-newline kind &optional stream)
```

There are four kinds:

### `:linear` — all or nothing

Break here if the **containing section** does not fit on the current line.
All `:linear` newlines in the same logical block fire together. Use this
when you want the block to be either fully flat or fully broken.

**When to use:** Function bodies, `let` bindings — places where partial
breaking looks wrong.

### `:fill` — break only where needed

Break here if the **next section** will not fit on the current line, or if the
**previous section** already broke. Each `:fill` newline decides independently.
Use this when you want to pack as many items per line as possible.

**When to use:** Argument lists, data lists, import lists — places where
partial lines look natural.

### `:mandatory` — always break

Always insert a newline here. Forces all `:linear` newlines in containing
sections to fire too (the mandatory break proves the section does not fit).

**When to use:** Between top-level clauses, after docstrings — places where
you always want a new line regardless of available space.

### `:miser` — break only when space is tight

Same as `:linear`, but only fires when **miser mode** is active (the block
starts within `*print-miser-width*` columns of the right margin). Use this
for graceful degradation in deeply nested code.

**When to use:** After the operator in a function call — break `(defun` from
its name only when deeply indented.

### The decision in plain English

When the pretty-printer encounters a conditional newline:

1. It looks at the relevant **section** (the text between this newline and the
   next one at the same nesting level).
2. It measures whether that section fits in the remaining columns.
3. Based on the kind, it decides:

| Kind | Fires when... |
|------|---------------|
| `:linear` | The containing section does not fit. All `:linear` in the block fire together. |
| `:fill` | The next section does not fit, or the previous section broke. Decides independently. |
| `:mandatory` | Always. |
| `:miser` | The containing section does not fit AND miser mode is active. |

When a conditional newline fires, any spaces immediately before it are
suppressed, and the next line starts at the current indentation level.

---

## 4. Sections

A **section** is the text between two adjacent conditional newlines at the
same nesting level within a logical block. The pretty-printer tries to fit
each section on a single line.

Consider this output with three `:fill` newlines (marked `|`):

```
(alpha | beta | gamma | delta)
```

This creates four sections:
- Section 1: `(alpha `
- Section 2: `beta `
- Section 3: `gamma `
- Section 4: `delta)`

The **immediately containing section** of a conditional newline is the
smallest section that encloses it. For a `:linear` newline, the decision
is based on this containing section. For a `:fill` newline, the decision
looks at the next section specifically.

**Sections and nested blocks:** A nested logical block is opaque to the
outer block's section calculation. The outer block sees the inner block as
a single unit of output, regardless of how many newlines the inner block
contains.

---

## 5. Indentation

```lisp
(pprint-indent relative-to n &optional stream)
```

Sets the indentation for subsequent continuation lines (takes effect after
the next line break, not immediately).

Two modes:

- **`:block`** — indent `n` columns from the start of the logical block.
- **`:current`** — indent `n` columns from the current output position.

### `:block` indentation — the common case

```lisp
(pprint-indent :block 2 stream)
```

If the logical block starts at column 5 and `n` is 2, continuation lines
start at column 7.

**When to use:** Body forms of `defun`, `let`, `flet` — the body is indented
a fixed amount from the opening paren, regardless of where the line breaks.

### `:current` indentation — align to a landmark

```lisp
(pprint-indent :current 0 stream)
```

Continuation lines start at whatever column the output is currently at.

**When to use:** After printing a function name, to align arguments under
each other.

### Indentation and miser mode

In miser mode, all `pprint-indent` calls are ignored. Continuation lines
align under the first character of the logical block. This prevents deeply
nested code from being pushed too far right.

---

## 6. Iterating Over Lists

Inside `pprint-logical-block`, two local macros handle list traversal:

**`pprint-pop`** — returns the next element from the list. Before returning,
it checks:
- Is the remainder a proper list? If not, prints `. rest)` and terminates.
- Has `*print-length*` been exceeded? If so, prints `...` and terminates.
- Is the remainder circular? If so, prints the circularity marker.

**`pprint-exit-if-list-exhausted`** — if the list is empty, terminates the
logical block (the suffix still prints). Otherwise returns `nil`.

**Canonical iteration pattern:**

```lisp
(pprint-logical-block (stream list :prefix "(" :suffix ")")
  (loop
    (write (pprint-pop) :stream stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\Space stream)
    (pprint-newline :fill stream)))
```

This pattern:
1. Writes one element.
2. Checks if the list is done (exits if so — suffix `)` is printed).
3. Writes a space.
4. Places a conditional newline (where a line break may go).
5. Repeats.

The result is a list whose elements are packed onto lines with `:fill`
breaks. Dotted lists, circularity, and `*print-length*` are handled
automatically by `pprint-pop`.

---

## 7. Worked Examples

Each example follows the pattern: **what layout I want** → **how I use
pretty-printer concepts to achieve it**.

### 7.1 Printing `defun`

**Desired layout:**

```lisp
;; When it fits on one line:
(defun compute-discount (price rate) (* price (- 1.0 rate)))

;; When the body doesn't fit:
(defun compute-discount (price rate)
  (* price (- 1.0 rate)))

;; When even the lambda list doesn't fit:
(defun compute-discount
    (price rate)
  (* price (- 1.0 rate)))
```

**Reasoning:**

- The operator `defun`, the name, and the lambda list should stay on one line
  when possible. Use `:fill` between them — break only where needed.
- The body must start on a new line if the whole form does not fit. Use
  `:linear` before the body — it fires when the containing section cannot
  fit, breaking all at once.
- The body is indented 2 from the block start (`:block 2`).
- The lambda list, if broken onto its own line, is indented 4 from the block
  start (`:block 4`) to distinguish it visually from the body.

```lisp
(defun pprint-defun (stream list)
  "Pretty-print a DEFUN form."
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    ;; DEFUN
    (write (pprint-pop) :stream stream)
    (write-char #\Space stream)
    (pprint-newline :miser stream)
    ;; Function name
    (write (pprint-pop) :stream stream)
    (write-char #\Space stream)
    ;; Lambda list — align under name if it breaks
    (pprint-indent :block 4 stream)
    (pprint-newline :fill stream)
    (write (pprint-pop) :stream stream)
    ;; Body — indented 2 from block start
    (pprint-indent :block 2 stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\Space stream)
    (pprint-newline :linear stream)
    ;; Remaining body forms, each on its own line
    (loop
      (write (pprint-pop) :stream stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (pprint-newline :linear stream))))
```

**Why `:miser` after `defun`?** We only want to separate `defun` from its
name when the form is deeply indented (miser mode). Normally they stay
together.

**Why `:fill` before the lambda list?** The lambda list moves to a new line
only if it does not fit after the name. The body stays on the same decision
independently.

**Why `:linear` before the body?** Once the body breaks, ALL body forms
break — we never want some body forms on the same line as the lambda list
and others on new lines.

**Format equivalent (for reference only):**
```lisp
(defun pprint-defun-fmt (stream list)
  (funcall (formatter "~:<~W~^ ~@_~W~^ ~4I~:_~W~^~2I ~_~@{~W~^ ~_~}~:>")
           stream list))
```

### 7.2 Printing `let`

**Desired layout:**

```lisp
;; Fits on one line:
(let ((x 1) (y 2)) (+ x y))

;; Body broken:
(let ((x 1) (y 2))
  (+ x y))

;; Bindings broken individually:
(let ((x 1)
      (y 2))
  (+ x y))
```

**Reasoning:**

- The binding list is a nested logical block with `:fill` newlines — bindings
  pack onto lines, each binding staying intact.
- The body uses `:linear` — either all on one line or all broken.
- Body indentation is `:block 2`.
- Within the binding list, bindings align under the first binding
  (`:current 0` after the opening paren).

```lisp
(defun pprint-let (stream list)
  "Pretty-print a LET form."
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    ;; LET
    (write (pprint-pop) :stream stream)
    (write-char #\Space stream)
    ;; Binding list — nested logical block
    (pprint-let-bindings stream (pprint-pop))
    ;; Body
    (pprint-indent :block 2 stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\Space stream)
    (pprint-newline :linear stream)
    (loop
      (write (pprint-pop) :stream stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (pprint-newline :linear stream))))

(defun pprint-let-bindings (stream bindings)
  "Pretty-print a LET binding list."
  (pprint-logical-block (stream bindings :prefix "(" :suffix ")")
    (loop
      ;; Each binding is itself a list — write it as a unit
      (write (pprint-pop) :stream stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (pprint-newline :fill stream))))
```

**Why `:fill` in the bindings?** We want `(x 1) (y 2)` on one line when
they fit, but each binding on its own line when they don't. `:fill` packs
them per-line.

**Why `:linear` for the body?** We never want the first body form on the
same line as the bindings and the second on a new line.

**Format equivalent:**
```lisp
;; ~:<~W~^ ~:<~@{~W~^ ~:_~}~:>~^~2I ~_~@{~W~^ ~_~}~:>
```

### 7.3 Printing `flet`

**Desired layout:**

```lisp
;; Fits:
(flet ((compute-total (x) (+ x tax))) (mapcar #'compute-total prices))

;; Body broken:
(flet ((compute-total (x) (+ x tax)))
  (mapcar #'compute-total prices))

;; Function binding broken:
(flet ((compute-total (x)
         (+ x tax)))
  (mapcar #'compute-total prices))
```

**Reasoning:**

- Same outer structure as `let`: operator, binding list, body.
- Each FLET binding is itself like a `defun`: name, lambda list, body.
  It gets its own nested logical block with the same break strategy as
  `defun`: `:fill` before the lambda list, `:linear` before the body.
- Within a binding, body indentation is `:block 2` relative to the
  binding's opening paren (standard body indent). The lambda list is
  naturally placed right after the name.

```lisp
(defun pprint-flet (stream list)
  "Pretty-print a FLET or LABELS form."
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    ;; FLET
    (write (pprint-pop) :stream stream)
    (write-char #\Space stream)
    ;; Binding list
    (pprint-flet-bindings stream (pprint-pop))
    ;; Body
    (pprint-indent :block 2 stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\Space stream)
    (pprint-newline :linear stream)
    (loop
      (write (pprint-pop) :stream stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (pprint-newline :linear stream))))

(defun pprint-flet-bindings (stream bindings)
  "Pretty-print the binding list of a FLET form."
  (pprint-logical-block (stream bindings :prefix "(" :suffix ")")
    (loop
      ;; Each binding: (name lambda-list body...)
      (pprint-flet-one-binding stream (pprint-pop))
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (pprint-newline :linear stream))))

(defun pprint-flet-one-binding (stream binding)
  "Pretty-print a single FLET binding: (name lambda-list body...)."
  (pprint-logical-block (stream binding :prefix "(" :suffix ")")
    ;; Name
    (write (pprint-pop) :stream stream)
    (write-char #\Space stream)
    ;; Lambda list
    (write (pprint-pop) :stream stream)
    ;; Body
    (pprint-indent :block 2 stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\Space stream)
    (pprint-newline :linear stream)
    (loop
      (write (pprint-pop) :stream stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (pprint-newline :linear stream))))
```

**Why `:linear` between FLET bindings?** Multiple function bindings should
each be on their own line when they don't all fit — partial packing of
function definitions would look messy.

### 7.4 Printing `loop`

**Desired layout:**

```lisp
;; Fits:
(loop :for x :in items :collect (transform-item x))

;; Broken — each clause on its own line, sub-keywords indented:
(loop :for x :in items
      :when (valid-item-p x)
        :collect (transform-item x))
```

**Reasoning:**

- `loop` forms have a flat body (no nesting of clauses), but clause keywords
  serve as natural break points.
- Place `:linear` newlines before each clause keyword — when the loop breaks,
  all clauses break.
- Main clause keywords (`:for`, `:when`, `:unless`, `:with`, `:initially`,
  `:finally`, `:repeat`) align under each other at the default block indent.
- Sub-clause keywords (`:collect`, `:do`, `:return`, `:sum`, etc.) are
  indented 2 extra spaces from the main clause level to show subordination.
- Indentation is `:block N` where N depends on the keyword.

```lisp
(defvar *loop-main-clause-keywords*
  '(:for :with :when :unless :while :until :initially :finally :repeat
    :named)
  "LOOP clause keywords that start a new clause at the base indentation.")

(defvar *loop-sub-clause-keywords*
  '(:do :collect :append :nconc :count :sum :maximize :minimize :return
    :in :across)
  "LOOP clause keywords that continue a clause, indented under their parent.")

(defun pprint-loop (stream list)
  "Pretty-print a LOOP form with keyword clause symbols."
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    ;; LOOP
    (write (pprint-pop) :stream stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\Space stream)
    ;; Clause keywords align under the first token after LOOP
    (pprint-indent :current 0 stream)
    (loop
      (let ((token (pprint-pop)))
        (cond
          ((member token *loop-main-clause-keywords*)
           (pprint-newline :linear stream)
           (pprint-indent :current 0 stream)
           (write token :stream stream))
          ((member token *loop-sub-clause-keywords*)
           (pprint-newline :linear stream)
           (pprint-indent :current 0 stream)
           (write-string "  " stream)
           (write token :stream stream))
          (t
           (write token :stream stream))))
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream))))
```

**Why `:current 0` after `LOOP`?** This sets continuation lines to align
at the column just after `(loop `. All clause keywords line up vertically.

**Why `:linear` before each keyword?** When the loop form breaks, every
clause should break — a partially-broken loop is hard to read.

**Note:** This is a simplified version. A production implementation would
need to handle `loop-finish`, `named`, and distinguish between the various
grammatical roles more precisely. The YAML fixture tests will verify the
exact output.

### 7.5 Printing a function call

**Desired layout:**

```lisp
;; Fits:
(compute-discount price rate tax)

;; Arguments broken — aligned under first argument:
(compute-discount price
                  rate
                  tax)
```

**Reasoning:**

- Print the operator, then a space, then set `:current 0` so that arguments
  align under the first argument.
- Use `:fill` between arguments — pack what fits, break the rest.

```lisp
(defun pprint-function-call (stream list)
  "Pretty-print a function call form."
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    ;; Operator
    (write (pprint-pop) :stream stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\Space stream)
    ;; Arguments align under the first argument
    (pprint-indent :current 0 stream)
    (loop
      (write (pprint-pop) :stream stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (pprint-newline :fill stream))))
```

**Why `:fill` for arguments?** Unlike `defun` bodies, function call
arguments are independent values that look fine packed onto lines. `:fill`
packs them; `:linear` would waste vertical space.

**Format equivalent:**
```lisp
;; ~:<~W~^ ~:I~@{~W~^ ~:_~}~:>
```

---

## 8. Dispatch Tables

### Creating an isolated table

```lisp
(defvar *atelier-pprint-dispatch*
  (let ((table (copy-pprint-dispatch nil)))
    ;; Register custom printers here
    table)
  "Atelier's pprint dispatch table. Never modifies the global table.")
```

`(copy-pprint-dispatch nil)` copies the **initial** (implementation-default)
table, which already has sensible printers for standard types.

### Registering a custom printer

```lisp
(set-pprint-dispatch '(cons (member defun defmacro))
                     #'pprint-defun
                     0           ; priority
                     *atelier-pprint-dispatch*)
```

- Type specifier `(cons (member defun defmacro))` matches any cons whose
  `car` is `defun` or `defmacro`.
- Priority `0` is the default. Higher priority wins when an object matches
  multiple entries. All user-specified priorities beat the initial table's
  built-in entries.
- The function receives `(stream object)`.

### Using the table

```lisp
(defun pretty-print-form (form column)
  "Pretty-print FORM as a string, indented to COLUMN."
  (let ((*print-pprint-dispatch* *atelier-pprint-dispatch*)
        (*print-pretty* t)
        (*print-case* :downcase)
        (*print-right-margin* (max 40 (- 100 column))))
    ;; Print to string, then indent continuation lines
    ...))
```

The global `*print-pprint-dispatch*` is never modified. The binding has
dynamic extent — only code within this `let` sees the custom table.

### Reusing an existing printer for a custom macro

If your macro `define-inspector` has the same structure as `defmacro`:

```lisp
(set-pprint-dispatch '(cons (member define-inspector))
                     (pprint-dispatch '(defmacro) *atelier-pprint-dispatch*)
                     0
                     *atelier-pprint-dispatch*)
```

`pprint-dispatch` looks up the printer for a given form — here we retrieve
the printer for `defmacro` forms and reuse it.

---

## 9. Variables That Control Layout

| Variable | Type | Effect |
|----------|------|--------|
| `*print-pretty*` | boolean | Enables pretty-printing. When nil, all conditional newlines and indentation are ignored. |
| `*print-right-margin*` | integer or nil | The column limit for line-breaking decisions. nil = implementation default (typically 72–80). |
| `*print-miser-width*` | integer or nil | When non-nil, miser mode activates for a block whose starting column is within this many columns of the right margin. In miser mode: `:miser` fires like `:linear`, `:fill` fires like `:linear`, `pprint-indent` is ignored. |
| `*print-lines*` | integer or nil | Maximum number of output lines. Excess lines are replaced with `..` followed by closing delimiters. |
| `*print-level*` | integer or nil | Maximum nesting depth. Structures beyond this depth are printed as `#`. Handled automatically by `pprint-logical-block`. |
| `*print-length*` | integer or nil | Maximum number of elements per list. Excess elements are printed as `...`. Handled automatically by `pprint-pop`. |
| `*print-case*` | keyword | `:downcase`, `:upcase`, or `:capitalize`. Controls symbol printing. |

**For Atelier's code writer, the typical binding is:**

```lisp
(*print-pretty* t)
(*print-case* :downcase)
(*print-right-margin* (- 100 column))
(*print-miser-width* 20)
```

---

## 10. Format Directive Equivalents

For reference only — Atelier uses explicit functions, not format strings.

| Function call | Format directive |
|--------------|-----------------|
| `(pprint-newline :linear s)` | `~_` |
| `(pprint-newline :fill s)` | `~:_` |
| `(pprint-newline :miser s)` | `~@_` |
| `(pprint-newline :mandatory s)` | `~:@_` |
| `(pprint-indent :block n s)` | `~nI` |
| `(pprint-indent :current n s)` | `~n:I` |
| `(pprint-logical-block (s list :prefix "(" :suffix ")") ...)` | `~:<...~:>` |
| `(pprint-exit-if-list-exhausted)` | `~^` |
| `(write (pprint-pop) :stream s)` | `~W` |

The format directive `~<...~:>` with a `:` before `>` is the logical block
form. Without `:`, `~<...~>` is justification (a different feature — do not
confuse them).

---

## Summary: Choosing the Right Newline Kind

| I want... | Use... |
|-----------|--------|
| All elements on one line, or each on its own line | `:linear` |
| Pack elements onto lines, break only where needed | `:fill` |
| Always start a new line here | `:mandatory` |
| Break only when deeply nested | `:miser` |

## Summary: Choosing the Right Indentation

| I want continuation lines... | Use... |
|------------------------------|--------|
| Indented N from the opening paren | `(pprint-indent :block N)` |
| Aligned under the current column | `(pprint-indent :current 0)` |
| Indented N from the current column | `(pprint-indent :current N)` |
