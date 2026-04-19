Rework needed after implementation 1

In the testsuite `test/src/finding.lisp`:


Identifiers:

Tests use a lot of one and two-letters identifiers, this goes again
the style defined by the `common-lisp` skill. Be sure to use the skill
when you program in Lisp.


For instance `ff` in `test-finding-construction` should be
`file-finding`, `:inspector :i` should be `:inspector :inspector-placeholder` 
and `:observation "o"` should be `:observation "Observation Placeholder"` or something.

Be sure to fix that in every file you touched or created. Be sure to
use the skill `common-lisp` to write common lisp coder.

---

Also the `test-finding-construction` is useless as it stands. It tests
the standard `make-instance` which does not need to be, it is not code
that we wrote.

Instead we should define a custom constructor, as stated by the
`common-lisp` skill, and test that constructor instead of testing
`make-instance`.

Be sure to fix that issue and similar issues in the code you wrote.

---

define-testcase test-inspector-metadata

  Multi-language inspectors are an hallucination, remove that idea
  from the code and document that.
  
---

src/maintainer.lisp

* reacts-to should be `resolves`
* `prepare-fn` should not exist, just use `defmethod` to define the
  behaviour.
  

