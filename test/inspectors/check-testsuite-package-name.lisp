;;;; check-testsuite-package-name.lisp — Tests for the /testsuite package-name inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/test)


;;;;
;;;; Helpers
;;;;

(defun inspect-testsuite-line (line)
  "Return the list of findings produced by CHECK-TESTSUITE-PACKAGE-NAME on LINE."
  (let ((inspector (atelier:find-inspector 'atelier:check-testsuite-package-name)))
    (atelier:inspect-line inspector line 1)))

(defun lint-testsuite-string (content)
  "Return the fixed string produced by lint-string on CONTENT with line-level
inspection only. Equivalent to running CHECK-TESTSUITE-PACKAGE-NAME and its
maintainer in a single pass."
  (nth-value 0 (atelier:lint-string content :levels (list :line))))


;;;;
;;;; Detection
;;;;

(define-testcase validate-check-testsuite-detects-defpackage ()
  "Flag a DEFPACKAGE form whose name ends with /testsuite."
  (let* ((line "(defpackage #:foo/testsuite")
         (findings (inspect-testsuite-line line)))
    (assert-eq 1 (length findings))
    (assert-t (typep (first findings) 'atelier:testsuite-package-name-finding))
    (assert-eq 17 (atelier:finding-column (first findings)))
    (assert-eq 27 (atelier:finding-end-column (first findings)))))

(define-testcase validate-check-testsuite-detects-in-package ()
  "Flag an IN-PACKAGE form whose name ends with /testsuite."
  (let ((findings (inspect-testsuite-line "(in-package #:foo/testsuite)")))
    (assert-eq 1 (length findings))
    (assert-string= "/testsuite"
                    (subseq "(in-package #:foo/testsuite)"
                            (atelier:finding-column (first findings))
                            (atelier:finding-end-column (first findings))))))

(define-testcase validate-check-testsuite-detects-qualified-symbol ()
  "Flag a qualified symbol whose package ends with /testsuite."
  (let ((findings (inspect-testsuite-line "(foo/testsuite::bar 42)")))
    (assert-eq 1 (length findings))))

(define-testcase validate-check-testsuite-detects-find-package-string ()
  "Flag a string argument passed to FIND-PACKAGE or similar."
  (let ((findings (inspect-testsuite-line "(find-package \"FOO/TESTSUITE\")")))
    (assert-eq 1 (length findings))
    (let ((matched (subseq "(find-package \"FOO/TESTSUITE\")"
                           (atelier:finding-column (first findings))
                           (atelier:finding-end-column (first findings)))))
      (assert-string= "/TESTSUITE" matched))))

(define-testcase validate-check-testsuite-detects-nested-package ()
  "Flag a multi-segment package name whose first segment ends with /testsuite."
  (let ((findings (inspect-testsuite-line "(in-package #:foo/testsuite/bar)")))
    (assert-eq 1 (length findings))
    (assert-string= "/testsuite"
                    (subseq "(in-package #:foo/testsuite/bar)"
                            (atelier:finding-column (first findings))
                            (atelier:finding-end-column (first findings))))))


;;;;
;;;; Non-Detection
;;;;

(define-testcase validate-check-testsuite-ignores-plural-suffix ()
  "Do not flag /testsuites — only the exact /testsuite suffix is deprecated."
  (let ((findings (inspect-testsuite-line "(foo/testsuites::bar 42)")))
    (assert-t (null findings))))

(define-testcase validate-check-testsuite-ignores-bare-word ()
  "Do not flag the English word testsuite when no package-name prefix precedes it."
  (let ((findings (inspect-testsuite-line ";; test fixture in the testsuite directory")))
    (assert-t (null findings))))

(define-testcase validate-check-testsuite-ignores-orphan-slash ()
  "Do not flag /testsuite when it is not preceded by an identifier character."
  (let ((findings (inspect-testsuite-line "docs mention /testsuite in passing")))
    (assert-t (null findings))))


;;;;
;;;; Autofix Cycle
;;;;

(define-testcase validate-fix-testsuite-preserves-lower-case ()
  "Rename /testsuite to /test, preserving the lower-case original."
  (let ((fixed (lint-testsuite-string "(in-package #:foo/testsuite)")))
    (assert-string= "(in-package #:foo/test)" fixed)))

(define-testcase validate-fix-testsuite-preserves-upper-case ()
  "Rename /TESTSUITE to /TEST, preserving the upper-case original."
  (let ((fixed (lint-testsuite-string "(find-package \"FOO/TESTSUITE\")")))
    (assert-string= "(find-package \"FOO/TEST\")" fixed)))

(define-testcase validate-fix-testsuite-rewrites-multiple-occurrences ()
  "Rename every /testsuite occurrence on a single line."
  (let* ((input (concatenate 'string
                             "(atelier"
                             "/testsuite::foo (find-package :atelier"
                             "/testsuite))"))
         (expected "(atelier/test::foo (find-package :atelier/test))")
         (fixed (lint-testsuite-string input)))
    (assert-string= expected fixed)))

(define-testcase validate-fix-testsuite-preserves-nested-segment ()
  "Rename only the /testsuite segment within a multi-segment package name."
  (let* ((input (concatenate 'string "(in-package #:atelier" "/testsuite/editor)"))
         (expected "(in-package #:atelier/test/editor)")
         (fixed (lint-testsuite-string input)))
    (assert-string= expected fixed)))


;;;;
;;;; Suite
;;;;

(define-testcase testsuite-check-testsuite-package-name ()
  (validate-check-testsuite-detects-defpackage)
  (validate-check-testsuite-detects-in-package)
  (validate-check-testsuite-detects-qualified-symbol)
  (validate-check-testsuite-detects-find-package-string)
  (validate-check-testsuite-detects-nested-package)
  (validate-check-testsuite-ignores-plural-suffix)
  (validate-check-testsuite-ignores-bare-word)
  (validate-check-testsuite-ignores-orphan-slash)
  (validate-fix-testsuite-preserves-lower-case)
  (validate-fix-testsuite-preserves-upper-case)
  (validate-fix-testsuite-rewrites-multiple-occurrences)
  (validate-fix-testsuite-preserves-nested-segment))

;;;; End of file `check-testsuite-package-name.lisp'
