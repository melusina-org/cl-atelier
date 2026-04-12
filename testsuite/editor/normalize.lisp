;;;; normalize.lisp — Tests for normalize-toplevel-form

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/editor)

(define-testcase validate-normalize-earmuffs ()
  "Verify that normalize-toplevel-form applies fix-earmuffs on defvar."
  (multiple-value-bind (normalized findings)
      (atelier/editor:normalize-toplevel-form
        (atelier/editor:read-toplevel-form-from-string "(defvar foo 42)"))
    (declare (ignore findings))
    (let ((text (atelier/editor:write-toplevel-form-to-string normalized)))
      (assert-string= "(defvar *foo* 42)" text))))

(define-testcase validate-normalize-returns-findings ()
  "Verify that normalize returns findings as the second value."
  (multiple-value-bind (normalized findings)
      (atelier/editor:normalize-toplevel-form
        (atelier/editor:read-toplevel-form-from-string "(defvar foo 42)"))
    (declare (ignore normalized))
    (assert-t* findings)
    (assert-t* (>= (length findings) 1))))

(define-testcase validate-normalize-idempotent ()
  "Verify that normalizing a normalized form yields the same text."
  (let* ((form1 (atelier/editor:read-toplevel-form-from-string "(defvar foo 42)"))
         (norm1 (atelier/editor:normalize-toplevel-form form1))
         (text1 (atelier/editor:write-toplevel-form-to-string norm1))
         (norm2 (atelier/editor:normalize-toplevel-form norm1))
         (text2 (atelier/editor:write-toplevel-form-to-string norm2)))
    (assert-string= text1 text2)))

;;;; End of file `normalize.lisp'
