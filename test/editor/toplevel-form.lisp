;;;; toplevel-form.lisp — Tests for the toplevel-form record

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/test/editor)


;;;;
;;;; Construction
;;;;

(define-testcase validate-make-toplevel-form ()
  "Verify that make-toplevel-form constructs a record with correct slot values."
  (let ((form (atelier/editor:make-toplevel-form
                :kind 'common-lisp:defun
                :name 'foo
                :body nil
                :eval-when '(:load-toplevel :execute))))
    (assert-t* (typep form 'atelier/editor:toplevel-form))
    (assert-equal 'common-lisp:defun (atelier/editor:toplevel-form-kind form))
    (assert-equal 'foo (atelier/editor:toplevel-form-name form))
    (assert-equal '(:load-toplevel :execute) (atelier/editor:toplevel-form-eval-when form))
    (assert-nil (atelier/editor:toplevel-form-source-text form))))

;;;; End of file `toplevel-form.lisp'
