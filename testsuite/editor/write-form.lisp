;;;; write-form.lisp — Tests for write-toplevel-form-to-string

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/editor)

(define-testcase validate-write-simple-defun ()
  "Verify write-toplevel-form-to-string on a simple defun."
  (let* ((form (atelier/editor:read-toplevel-form-from-string
                 "(defun foo (x) (1+ x))"))
         (text (atelier/editor:write-toplevel-form-to-string form)))
    (assert-string= "(defun foo (x) (1+ x))" text)))

(define-testcase validate-write-preserves-feature-conditional ()
  "Verify that #+/#- inside the body is preserved in the output."
  (let* ((input "(defun connect () #+sbcl :yes #-sbcl :no)")
         (form (atelier/editor:read-toplevel-form-from-string input))
         (text (atelier/editor:write-toplevel-form-to-string form)))
    ;; The matching branch should be pretty-printed
    (assert-t* (search ":yes" text :test #'char-equal))
    ;; The non-matching branch should be preserved verbatim
    (assert-t* (search "#-sbcl :no" text))))

(define-testcase validate-write-wraps-eval-when ()
  "Verify that non-default eval-when is wrapped on write."
  (let* ((form (atelier/editor:read-toplevel-form-from-string
                 "(eval-when (:compile-toplevel :load-toplevel :execute) (defun helper (x) (1+ x)))"))
         (text (atelier/editor:write-toplevel-form-to-string form)))
    (assert-t* (search "eval-when" text :test #'char-equal))
    (assert-t* (search ":compile-toplevel" text :test #'char-equal))))

(define-testcase validate-write-elides-default-eval-when ()
  "Verify that default eval-when (:load-toplevel :execute) is NOT written."
  (let* ((form (atelier/editor:read-toplevel-form-from-string
                 "(defun bar (x) x)"))
         (text (atelier/editor:write-toplevel-form-to-string form)))
    (assert-nil (search "eval-when" text :test #'char-equal))))

(define-testcase validate-round-trip-fixed-point ()
  "Verify that write(read(write(read(s)))) = write(read(s))."
  (flet ((round-trip (input)
           (let* ((form1 (atelier/editor:read-toplevel-form-from-string input))
                  (text1 (atelier/editor:write-toplevel-form-to-string form1))
                  (form2 (atelier/editor:read-toplevel-form-from-string text1))
                  (text2 (atelier/editor:write-toplevel-form-to-string form2)))
             (assert-string= text1 text2))))
    (round-trip "(defun foo (x) (1+ x))")
    (round-trip "(defvar *bar* 42)")
    (round-trip "(defclass thing () ((name :reader thing-name)))")))

;;;; End of file `write-form.lisp'
