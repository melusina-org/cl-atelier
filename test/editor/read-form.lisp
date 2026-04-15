;;;; read-form.lisp — Tests for read-toplevel-form-from-string

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/editor)


;;;;
;;;; Reading from Strings
;;;;

(define-testcase validate-read-simple-defun ()
  "Verify read-toplevel-form-from-string on a simple defun."
  (let ((form (atelier/editor:read-toplevel-form-from-string
                "(defun foo (x) (1+ x))")))
    (assert-equal 'common-lisp:defun (atelier/editor:toplevel-form-kind form))
    (assert-string= "FOO" (symbol-name (atelier/editor:toplevel-form-name form)))
    (assert-equal '(:load-toplevel :execute) (atelier/editor:toplevel-form-eval-when form))
    (assert-t* (atelier/editor:toplevel-form-source-text form))
    (assert-t* (atelier/editor:toplevel-form-body form))))

(define-testcase validate-read-peels-eval-when ()
  "Verify that eval-when wrappers are peeled into the eval-when slot."
  (let ((form (atelier/editor:read-toplevel-form-from-string
                "(eval-when (:compile-toplevel :load-toplevel :execute) (defun helper (x) (1+ x)))")))
    (assert-equal 'common-lisp:defun (atelier/editor:toplevel-form-kind form))
    (assert-string= "HELPER" (symbol-name (atelier/editor:toplevel-form-name form)))
    (assert-equal '(:compile-toplevel :load-toplevel :execute)
                  (atelier/editor:toplevel-form-eval-when form))))

(define-testcase validate-read-preserves-feature-conditional ()
  "Verify that #+sbcl inside a body is preserved as CST structure."
  (let* ((input "(defun connect () #+sbcl :yes #-sbcl :no)")
         (form (atelier/editor:read-toplevel-form-from-string input)))
    (assert-equal 'common-lisp:defun (atelier/editor:toplevel-form-kind form))
    (assert-string= input (atelier/editor:toplevel-form-source-text form))))

(define-testcase validate-ast-default-features ()
  "Verify toplevel-form-ast with default features evaluates #+sbcl on SBCL."
  (let* ((form (atelier/editor:read-toplevel-form-from-string
                 "(defun connect () #-inexistant-feature :yes #+inexistant-feature :no)"))
         (ast (atelier/editor:toplevel-form-ast form)))
    (assert-t* (member :yes ast))
    (assert-nil (member :no ast))))

(define-testcase validate-ast-overridden-features ()
  "Verify toplevel-form-ast with overridden features."
  (let* ((form (atelier/editor:read-toplevel-form-from-string
                 "(defun foo () #+sbcl :sbcl-val)"))
         (ast (atelier/editor:toplevel-form-ast form :features '(:ccl))))
    (assert-t* (consp ast))))

(define-testcase validate-read-unknown-macro-name ()
  "Verify that unknown macros derive name from position 1."
  (let ((form (atelier/editor:read-toplevel-form-from-string
                "(alexandria:define-constant +max-retries+ 3)")))
    (assert-equal 'alexandria:define-constant (atelier/editor:toplevel-form-kind form))
    (assert-string= "+MAX-RETRIES+" (symbol-name (atelier/editor:toplevel-form-name form)))))

(define-testcase validate-read-progn-signals ()
  "Verify that a toplevel progn signals unexpected-toplevel-form with :progn reason."
  (assert-condition
    (atelier/editor:read-toplevel-form-from-string
      "(progn (defvar *a* 1) (defun b () *a*))")
    atelier/editor:unexpected-toplevel-form))

(define-testcase validate-read-progn-decompose-restart ()
  "Verify that the decompose restart splits a progn into individual forms."
  (let ((forms
          (handler-bind
              ((atelier/editor:unexpected-toplevel-form
                 (lambda (c)
                   (when (eq (atelier/editor:unexpected-toplevel-form-reason c) :progn)
                     (invoke-restart 'atelier/editor:decompose)))))
            (atelier/editor:read-toplevel-form-from-string
              "(progn (defvar *a* 1) (defun b () *a*))"))))
    (assert-equal 2 (length forms))
    (assert-equal 'common-lisp:defvar (atelier/editor:toplevel-form-kind (first forms)))
    (assert-equal 'common-lisp:defun (atelier/editor:toplevel-form-kind (second forms)))))

(define-testcase validate-read-side-effect-signals ()
  "Verify that a side-effectful toplevel form signals with :side-effect reason."
  (handler-case
      (progn
        (atelier/editor:read-toplevel-form-from-string "(format t \"hello\")")
        (assert-t nil))
    (atelier/editor:unexpected-toplevel-form (c)
      (assert-equal :side-effect (atelier/editor:unexpected-toplevel-form-reason c)))))

;;;; End of file `read-form.lisp'
