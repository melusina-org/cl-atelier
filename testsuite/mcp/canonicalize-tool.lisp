;;;; canonicalize-tool.lisp — Tests for the canonicalize-form MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

;;; These tests verify the canonicalize-form tool runs in-process
;;; (no child SBCL) and produces correct results.

(defun %invoke-canonicalize-tool (form-string)
  "Invoke the canonicalize-form tool with FORM-STRING and return the result."
  (let ((tool (atelier/mcp:find-tool-by-name "atelier:canonicalize-form")))
    (assert-t* tool)
    (atelier/mcp:handle-tool-call tool
                                  (list (cons "form" form-string)))))

(define-testcase validate-canonicalize-tool-basic ()
  "Verify that canonicalize-form tool produces a canonicalized defun."
  (let ((result (%invoke-canonicalize-tool "(defun foo(x)(1+ x))")))
    ;; Result should be an alist with "canonicalized" key
    (assert-t* (assoc "canonicalized" result :test #'string=))
    ;; The canonicalized form should be a non-empty string
    (let ((canonical (cdr (assoc "canonicalized" result :test #'string=))))
      (assert-t* (stringp canonical))
      (assert-t* (plusp (length canonical)))
      ;; Should contain "defun" and "foo"
      (assert-t* (search "defun" canonical :test #'char-equal))
      (assert-t* (search "foo" canonical :test #'char-equal)))))

(define-testcase validate-canonicalize-tool-earmuffs ()
  "Verify that canonicalize-form applies fix-earmuffs on defvar."
  (let* ((result (%invoke-canonicalize-tool "(defvar foo 42)"))
         (canonical (cdr (assoc "canonicalized" result :test #'string=))))
    ;; Should have earmuffs applied
    (assert-t* (search "*foo*" canonical :test #'char-equal))))

(define-testcase validate-canonicalize-tool-forbidden ()
  "Verify that canonicalize-form signals error on forbidden toplevel form."
  (assert-condition
   (%invoke-canonicalize-tool "(format t \"hello\")")
   error))

(define-testcase validate-canonicalize-tool-progn-decompose ()
  "Verify that canonicalize-form signals error on toplevel progn."
  (assert-condition
   (%invoke-canonicalize-tool "(progn (defun foo () 1) (defun bar () 2))")
   error))

(define-testcase validate-canonicalize-tool-no-child ()
  "Verify that canonicalize-form runs without *current-server*
   (i.e., it does not attempt to create a child connection)."
  (let ((atelier/mcp::*current-server* nil))
    ;; Should succeed — canonicalize-form doesn't use the child
    (let ((result (%invoke-canonicalize-tool "(defun foo (x) x)")))
      (assert-t* (assoc "canonicalized" result :test #'string=)))))

;;;; End of file `canonicalize-tool.lisp'
