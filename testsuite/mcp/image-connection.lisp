;;;; image-connection.lisp — Tests for the abstract image-connection class

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

(define-testcase validate-image-connection-class-exists ()
  "image-connection is a STANDARD-CLASS."
  (assert-type (find-class 'image-connection) 'standard-class))

(define-testcase validate-image-connection-generics-defined ()
  "The three generic functions are defined."
  (assert-t* (fboundp 'connection-eval))
  (assert-t* (fboundp 'connection-shutdown))
  (assert-t* (fboundp 'connection-alive-p)))

(define-testcase validate-image-connection-documentation ()
  "The class carries a docstring."
  (let ((doc (documentation (find-class 'image-connection) 't)))
    (assert-t* (stringp doc))
    (assert-t* (plusp (length doc)))))

(define-testcase validate-bare-connection-alive-is-nil ()
  "A bare instance with no process-info is not alive."
  (let ((c (make-instance 'image-connection :id "t")))
    (assert-nil (connection-alive-p c))))

(define-testcase validate-bare-connection-eval-signals ()
  "CONNECTION-EVAL on the bare class signals NOT-IMPLEMENTED."
  (let ((c (make-instance 'image-connection :id "t")))
    (assert-condition (connection-eval c '(+ 1 2)) not-implemented)))

(define-testcase validate-image-connection-tests ()
  (validate-image-connection-class-exists)
  (validate-image-connection-generics-defined)
  (validate-image-connection-documentation)
  (validate-bare-connection-alive-is-nil)
  (validate-bare-connection-eval-signals))

;;;; End of file `image-connection.lisp'
