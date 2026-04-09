;;;; check-single-form-progn.lisp — Tests for the single-form PROGN inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)

(define-testcase validate-check-single-form-progn-registered ()
  "Verify that CHECK-SINGLE-FORM-PROGN is registered in the inspector registry."
  (assert-t (not (null (member 'atelier:check-single-form-progn
                               (atelier:list-inspectors))))))

(define-testcase validate-check-single-form-progn-baseline ()
  "Verify that (PROGN F) with a single body form is flagged."
  (validate-one-inspector-fixture 'atelier:check-single-form-progn "baseline"))

(define-testcase validate-check-single-form-progn-multi-body ()
  "Verify that (PROGN A B) with multiple body forms is not flagged."
  (validate-one-inspector-fixture 'atelier:check-single-form-progn "clean"))

(define-testcase validate-check-single-form-progn-empty ()
  "Verify that (PROGN) with no body forms is not flagged."
  (validate-one-inspector-fixture 'atelier:check-single-form-progn "good"))

(define-testcase testsuite-check-single-form-progn ()
  (validate-check-single-form-progn-registered)
  (validate-check-single-form-progn-baseline)
  (validate-check-single-form-progn-multi-body)
  (validate-check-single-form-progn-empty))

;;;; End of file `check-single-form-progn.lisp'
