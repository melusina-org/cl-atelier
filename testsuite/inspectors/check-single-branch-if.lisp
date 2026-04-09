;;;; check-single-branch-if.lisp — Tests for the IF to WHEN/UNLESS inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)

(define-testcase validate-check-single-branch-if-registered ()
  "Verify that CHECK-SINGLE-BRANCH-IF is registered in the inspector registry."
  (assert-t (not (null (member 'atelier:check-single-branch-if
                               (atelier:list-inspectors))))))

(define-testcase validate-check-single-branch-if-when ()
  "Verify that (IF TEST THEN NIL) is flagged with a WHEN suggestion."
  (validate-one-inspector-fixture 'atelier:check-single-branch-if "when"))

(define-testcase validate-check-single-branch-if-unless ()
  "Verify that (IF TEST NIL ELSE) is flagged with an UNLESS suggestion."
  (validate-one-inspector-fixture 'atelier:check-single-branch-if "unless"))

(define-testcase validate-check-single-branch-if-implicit-nil ()
  "Verify that (IF TEST THEN) with no else clause is flagged."
  (validate-one-inspector-fixture 'atelier:check-single-branch-if "implicit-nil"))

(define-testcase validate-check-single-branch-if-clean ()
  "Verify that (IF TEST THEN ELSE) with both branches non-NIL produces no findings."
  (validate-one-inspector-fixture 'atelier:check-single-branch-if "clean"))

(define-testcase testsuite-check-single-branch-if ()
  (validate-check-single-branch-if-registered)
  (validate-check-single-branch-if-when)
  (validate-check-single-branch-if-unless)
  (validate-check-single-branch-if-implicit-nil)
  (validate-check-single-branch-if-clean))

;;;; End of file `check-single-branch-if.lisp'
