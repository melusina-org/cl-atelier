;;;; check-when-not.lisp — Tests for the WHEN-NOT to UNLESS inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)

(define-testcase validate-check-when-not-registered ()
  "Verify that CHECK-WHEN-NOT is registered in the inspector registry."
  (assert-t (not (null (member 'atelier:check-when-not
                               (atelier:list-inspectors))))))

(define-testcase validate-check-when-not-baseline ()
  "Verify that (WHEN (NOT X) BODY) is flagged with an UNLESS suggestion."
  (validate-one-inspector-fixture 'atelier:check-when-not "baseline"))

(define-testcase validate-check-when-not-clean ()
  "Verify that (WHEN X BODY) with a positive test is not flagged."
  (validate-one-inspector-fixture 'atelier:check-when-not "clean"))

(define-testcase testsuite-check-when-not ()
  (validate-check-when-not-registered)
  (validate-check-when-not-baseline)
  (validate-check-when-not-clean))

;;;; End of file `check-when-not.lisp'
