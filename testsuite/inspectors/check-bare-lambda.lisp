;;;; check-bare-lambda.lisp — Tests for the bare lambda inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)

(define-testcase validate-check-bare-lambda-registered ()
  "Verify that CHECK-BARE-LAMBDA is registered in the inspector registry."
  (assert-t (not (null (member 'atelier:check-bare-lambda
                               (atelier:list-inspectors))))))

(define-testcase validate-check-bare-lambda-violation ()
  "Verify that MAPCAR with a bare LAMBDA produces a BARE-LAMBDA-FINDING."
  (let ((fixture-path (inspector-fixture 'atelier:check-bare-lambda)))
    (unless (probe-file fixture-path)
      (format t "~&SKIP validate-check-bare-lambda-violation: ~A not found.~%" fixture-path)
      (return-from validate-check-bare-lambda-violation nil))
    (let* ((inspector (atelier:find-inspector 'atelier:check-bare-lambda))
           (findings (atelier:inspect-file inspector fixture-path)))
      (assert-t (not (null findings)))
      (assert-t (typep (first findings) 'atelier:bare-lambda-finding))
      (assert-eq 1 (length findings)))))

(define-testcase validate-check-bare-lambda-named-function ()
  "Verify that MAPCAR with a named function produces no finding."
  (let ((fixture-path (inspector-fixture 'atelier:check-bare-lambda)))
    (unless (probe-file fixture-path)
      (format t "~&SKIP validate-check-bare-lambda-named-function: ~A not found.~%" fixture-path)
      (return-from validate-check-bare-lambda-named-function nil))
    ;; The fixture contains exactly one violation and one correct usage.
    ;; We verify the violation count is 1 (not 2), confirming the named
    ;; function form is not flagged.
    (let* ((inspector (atelier:find-inspector 'atelier:check-bare-lambda))
           (findings (atelier:inspect-file inspector fixture-path)))
      (assert-eq 1 (length findings)))))

(define-testcase testsuite-check-bare-lambda ()
  (validate-check-bare-lambda-registered)
  (validate-check-bare-lambda-violation)
  (validate-check-bare-lambda-named-function))

;;;; End of file `check-bare-lambda.lisp'
