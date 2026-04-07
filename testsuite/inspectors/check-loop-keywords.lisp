;;;; check-loop-keywords.lisp — Tests for the bare loop keyword inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)

(define-testcase validate-check-loop-keywords-registered ()
  "Verify that CHECK-LOOP-KEYWORDS is registered in the inspector registry."
  (assert-t (not (null (member 'atelier:check-loop-keywords
                               (atelier:list-inspectors))))))

(define-testcase validate-check-loop-keywords-violation ()
  "Verify that a LOOP with bare clause keywords produces BARE-LOOP-KEYWORD-FINDING instances."
  (let ((fixture-path (inspector-fixture 'atelier:check-loop-keywords)))
    (unless (probe-file fixture-path)
      (format t "~&SKIP validate-check-loop-keywords-violation: ~A not found.~%" fixture-path)
      (return-from validate-check-loop-keywords-violation nil))
    (let* ((inspector (atelier:find-inspector 'atelier:check-loop-keywords))
           (findings (atelier:inspect-file inspector fixture-path)))
      (assert-t (not (null findings)))
      (assert-t (every (lambda (f) (typep f 'atelier:bare-loop-keyword-finding)) findings)))))

(define-testcase validate-check-loop-keywords-correct ()
  "Verify that a LOOP with keyword symbols produces no findings."
  (let ((fixture-path (inspector-fixture 'atelier:check-loop-keywords)))
    (unless (probe-file fixture-path)
      (format t "~&SKIP validate-check-loop-keywords-correct: ~A not found.~%" fixture-path)
      (return-from validate-check-loop-keywords-correct nil))
    ;; The fixture contains one bare-keyword loop (3 violations) and one
    ;; correct keyword-symbol loop (0 violations). Total findings must be
    ;; exactly 3, confirming the correct form is not flagged.
    (let* ((inspector (atelier:find-inspector 'atelier:check-loop-keywords))
           (findings (atelier:inspect-file inspector fixture-path)))
      (assert-eq 3 (length findings)))))

(define-testcase testsuite-check-loop-keywords ()
  (validate-check-loop-keywords-registered)
  (validate-check-loop-keywords-violation)
  (validate-check-loop-keywords-correct))

;;;; End of file `check-loop-keywords.lisp'
