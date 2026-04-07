;;;; check-earmuffs.lisp — Tests for the earmuffs naming convention inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)

(define-testcase validate-check-earmuffs-registered ()
  "Verify that CHECK-EARMUFFS is registered in the inspector registry."
  (assert-t (not (null (member 'atelier:check-earmuffs
                               (atelier:list-inspectors))))))

(define-testcase validate-check-earmuffs-correct ()
  "Verify that a file with correct *earmuffs* produces no findings."
  (let ((fixture-path (inspector-fixture 'atelier:check-earmuffs "good")))
    (unless (probe-file fixture-path)
      (format t "~&SKIP validate-check-earmuffs-correct: ~A not found.~%" fixture-path)
      (return-from validate-check-earmuffs-correct nil))
    (let* ((inspector (atelier:find-inspector 'atelier:check-earmuffs))
           (findings (atelier:inspect-file inspector fixture-path)))
      (assert-t (null findings)))))

(define-testcase validate-check-earmuffs-violation ()
  "Verify that a file with missing earmuffs produces EARMUFFS-FINDING instances."
  (let ((fixture-path (inspector-fixture 'atelier:check-earmuffs "bad")))
    (unless (probe-file fixture-path)
      (format t "~&SKIP validate-check-earmuffs-violation: ~A not found.~%" fixture-path)
      (return-from validate-check-earmuffs-violation nil))
    (let* ((inspector (atelier:find-inspector 'atelier:check-earmuffs))
           (findings (atelier:inspect-file inspector fixture-path)))
      (assert-t (not (null findings)))
      (assert-t (every (lambda (f) (typep f 'atelier:earmuffs-finding)) findings))
      (assert-eq 2 (length findings)))))

(define-testcase testsuite-check-earmuffs ()
  (validate-check-earmuffs-registered)
  (validate-check-earmuffs-correct)
  (validate-check-earmuffs-violation))

;;;; End of file `check-earmuffs.lisp'
