;;;; runner.lisp — Tests for the inspector runner

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)

(define-testcase validate-perform-inspection ()
  "Verify that the runner calls file and line inspectors and collects findings."
  (let* ((fixture-path (merge-pathnames "missing-spdx.lisp"
                                        (testsuite-fixtures-directory)))
         (findings (atelier:perform-inspection fixture-path)))
    (assert-t (not (null findings)))
    (assert-t (every (lambda (finding)
                       (typep finding 'atelier:finding))
                     findings))))

(define-testcase validate-perform-inspection-respects-policy ()
  "Verify that the runner skips disabled inspectors."
  (let* ((fixture-path (merge-pathnames "missing-spdx.lisp"
                                        (testsuite-fixtures-directory)))
         (all-inspector-names (atelier:list-inspectors))
         (policy (atelier:make-linter-configuration
                   :disabled-inspectors all-inspector-names))
         (atelier:*linter-configuration* policy)
         (findings (atelier:perform-inspection fixture-path)))
    (assert-t (null findings))))

(define-testcase validate-perform-inspection-severity-override ()
  "Verify that severity overrides are applied to findings."
  (let* ((fixture-path (merge-pathnames "missing-spdx.lisp"
                                        (testsuite-fixtures-directory)))
         (policy (atelier:make-linter-configuration
                   :severity-overrides
                   '((atelier:check-spdx-license-header . :error))))
         (atelier:*linter-configuration* policy)
         (findings (atelier:perform-inspection fixture-path)))
    (flet ((spdx-finding-p (finding)
             (eq 'atelier:check-spdx-license-header
                 (atelier:finding-inspector finding))))
      (let ((spdx-findings (remove-if-not #'spdx-finding-p findings)))
        (assert-t (not (null spdx-findings)))
        (assert-eq :error (atelier:finding-severity (first spdx-findings)))))))

(define-testcase testsuite-runner ()
  (validate-perform-inspection)
  (validate-perform-inspection-respects-policy)
  (validate-perform-inspection-severity-override))

;;;; End of file `runner.lisp'
