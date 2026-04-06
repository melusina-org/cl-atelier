;;;; runner.lisp — Tests for the inspector runner

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:atelier/testsuite)

(define-testcase validate-run-file-inspectors ()
  "Verify that the runner calls file-level inspectors and collects findings."
  (let* ((fixture-path (merge-pathnames "missing-spdx.lisp"
                                        (testsuite-fixtures-directory)))
         (findings (atelier:run-file-inspectors fixture-path nil nil)))
    ;; At minimum, the SPDX inspector should produce a finding
    (assert-t (not (null findings)))
    (assert-t (every (lambda (finding) (typep finding 'atelier:finding)) findings))))

(define-testcase validate-run-file-inspectors-respects-policy ()
  "Verify that the runner skips disabled inspectors."
  (let* ((fixture-path (merge-pathnames "missing-spdx.lisp"
                                        (testsuite-fixtures-directory)))
         (policy (atelier:make-linter-configuration
                   :disabled-inspectors '(atelier:check-spdx-license-header
                                          atelier:check-file-encoding)))
         (findings (atelier:run-file-inspectors fixture-path nil policy)))
    (assert-t (null findings))))

(define-testcase validate-run-file-inspectors-severity-override ()
  "Verify that severity overrides are applied to findings."
  (let* ((fixture-path (merge-pathnames "missing-spdx.lisp"
                                        (testsuite-fixtures-directory)))
         (policy (atelier:make-linter-configuration
                   :severity-overrides
                   '((atelier:check-spdx-license-header . :error))))
         (findings (atelier:run-file-inspectors fixture-path nil policy)))
    (let ((spdx-findings
            (remove-if-not (lambda (finding)
                             (eq 'atelier:check-spdx-license-header
                                 (atelier:finding-inspector finding)))
                           findings)))
      (assert-t (not (null spdx-findings)))
      (assert-eq :error (atelier:finding-severity (first spdx-findings))))))

(define-testcase testsuite-runner ()
  (validate-run-file-inspectors)
  (validate-run-file-inspectors-respects-policy)
  (validate-run-file-inspectors-severity-override))

;;;; End of file `runner.lisp'
