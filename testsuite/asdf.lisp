;;;; asdf.lisp — Tests for ASDF integration

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


;;;;
;;;; Project Configuration
;;;;

(define-testcase validate-project-configuration-defaults ()
  "Verify that a project configuration with no arguments has NIL defaults."
  (let ((project-configuration (atelier:make-project-configuration)))
    (assert-t (null (atelier:project-configuration-copyright-holder project-configuration)))
    (assert-t (null (atelier:project-configuration-copyright-year project-configuration)))
    (assert-t (null (atelier:project-configuration-project-filename project-configuration)))
    (assert-t (null (atelier:project-configuration-project-name project-configuration)))
    (assert-t (null (atelier:project-configuration-project-description project-configuration)))
    (assert-t (null (atelier:project-configuration-project-long-description project-configuration)))
    (assert-t (null (atelier:project-configuration-homepage project-configuration)))
    (assert-t (null (atelier:project-configuration-license project-configuration)))))

(define-testcase validate-read-project-configuration ()
  "Verify that READ-PROJECT-CONFIGURATION reads a .sexp file correctly."
  (let ((temporary-path (merge-pathnames "test-project-config.sexp"
                                         (uiop:temporary-directory))))
    (unwind-protect
        (progn
          (with-open-file (stream temporary-path
                                  :direction :output
                                  :if-exists :supersede)
            (write-string (concatenate
                           'string
                           "(:copyright-holder \"A. U. Thor\""
                           " :copyright-year \"2017-2026\""
                           " :project-filename \"example\""
                           " :project-name \"Example\""
                           " :project-description \"An example project\""
                           " :project-long-description \"A longer description.\""
                           " :homepage \"https://example.com\""
                           " :license \"MIT\")")
                          stream))
          (let ((project-configuration
                  (atelier:read-project-configuration temporary-path)))
            (assert-string= "A. U. Thor"
                            (atelier:project-configuration-copyright-holder
                             project-configuration))
            (assert-string= "2017-2026"
                            (atelier:project-configuration-copyright-year
                             project-configuration))
            (assert-string= "example"
                            (atelier:project-configuration-project-filename
                             project-configuration))
            (assert-string= "Example"
                            (atelier:project-configuration-project-name
                             project-configuration))
            (assert-string= "An example project"
                            (atelier:project-configuration-project-description
                             project-configuration))
            (assert-string= "https://example.com"
                            (atelier:project-configuration-homepage
                             project-configuration))
            (assert-string= "MIT"
                            (atelier:project-configuration-license
                             project-configuration))))
      (when (probe-file temporary-path)
        (delete-file temporary-path)))))


;;;;
;;;; Linter Configuration
;;;;

(define-testcase validate-read-linter-configuration ()
  "Verify that READ-LINTER-CONFIGURATION reads a .sexp file with *READ-EVAL* NIL."
  (let ((temporary-path (merge-pathnames "test-linter-config.sexp"
                                         (uiop:temporary-directory))))
    (unwind-protect
        (progn
          (with-open-file (stream temporary-path
                                  :direction :output
                                  :if-exists :supersede)
            (write-string "(:disabled-inspectors (atelier:check-file-encoding) :severity-overrides ((atelier:check-spdx-license-header . :error)))"
                          stream))
          (let ((linter-configuration (atelier:read-linter-configuration temporary-path)))
            (assert-t (not (null (member 'atelier:check-file-encoding
                                        (atelier:linter-configuration-disabled-inspectors
                                         linter-configuration)))))
            (let ((override (assoc 'atelier:check-spdx-license-header
                                   (atelier:linter-configuration-severity-overrides
                                    linter-configuration))))
              (assert-t (not (null override)))
              (assert-eq :error (cdr override)))))
      (when (probe-file temporary-path)
        (delete-file temporary-path)))))

(define-testcase validate-linter-configuration-disables-inspector ()
  "Verify that disabled inspectors are skipped by the runner."
  (let* ((fixture-path (merge-pathnames "missing-spdx.lisp"
                                        (testsuite-fixtures-directory)))
         (policy (atelier:make-linter-configuration
                   :disabled-inspectors '(atelier:check-spdx-license-header
                                          atelier:check-file-encoding)))
         (atelier:*linter-configuration* policy)
         (findings (atelier:perform-inspection fixture-path)))
    (assert-t (null findings))))

(define-testcase validate-linter-configuration-severity-override ()
  "Verify that severity overrides are applied by the runner."
  (let* ((fixture-path (merge-pathnames "missing-spdx.lisp"
                                        (testsuite-fixtures-directory)))
         (policy (atelier:make-linter-configuration
                   :severity-overrides
                   '((atelier:check-spdx-license-header . :error))))
         (atelier:*linter-configuration* policy)
         (findings (atelier:perform-inspection fixture-path)))
    (let ((spdx-findings
            (remove-if-not (lambda (finding)
                             (eq 'atelier:check-spdx-license-header
                                 (atelier:finding-inspector finding)))
                           findings)))
      (assert-t (not (null spdx-findings)))
      (assert-eq :error (atelier:finding-severity (first spdx-findings))))))


;;;;
;;;; Test Suite
;;;;

(define-testcase testsuite-asdf ()
  (validate-project-configuration-defaults)
  (validate-read-project-configuration)
  (validate-read-linter-configuration)
  (validate-linter-configuration-disables-inspector)
  (validate-linter-configuration-severity-override))

;;;; End of file `asdf.lisp'
