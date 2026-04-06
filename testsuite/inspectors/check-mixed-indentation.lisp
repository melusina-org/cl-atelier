;;;; check-mixed-indentation.lisp — Tests for the mixed indentation inspector

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

(define-testcase validate-check-mixed-indentation-registered ()
  "Verify that CHECK-MIXED-INDENTATION is registered."
  (assert-t (not (null (member 'atelier:check-mixed-indentation
                               (atelier:list-inspectors))))))

(define-testcase validate-check-mixed-indentation-clean ()
  "Verify that a spaces-only file produces no findings with default config."
  (let* ((fixture-path (merge-pathnames "valid-with-spdx.lisp"
                                        (testsuite-fixtures-directory)))
         (atelier:*current-project-configuration* nil)
         (atelier:*current-linter-configuration*
           (atelier:make-linter-configuration))
         (inspector-instance
           (atelier:find-inspector 'atelier:check-mixed-indentation))
         (findings (atelier:inspect-file inspector-instance fixture-path)))
    (assert-t (null findings))))

(define-testcase validate-check-mixed-indentation-tabs ()
  "Verify that a file with tabs produces findings under spaces-default config."
  (let ((temporary-path (merge-pathnames "mixed-indent-test.lisp"
                                         (uiop:temporary-directory))))
    (unwind-protect
        (progn
          (with-open-file (stream temporary-path
                                  :direction :output
                                  :if-exists :supersede)
            (write-string "  spaces-line" stream)
            (write-char #\Newline stream)
            (write-char #\Tab stream)
            (write-string "tab-line" stream)
            (write-char #\Newline stream)
            (write-string "  spaces-again" stream)
            (write-char #\Newline stream))
          (let* ((atelier:*current-project-configuration* nil)
                 (atelier:*current-linter-configuration*
                   (atelier:make-linter-configuration))
                 (inspector-instance
                   (atelier:find-inspector 'atelier:check-mixed-indentation))
                 (findings
                   (atelier:inspect-file inspector-instance temporary-path)))
            (assert-eq 1 (length findings))
            (assert-t (typep (first findings)
                             'atelier:mixed-indentation-finding))
            (assert-eq 2 (atelier:finding-line (first findings)))))
      (when (probe-file temporary-path)
        (delete-file temporary-path)))))

(define-testcase validate-indentation-style-configuration ()
  "Verify that configuring :TABS flags spaces instead."
  (let ((temporary-path (merge-pathnames "indent-style-test.lisp"
                                         (uiop:temporary-directory))))
    (unwind-protect
        (progn
          (with-open-file (stream temporary-path
                                  :direction :output
                                  :if-exists :supersede)
            (write-char #\Tab stream)
            (write-string "tab-line" stream)
            (write-char #\Newline stream)
            (write-string "  spaces-line" stream)
            (write-char #\Newline stream))
          (let* ((atelier:*current-project-configuration* nil)
                 (atelier:*current-linter-configuration*
                   (atelier:make-linter-configuration
                     :indentation-style :tabs))
                 (inspector-instance
                   (atelier:find-inspector 'atelier:check-mixed-indentation))
                 (findings
                   (atelier:inspect-file inspector-instance temporary-path)))
            (assert-eq 1 (length findings))
            (assert-eq 2 (atelier:finding-line (first findings)))))
      (when (probe-file temporary-path)
        (delete-file temporary-path)))))

(define-testcase testsuite-check-mixed-indentation ()
  (validate-check-mixed-indentation-registered)
  (validate-check-mixed-indentation-clean)
  (validate-check-mixed-indentation-tabs)
  (validate-indentation-style-configuration))

;;;; End of file `check-mixed-indentation.lisp'
