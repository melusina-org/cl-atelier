;;;; check-mixed-indentation.lisp — Tests for the mixed indentation inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/test)

(define-testcase validate-check-mixed-indentation-clean ()
  "Verify that a spaces-only file produces no findings with default config."
  (let* ((fixture-path (merge-pathnames "valid-with-spdx.lisp"
                                        (testsuite-fixtures-directory)))
         (atelier:*project-configuration* nil)
         (atelier:*linter-configuration*
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
          (let* ((atelier:*project-configuration* nil)
                 (atelier:*linter-configuration*
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
          (let* ((atelier:*project-configuration* nil)
                 (atelier:*linter-configuration*
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
  (validate-check-mixed-indentation-clean)
  (validate-check-mixed-indentation-tabs)
  (validate-indentation-style-configuration))

;;;; End of file `check-mixed-indentation.lisp'
