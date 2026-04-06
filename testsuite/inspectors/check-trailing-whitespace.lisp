;;;; check-trailing-whitespace.lisp — Tests for the trailing whitespace inspector

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

(define-testcase validate-check-trailing-whitespace-registered ()
  "Verify that CHECK-TRAILING-WHITESPACE is registered."
  (assert-t (not (null (member 'atelier:check-trailing-whitespace
                               (atelier:list-inspectors))))))

(define-testcase validate-check-trailing-whitespace-clean ()
  "Verify that a file without trailing whitespace produces no findings."
  (let* ((fixture-path (merge-pathnames "valid-with-spdx.lisp"
                                        (testsuite-fixtures-directory)))
         (atelier:*current-project-configuration* nil)
         (atelier:*current-linter-configuration*
           (atelier:make-linter-configuration))
         (inspector-instance
           (atelier:find-inspector 'atelier:check-trailing-whitespace))
         (findings (atelier:inspect-file inspector-instance fixture-path)))
    (assert-t (null findings))))

(define-testcase validate-check-trailing-whitespace-dirty ()
  "Verify that a file with trailing spaces produces findings."
  (let ((temporary-path (merge-pathnames "trailing-ws-test.lisp"
                                         (uiop:temporary-directory))))
    (unwind-protect
        (progn
          (with-open-file (stream temporary-path
                                  :direction :output
                                  :if-exists :supersede)
            (write-string "clean line" stream)
            (write-char #\Newline stream)
            (write-string "dirty line   " stream)
            (write-char #\Newline stream)
            (write-string "also clean" stream)
            (write-char #\Newline stream))
          (let* ((atelier:*current-project-configuration* nil)
                 (atelier:*current-linter-configuration*
                   (atelier:make-linter-configuration))
                 (inspector-instance
                   (atelier:find-inspector 'atelier:check-trailing-whitespace))
                 (findings
                   (atelier:inspect-file inspector-instance temporary-path)))
            (assert-eq 1 (length findings))
            (assert-t (typep (first findings)
                             'atelier:trailing-whitespace-finding))
            (assert-eq 2 (atelier:finding-line (first findings)))))
      (when (probe-file temporary-path)
        (delete-file temporary-path)))))

(define-testcase testsuite-check-trailing-whitespace ()
  (validate-check-trailing-whitespace-registered)
  (validate-check-trailing-whitespace-clean)
  (validate-check-trailing-whitespace-dirty))

;;;; End of file `check-trailing-whitespace.lisp'
