;;;; check-line-length.lisp — Tests for the line length inspector

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

(define-testcase validate-check-line-length-registered ()
  "Verify that CHECK-LINE-LENGTH is registered."
  (assert-t (not (null (member 'atelier:check-line-length
                               (atelier:list-inspectors))))))

(define-testcase validate-check-line-length-short ()
  "Verify that a file with short lines produces no findings."
  (let* ((fixture-path (merge-pathnames "valid-with-spdx.lisp"
                                        (testsuite-fixtures-directory)))
         (atelier:*current-project-configuration* nil)
         (atelier:*current-linter-configuration*
           (atelier:make-linter-configuration))
         (inspector-instance
           (atelier:find-inspector 'atelier:check-line-length))
         (findings (atelier:inspect-file inspector-instance fixture-path)))
    (assert-t (null findings))))

(define-testcase validate-check-line-length-long ()
  "Verify that a file with long lines produces findings."
  (let* ((fixture-path (merge-pathnames "long-lines.lisp"
                                        (testsuite-fixtures-directory)))
         (atelier:*current-project-configuration* nil)
         (atelier:*current-linter-configuration*
           (atelier:make-linter-configuration))
         (inspector-instance
           (atelier:find-inspector 'atelier:check-line-length))
         (findings (atelier:inspect-file inspector-instance fixture-path)))
    ;; Should find the long comment and the long comment-prefixed word,
    ;; but not the definition line
    (assert-eq 2 (length findings))
    (assert-t (typep (first findings) 'atelier:line-too-long-finding))))

(define-testcase validate-check-line-length-skips-definitions ()
  "Verify that definition lines are skipped regardless of length."
  (let ((temporary-path (merge-pathnames "def-line-test.lisp"
                                         (uiop:temporary-directory))))
    (unwind-protect
        (progn
          (with-open-file (stream temporary-path
                                  :direction :output
                                  :if-exists :supersede)
            (format stream "(defun ~A ()~%  nil)~%"
                    (make-string 120 :initial-element #\x)))
          (let* ((atelier:*current-project-configuration* nil)
                 (atelier:*current-linter-configuration*
                   (atelier:make-linter-configuration))
                 (inspector-instance
                   (atelier:find-inspector 'atelier:check-line-length))
                 (findings
                   (atelier:inspect-file inspector-instance temporary-path)))
            (assert-t (null findings))))
      (when (probe-file temporary-path)
        (delete-file temporary-path)))))

(define-testcase testsuite-check-line-length ()
  (validate-check-line-length-registered)
  (validate-check-line-length-short)
  (validate-check-line-length-long)
  (validate-check-line-length-skips-definitions))

;;;; End of file `check-line-length.lisp'
