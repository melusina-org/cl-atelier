;;;; lint.lisp — Testing the Linter of Atelier

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.atelier/testsuite)

(define-testcase validate-lint-contents ()
  (flet ((linter1 (contents)
	   (declare (ignore contents))
	   "A fixed content")
	 (linter2 (contents)
	   (declare (ignore contents))
	   (let ((atelier::*hint-code* :hint-linter-2))
	     (atelier:hint-at-file "Linter 2")))
	 (linter3 (contents)
	   (declare (ignore contents))
	   (list
	    (let ((atelier::*hint-code* :hint-linter-3-1))
	      (atelier:hint-at-file "Linter 3 1"))
	    (let ((atelier::*hint-code* :hint-linter-3-2))
	      (atelier:hint-at-file "Linter 3 2")))))
    (multiple-value-bind (hints contents)
	(atelier::lint-contents "Some file contents" (list #'linter1 #'linter2 #'linter3))
      (assert-string= "A fixed content" contents)
      (assert-eq 3 (length hints))
      (assert-type hints 'list)
      (loop :for item :in hints
	    :do (assert-type item 'atelier::hint)))))

(define-testcase validate-lint-lines ()
  (flet ((linter1 (line)
	   (when (position #\X line)
	     "*REDACTED*"))
	 (linter2 (line)
	   (when (< 10 (length line))
	     (let ((atelier::*hint-code* :hint-linter-2))
	       (atelier:hint-at-file-line "Linter 2")))))
    (multiple-value-bind (hints lines)
	(atelier::lint-lines
	 (list "Some X contents that should be redacted"
	       "A very long line"
	       "Canary")
	 (list #'linter1 #'linter2))
      (assert-eq 3 (length lines))
      (assert-eq 1 (length hints))
      (assert-eq 2 (slot-value (first hints) 'atelier::line))
      (assert-string= "*REDACTED*" (first lines))
      (assert-string= "A very long line" (second lines))
      (assert-string= "Canary" (third lines)))))

(define-testcase testsuite-linter ()
  (validate-lint-contents)
  (validate-lint-lines) 
  (validate-codestyle-0002-hint-at-file-line-when-it-is-very-long)
  (validate-codestyle-0003-hint-at-file-when-it-lacks-canonical-header-line)
  (validate-codestyle-0004-hint-at-file-when-it-lacks-canonical-footer-line)
  (validate-codestyle-0005-hint-at-file-when-it-lacks-canonical-project-identification)
  (validate-codestyle-0006-hint-at-file-when-it-lacks-project-license-information))

;;;; End of file `lint.lisp'
