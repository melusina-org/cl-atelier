;;;; license.lisp — Tests for the Atelier License System

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:atelier/testsuite)

(define-testcase test-license-repository-load-definition ()
  (uiop:with-temporary-file (:pathname pathname :stream stream :keep t)
    (write-string
     (atelier::join-lines
      (list "---"
	    "name: Test License"
	    "id: TEST"
	    "---"
	    "Test Header 1"
	    "Test Header 2"
	    "---"
	    "Test Text 1"
	    "Test Text 2"
	    "..."))
     stream)
    (close stream)
    (let ((license (atelier::license-repository-load-definition pathname)))
      (assert-string= "Test License" (atelier:license-name license))
      (assert-string= "TEST" (atelier:license-id license))
      (assert-string= (format nil "Test Header 1~%Test Header 2")
		      (atelier:license-header license))
      (assert-string= (format nil "Test Text 1~%Test Text 2")
		      (atelier:license-text license)))
    (uiop:delete-file-if-exists pathname)))

(define-testcase testsuite-license ()
  (test-license-repository-load-definition))
