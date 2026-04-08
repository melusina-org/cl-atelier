;;;; license.lisp — Tests for the Atelier License System

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

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

;;;; End of file `license.lisp'
