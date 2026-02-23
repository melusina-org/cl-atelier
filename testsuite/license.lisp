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

(define-testcase test-license-repository-load-definition-from-text ()
  (uiop:with-temporary-file (:pathname pathname :stream stream :keep t)
    (format stream "---~%name: Test License~%id: TEST~%---~%Test Header~%Line 2~%---~%Test Text~%More text~%")
    (close stream)
    (let ((license (atelier::license-repository-load-definition-from-text pathname)))
      (assert-string= "Test License" (slot-value license 'atelier::license-name))
      (assert-string= "TEST" (slot-value license 'atelier::license-id))
      (assert-string= (format nil "Test Header~%Line 2") (slot-value license 'atelier::license-header))
      (assert-string= (format nil "Test Text~%More text") (slot-value license 'atelier::license-text)))
    (uiop:delete-file-if-exists pathname)))

(define-testcase testsuite-license ()
  (test-license-repository-load-definition-from-text))
