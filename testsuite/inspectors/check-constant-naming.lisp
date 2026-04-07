;;;; check-constant-naming.lisp — Tests for the constant naming convention inspector

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

(define-testcase validate-check-constant-naming-registered ()
  "Verify that CHECK-CONSTANT-NAMING is registered in the inspector registry."
  (assert-t (not (null (member 'atelier:check-constant-naming
                               (atelier:list-inspectors))))))

(define-testcase validate-check-constant-naming-violation ()
  "Verify that a DEFCONSTANT without +plus+ convention produces a CONSTANT-NAMING-FINDING."
  (let ((fixture-path (merge-pathnames "constant-naming-bad.lisp"
                                       (testsuite-fixtures-directory))))
    (unless (probe-file fixture-path)
      (format t "~&SKIP validate-check-constant-naming-violation: ~A not found.~%" fixture-path)
      (return-from validate-check-constant-naming-violation nil))
    (let* ((inspector (atelier:find-inspector 'atelier:check-constant-naming))
           (findings (atelier:inspect-file inspector fixture-path)))
      (assert-t (not (null findings)))
      (assert-t (typep (first findings) 'atelier:constant-naming-finding))
      (assert-eq 1 (length findings)))))

(define-testcase testsuite-check-constant-naming ()
  (validate-check-constant-naming-registered)
  (validate-check-constant-naming-violation))

;;;; End of file `check-constant-naming.lisp'
