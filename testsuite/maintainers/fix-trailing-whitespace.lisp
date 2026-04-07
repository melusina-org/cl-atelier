;;;; fix-trailing-whitespace.lisp — Testsuite for the trailing whitespace maintainer

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

(define-testcase validate-fix-trailing-whitespace ()
  "Verify fix-trailing-whitespace produces a text-resolution with empty replacement.
Uses a synthetic finding where column 14 to 17 are trailing spaces."
  (let* ((finding
           (make-instance 'atelier:trailing-whitespace-finding
            :inspector 'atelier:check-trailing-whitespace
            :severity :style
            :observation "Line 1 has 3 trailing whitespace characters."
            :rationale "Trailing whitespace creates noisy diffs."
            :file #p"/tmp/test.lisp"
            :line 1
            :column 14
            :end-line 1
            :end-column 17
            :source-text "(defvar *x* 1)   "))
         (maintainer (atelier:find-maintainer 'atelier:fix-trailing-whitespace))
         (resolution (atelier:prepare-resolution maintainer finding)))
    (assert-type resolution 'atelier:text-resolution)
    (assert-string= "" (atelier:resolution-replacement resolution))
    (assert-eq 'atelier:fix-trailing-whitespace
               (atelier:resolution-maintainer resolution))))

(define-testcase validate-line-maintainers-registered ()
  "Verify fix-trailing-whitespace and fix-mixed-indentation are in the registry."
  (assert-t (not (null (atelier:find-maintainer 'atelier:fix-trailing-whitespace))))
  (assert-t (not (null (atelier:find-maintainer 'atelier:fix-mixed-indentation)))))

(define-testcase testsuite-fix-trailing-whitespace ()
  "Run all fix-trailing-whitespace tests."
  (validate-fix-trailing-whitespace)
  (validate-line-maintainers-registered))

;;;; End of file 'fix-trailing-whitespace.lisp'
