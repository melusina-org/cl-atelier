;;;; fix-bare-lambda.lisp — Testsuite for the bare lambda maintainer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)

(define-testcase validate-fix-bare-lambda ()
  "Verify fix-bare-lambda extracts a bare lambda to a named FLET.
Parses source from a string, runs the inspector in memory, invokes
prepare-resolution, applies the resolution to the string, and verifies."
  (let* ((source "(mapcar (lambda (x) (1+ x)) items)")
         (cst-forms (atelier:parse-common-lisp source))
         (top-form (first cst-forms))
         (line-vector (atelier:string-to-line-vector source))
         (inspector (atelier:find-inspector 'atelier:check-bare-lambda))
         (findings
           (let ((atelier::*current-pathname* #p"test.lisp")
                 (atelier::*current-line-vector* line-vector)
                 (atelier::*current-cst-root* top-form))
             (atelier:inspect-syntax inspector top-form)))
         (finding (first findings))
         (maintainer (atelier:find-maintainer 'atelier:fix-bare-lambda))
         (resolution (let ((atelier::*current-line-vector* line-vector))
                       (atelier:prepare-resolution maintainer finding))))
    (assert-type resolution 'atelier:text-resolution)
    (assert-eq 'atelier:fix-bare-lambda (atelier:resolution-maintainer resolution))
    (let ((result (atelier:apply-resolutions source (list resolution))))
      (assert-string=
       (format nil "(flet ((map-item (x)~%         (1+ x)))~%  (mapcar #'map-item items))")
       result))))

(define-testcase validate-fix-bare-lambda-registered ()
  "Verify fix-bare-lambda is registered in the maintainer registry."
  (assert-t (not (null (atelier:find-maintainer 'atelier:fix-bare-lambda)))))

(define-testcase testsuite-fix-bare-lambda ()
  "Run all fix-bare-lambda tests."
  (validate-fix-bare-lambda)
  (validate-fix-bare-lambda-registered))

;;;; End of file `fix-bare-lambda.lisp'
