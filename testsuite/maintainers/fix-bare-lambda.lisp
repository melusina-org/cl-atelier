;;;; fix-bare-lambda.lisp — Testsuite for the bare lambda maintainer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)

(define-testcase validate-fix-bare-lambda ()
  "Verify fix-bare-lambda extracts a bare lambda to a named FLET and writes it back.
Writes '(mapcar (lambda (x) (1+ x)) items)' to a temp file, runs the inspector
to obtain a real finding (with correct CST source positions), invokes
prepare-resolution, then applies the resolution and verifies the result."
  (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (write-string "(mapcar (lambda (x) (1+ x)) items)" s))
    ;; Parse the file and run the inspector to get a finding with real source positions.
    (let* ((cst-forms (atelier::parse-lisp-file p))
           (top-form (first cst-forms))
           (line-vector (atelier:read-file-into-line-vector p))
           (inspector (atelier:find-inspector 'atelier:check-bare-lambda))
           (findings
             (let ((atelier::*current-pathname* p)
                   (atelier::*current-line-vector* line-vector)
                   (atelier::*current-cst-root* top-form))
               (atelier:inspect-syntax inspector top-form)))
           (finding (first findings))
           (maintainer (atelier:find-maintainer 'atelier:fix-bare-lambda))
           (resolution (atelier:prepare-resolution maintainer finding)))
      (assert-type resolution 'atelier:text-resolution)
      (assert-eq 'atelier:fix-bare-lambda (atelier:resolution-maintainer resolution))
      ;; Apply the resolution and verify the file content.
      (atelier:apply-resolutions-to-file p (list resolution))
      (let ((result (uiop:read-file-string p :external-format :utf-8)))
        (assert-string=
         (format nil "(flet ((map-item (x)~%         (1+ x)))~%  (mapcar #'map-item items))")
         result)))))

(define-testcase validate-fix-bare-lambda-registered ()
  "Verify fix-bare-lambda is registered in the maintainer registry."
  (assert-t (not (null (atelier:find-maintainer 'atelier:fix-bare-lambda)))))

(define-testcase testsuite-fix-bare-lambda ()
  "Run all fix-bare-lambda tests."
  (validate-fix-bare-lambda)
  (validate-fix-bare-lambda-registered))

;;;; End of file `fix-bare-lambda.lisp'
