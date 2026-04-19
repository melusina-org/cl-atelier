;;;; eclector-client.lisp — Eclector #+/#- preservation capability tests

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/test/editor)

(define-testcase validate-eclector-preserves-feature-expression ()
  "Verify that the preserving CST client captures #+/#- as skipped-cst nodes."
  (let* ((input "(defun foo () #+sbcl :yes #-sbcl :no)")
         (form (atelier/editor:read-toplevel-form-from-string input))
         (source-text (atelier/editor:toplevel-form-source-text form)))
    ;; The source text should be the full input
    (assert-string= input source-text)
    ;; The write path should preserve the #-sbcl region
    (let ((output (atelier/editor:write-toplevel-form-to-string form)))
      (assert-t* (search "#+sbcl" output))
      (assert-t* (search "#-sbcl" output)))))

(define-testcase validate-eclector-cst-to-text-round-trip ()
  "Verify that write(read(s)) preserves #+/#- text for round-trip."
  (let* ((input "(defun foo () #+sbcl :yes #-sbcl :no)")
         (form (atelier/editor:read-toplevel-form-from-string input))
         (output (atelier/editor:write-toplevel-form-to-string form)))
    ;; Both branches should be present in the output
    (assert-t* (search "#+sbcl :yes" output))
    (assert-t* (search "#-sbcl :no" output))))

;;;; End of file `eclector-client.lisp'
