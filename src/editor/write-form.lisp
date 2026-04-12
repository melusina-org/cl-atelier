;;;; write-form.lisp — Write a toplevel form to a string

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/editor)


;;;;
;;;; CST-to-Text Writer
;;;;
;;;; Strategy: when SOURCE-TEXT is available, copy the body's source range
;;;; verbatim from the original string. This preserves everything: #+/#-
;;;; prefixes on matching branches, whitespace, exact number formatting.
;;;; The normalize pipeline applies maintainer resolutions as span
;;;; replacements via lint-string; the write path just preserves the source.
;;;;
;;;; When SOURCE-TEXT is NIL (programmatic forms), fall back to pretty-print
;;;; from CST:RAW.
;;;;

(defun write-cst-to-string (cst source-text)
  "Convert an Eclector CST to a string.
When SOURCE-TEXT is available, return the body's source range verbatim
from the original string — preserving #+/#- prefixes, whitespace, and
exact formatting. When SOURCE-TEXT is NIL, pretty-print from CST:RAW."
  (declare (type (or cst:cst null) cst)
           (type (or string null) source-text)
           (values string))
  (if (and source-text (cst:source cst))
      (subseq source-text (car (cst:source cst)) (cdr (cst:source cst)))
      (atelier:pretty-print-form (cst:raw cst) 0)))


;;;;
;;;; Eval-When Wrapping
;;;;

(defun wrap-eval-when-if-needed (form-string eval-when-situations)
  "Wrap FORM-STRING in (EVAL-WHEN (...) ...) if EVAL-WHEN-SITUATIONS
is non-default. Return the string as-is if the situations are the default
\(:LOAD-TOPLEVEL :EXECUTE)."
  (declare (type string form-string)
           (type list eval-when-situations)
           (values string))
  (if (or (equal eval-when-situations '(:load-toplevel :execute))
          (null eval-when-situations))
      form-string
      (let ((situations-string
              (format nil "(~{~S~^ ~})" eval-when-situations)))
        (format nil "(eval-when ~A~%  ~A)"
                situations-string form-string))))


;;;;
;;;; Entry Point
;;;;

(defun write-toplevel-form-to-string (form)
  "Emit FORM as a canonical string.

When the form was parsed from a string (SOURCE-TEXT is non-NIL), the
body text is copied verbatim from the original source, preserving
#+/#- prefixes, whitespace, and exact formatting. When the form was
constructed programmatically (SOURCE-TEXT is NIL), the body is
pretty-printed from the CST's raw s-expression.

If EVAL-WHEN is non-default, the form is wrapped in
\(EVAL-WHEN (...) ...).

The result is a fixed point:
  (WRITE (READ (WRITE (READ s)))) = (WRITE (READ s))
for any string s accepted by READ-TOPLEVEL-FORM-FROM-STRING."
  (declare (type toplevel-form form)
           (values string))
  (let* ((body-cst (toplevel-form-body form))
         (source-text (toplevel-form-source-text form))
         (body-string (write-cst-to-string body-cst source-text)))
    (wrap-eval-when-if-needed body-string (toplevel-form-eval-when form))))

;;;; End of file `write-form.lisp'
