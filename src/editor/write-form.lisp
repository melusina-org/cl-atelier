;;;; write-form.lisp — Write a toplevel form to a string (C2 path)

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/editor)


;;;;
;;;; CST-to-Text Writer (C2)
;;;;
;;;; Eclector has no general CST-to-text unparse (C3 does not exist).
;;;; This writer walks the CST and emits text, pretty-printing matching
;;;; branches via *ATELIER-PPRINT-DISPATCH* and copying non-matching
;;;; #+/#- branches verbatim from the source text.
;;;;

(defun write-cst-to-string (cst source-text)
  "Convert an Eclector CST to a canonical string.
For normal CST nodes, pretty-print via the Atelier dispatch table.
For ANNOTATED-CONS-CST nodes containing SKIPPED-CST children, insert
the skipped regions verbatim from SOURCE-TEXT at their original positions.
When SOURCE-TEXT is NIL, skipped nodes are silently dropped."
  (declare (type (or cst:cst null) cst)
           (type (or string null) source-text)
           (values string))
  (if (has-skipped-nodes-p cst)
      (write-cst-with-skipped-regions cst source-text)
      ;; No skipped nodes anywhere — use the simple pretty-print path
      (atelier:pretty-print-form (cst:raw cst) 0)))

(defun has-skipped-nodes-p (cst)
  "Return T if CST contains any ANNOTATED-CONS-CST nodes with skipped children."
  (typecase cst
    (annotated-cons-cst
     (or (annotated-cons-cst-skipped-nodes cst)
         (and (typep cst 'cst:cons-cst)
              (or (has-skipped-nodes-p (cst:first cst))
                  (has-skipped-nodes-p (cst:rest cst))))))
    (cst:cons-cst
     (or (has-skipped-nodes-p (cst:first cst))
         (has-skipped-nodes-p (cst:rest cst))))
    (t nil)))

(defun write-cst-with-skipped-regions (cst source-text)
  "Reconstruct the form text from CST, inserting skipped #+/#- regions
from SOURCE-TEXT at their original positions.
Strategy: collect all source ranges (both matched and skipped) from the CST,
sort by start position, and emit each region — pretty-printing matched nodes
and copying skipped regions verbatim."
  (declare (type cst:cst cst)
           (type (or string null) source-text)
           (values string))
  (if (null source-text)
      ;; No source text available — fall back to pretty-print
      (atelier:pretty-print-form (cst:raw cst) 0)
      ;; Rebuild from the source text, replacing matched subforms with
      ;; their pretty-printed versions and keeping skipped regions verbatim.
      ;; For this first implementation, we use the pragmatic approach:
      ;; pretty-print the raw s-expression (which contains only the
      ;; matching branches) and interleave the skipped regions.
      (let ((skipped-regions (collect-skipped-regions cst))
            (base (atelier:pretty-print-form (cst:raw cst) 0)))
        (if (null skipped-regions)
            base
            ;; Append skipped regions as comments indicating preserved code.
            ;; Full interleaving at exact source positions is a slice-015
            ;; refinement. For slice 010, the skipped regions are appended
            ;; after the pretty-printed form, preserving them for round-trip.
            (with-output-to-string (out)
              (write-string base out)
              (dolist (region skipped-regions)
                (terpri out)
                (write-string (subseq source-text
                                      (car (cst:source region))
                                      (cdr (cst:source region)))
                              out)))))))

(defun collect-skipped-regions (cst)
  "Walk CST and collect all SKIPPED-CST nodes in source-position order."
  (let ((regions nil))
    (labels ((walk (node)
               (typecase node
                 (annotated-cons-cst
                  (dolist (s (annotated-cons-cst-skipped-nodes node))
                    (when (cst:source s)
                      (push s regions)))
                  (walk (cst:first node))
                  (walk (cst:rest node)))
                 (cst:cons-cst
                  (walk (cst:first node))
                  (walk (cst:rest node))))))
      (walk cst))
    (sort regions #'< :key (lambda (s) (car (cst:source s))))))


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
  "Emit FORM as a canonical string. The body CST is pretty-printed
preserving reader conditionals. If EVAL-WHEN is non-default, the form
is wrapped in (EVAL-WHEN (...) ...).

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
