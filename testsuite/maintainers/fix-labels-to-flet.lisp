;;;; fix-labels-to-flet.lisp — Tests for the spurious LABELS maintainer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)

(define-testcase validate-fix-labels-to-flet-registered ()
  "Verify FIX-LABELS-TO-FLET is registered in the maintainer registry."
  (assert-t (not (null (atelier:find-maintainer 'atelier:fix-labels-to-flet)))))

(define-testcase validate-compute-flet-depth-table-leaf ()
  "Verify depth 0 is assigned to a function that calls no other locals."
  (let* ((bindings '((leaf (x) (* x 2))))
         (table (atelier:compute-flet-depth-table bindings)))
    (assert-t (= 0 (gethash 'leaf table)))))

(define-testcase validate-compute-flet-depth-table-caller ()
  "Verify depth 1 is assigned to a function that calls a depth-0 local."
  (let* ((bindings '((leaf (x) (* x 2))
                     (caller (items) (mapcar #'leaf items))))
         (table (atelier:compute-flet-depth-table bindings)))
    (assert-t (= 0 (gethash 'leaf table)))
    (assert-t (= 1 (gethash 'caller table)))))

(define-testcase validate-labels-transform-to-flet-simple ()
  "Verify LABELS-TRANSFORM-TO-FLET produces FLET for a single-binding form."
  ;; (labels ((double (x) (* x 2))) (double 5))
  ;; → (flet ((double (x) (* x 2))) (double 5))
  (let ((result (atelier:labels-transform-to-flet
                 '(labels ((double (x) (* x 2))) (double 5)))))
    (assert-eq 'flet (first result))
    (assert-t (equal '((double (x) (* x 2))) (second result)))
    (assert-t (equal '((double 5)) (cddr result)))))

(define-testcase validate-labels-transform-to-flet-chain ()
  "Verify LABELS-TRANSFORM-TO-FLET nests a caller around its callee."
  ;; (labels ((leaf (x) (* x 2)) (caller (items) (mapcar #'leaf items))) body)
  ;; depth-0 (leaf) is outermost FLET; depth-1 (caller) is inner FLET.
  (let ((result (atelier:labels-transform-to-flet
                 '(labels ((leaf (x) (* x 2))
                           (caller (items) (mapcar #'leaf items)))
                   (caller data)))))
    ;; Outermost form is FLET
    (assert-eq 'flet (first result))
    ;; Outer FLET binds leaf (depth 0)
    (assert-eq 'leaf (first (first (second result))))
    ;; Body of outer FLET is a nested FLET for caller (depth 1)
    (let ((inner (first (cddr result))))
      (assert-eq 'flet (first inner))
      (assert-eq 'caller (first (first (second inner)))))))

(define-testcase validate-fix-labels-to-flet-end-to-end ()
  "Verify fix-labels-to-flet transforms a spurious LABELS form to nested FLET.
Writes a LABELS form to a temp file, inspects it to get a real finding with
correct CST source positions, invokes the maintainer, applies the resolution,
and verifies the result uses FLET instead of LABELS."
  (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (write-string
       "(labels ((leaf (x) (* x 2)) (caller (items) (mapcar #'leaf items))) (caller data))"
       s))
    (let* ((cst-forms (atelier::parse-lisp-file p))
           (top-form (first cst-forms))
           (line-vector (atelier:read-file-into-line-vector p))
           (inspector (atelier:find-inspector 'atelier:check-labels-for-flet))
           (findings
             (let ((atelier::*current-pathname* p)
                   (atelier::*current-line-vector* line-vector)
                   (atelier::*current-cst-root* top-form))
               (atelier:inspect-syntax inspector top-form)))
           (finding (first findings))
           (maintainer (atelier:find-maintainer 'atelier:fix-labels-to-flet))
           (resolution (atelier:prepare-resolution maintainer finding)))
      (assert-type resolution 'atelier:syntax-resolution)
      (assert-eq 'atelier:fix-labels-to-flet (atelier:resolution-maintainer resolution))
      ;; Apply the resolution and verify LABELS is replaced by FLET.
      (atelier:apply-resolutions-to-file p (list resolution))
      (let ((result (uiop:read-file-string p :external-format :utf-8)))
        (assert-t (not (null (search "(flet" result :test #'char=))))
        (assert-t (null (search "(labels" result :test #'char=)))))))

(define-testcase testsuite-fix-labels-to-flet ()
  (validate-fix-labels-to-flet-registered)
  (validate-compute-flet-depth-table-leaf)
  (validate-compute-flet-depth-table-caller)
  (validate-labels-transform-to-flet-simple)
  (validate-labels-transform-to-flet-chain)
  (validate-fix-labels-to-flet-end-to-end))

;;;; End of file `fix-labels-to-flet.lisp'
