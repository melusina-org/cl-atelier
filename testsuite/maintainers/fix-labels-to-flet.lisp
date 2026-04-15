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
  (let ((result (atelier:labels-transform-to-flet
                 '(flet ((double (x)
                           (* x 2)))
                    (double 5)))))
    (assert-eq 'flet (first result))
    (assert-t (equal '((double (x) (* x 2))) (second result)))
    (assert-t (equal '((double 5)) (cddr result)))))

(define-testcase validate-labels-transform-to-flet-chain ()
  "Verify LABELS-TRANSFORM-TO-FLET nests a caller around its callee."
  (let ((result (atelier:labels-transform-to-flet
                 '(flet ((leaf (x)
                           (* x 2)))
                    (flet ((caller (items)
                             (mapcar #'leaf items)))
                      (caller data))))))
    (assert-eq 'flet (first result))
    (assert-eq 'leaf (first (first (second result))))
    (let ((inner (first (cddr result))))
      (assert-eq 'flet (first inner))
      (assert-eq 'caller (first (first (second inner)))))))

(define-testcase validate-fix-labels-to-flet-end-to-end ()
  "Verify fix-labels-to-flet transforms a spurious LABELS form in memory.
Parses source from a string, inspects, invokes the maintainer, applies
the resolution to the string, and verifies LABELS is replaced by FLET."
  (let* ((source "(labels ((leaf (x) (* x 2)) (caller (items) (mapcar #'leaf items))) (caller data))")
         (cst-forms (atelier:parse-common-lisp source))
         (top-form (first cst-forms))
         (line-vector (atelier:string-to-line-vector source))
         (inspector (atelier:find-inspector 'atelier:check-labels-for-flet))
         (findings
           (let ((atelier::*current-pathname* #p"test.lisp")
                 (atelier::*current-line-vector* line-vector)
                 (atelier::*current-cst-root* top-form))
             (atelier:inspect-syntax inspector top-form)))
         (finding (first findings))
         (maintainer (atelier:find-maintainer 'atelier:fix-labels-to-flet))
         (resolution (atelier:prepare-resolution maintainer finding)))
    (assert-type resolution 'atelier:syntax-resolution)
    (assert-eq 'atelier:fix-labels-to-flet (atelier:resolution-maintainer resolution))
    (let ((result (atelier:apply-resolutions source (list resolution))))
      (assert-t (not (null (search "(flet" result :test #'char=))))
      (assert-t (null (search "(labels" result :test #'char=))))))

(define-testcase testsuite-fix-labels-to-flet ()
  (validate-fix-labels-to-flet-registered)
  (validate-compute-flet-depth-table-leaf)
  (validate-compute-flet-depth-table-caller)
  (validate-labels-transform-to-flet-simple)
  (validate-labels-transform-to-flet-chain)
  (validate-fix-labels-to-flet-end-to-end))

;;;; End of file `fix-labels-to-flet.lisp'
