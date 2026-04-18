;;;; check-when-not.lisp — WHEN-NOT to UNLESS inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)

(define-syntax-inspector check-when-not (form)
  "Check that WHEN forms do not test (NOT X). Walk the entire top-level
FORM recursively. For each (WHEN (NOT X) BODY...), report a WHEN-NOT-FINDING
suggesting (UNLESS X BODY...)."
  (let ((findings nil))
    (labels ((negated-test-p (when-node)
               ;; WHEN-NODE is known to be a CST cons whose first element is WHEN.
               ;; Return T if its test form is (NOT X).
               (let ((after-when (cst:rest when-node)))
                 (when (cst:consp after-when)
                   (let ((test-cst (cst:first after-when)))
                     (and (cst:consp test-cst)
                          (eq 'not (cst:raw (cst:first test-cst))))))))
             (walk (node)
               (when (cst:consp node)
                 (when (and (eq 'when (cst:raw (cst:first node)))
                            (negated-test-p node))
                   (push (make-syntax-finding-from-form
                          node 'when-not-finding
                          :inspector 'check-when-not
                          :severity :style
                          :observation "WHEN form tests (NOT X); use UNLESS instead."
                          :rationale "UNLESS expresses the negated guard directly; WHEN (NOT X) reads as a double negation.")
                         findings))
                 (loop :for tail = node :then (cst:rest tail)
                       :while (cst:consp tail)
                       :do (walk (cst:first tail))))))
      (walk form))
    (nreverse findings)))

;;;; End of file `check-when-not.lisp'
