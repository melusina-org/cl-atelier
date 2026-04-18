;;;; check-single-branch-if.lisp — IF to WHEN/UNLESS inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)

(define-syntax-inspector check-single-branch-if (form)
  "Check that IF forms do not have a NIL branch. Walk the entire top-level
FORM recursively. For each (IF TEST THEN) or (IF TEST THEN NIL), report a
SINGLE-BRANCH-IF-FINDING suggesting WHEN. For each (IF TEST NIL ELSE),
report a finding suggesting UNLESS. Forms (IF TEST THEN ELSE) where
neither branch is NIL are not reported."
  (let ((findings nil))
    (labels ((nil-branch-p (branch-cst)
               ;; A branch is "NIL" if its raw form is the symbol NIL.
               (and branch-cst (null (cst:raw branch-cst))))
             (report (node observation)
               (push (make-syntax-finding-from-form
                      node 'single-branch-if-finding
                      :inspector 'check-single-branch-if
                      :severity :style
                      :observation observation
                      :rationale "IF with a NIL branch is conventionally written as WHEN or UNLESS to signal the single-branch intent at a glance.")
                     findings))
             (check-if (node)
               ;; NODE is known to be a CST cons whose first element is IF.
               (let* ((args (cst:rest node))
                      (has-test (cst:consp args))
                      (after-test (when has-test (cst:rest args)))
                      (then-cst (when (and after-test (cst:consp after-test))
                                  (cst:first after-test)))
                      (after-then (when after-test (cst:rest after-test)))
                      (has-else (and after-then (cst:consp after-then)))
                      (else-cst (when has-else (cst:first after-then))))
                 (cond
                   ;; (IF TEST THEN) — implicit NIL else → WHEN.
                   ((and then-cst (not has-else))
                    (report node "IF form has an implicit NIL else branch; use WHEN."))
                   ;; (IF TEST THEN NIL) — explicit NIL else → WHEN.
                   ((and then-cst has-else (nil-branch-p else-cst)
                         (not (nil-branch-p then-cst)))
                    (report node "IF form has a NIL else branch; use WHEN."))
                   ;; (IF TEST NIL ELSE) — NIL then → UNLESS.
                   ((and then-cst has-else (nil-branch-p then-cst)
                         (not (nil-branch-p else-cst)))
                    (report node "IF form has a NIL then branch; use UNLESS.")))))
             (walk (node)
               (when (cst:consp node)
                 (when (eq 'if (cst:raw (cst:first node)))
                   (check-if node))
                 (loop :for tail = node :then (cst:rest tail)
                       :while (cst:consp tail)
                       :do (walk (cst:first tail))))))
      (walk form))
    (nreverse findings)))

;;;; End of file `check-single-branch-if.lisp'
