;;;; fix-single-branch-if.lisp — IF to WHEN/UNLESS automatic maintainer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)

(define-automatic-maintainer fix-single-branch-if
    ((finding single-branch-if-finding))
  "Replace single-branch IF with WHEN or UNLESS.
- (IF TEST THEN) or (IF TEST THEN NIL) becomes (WHEN TEST THEN).
- (IF TEST NIL ELSE) becomes (UNLESS TEST ELSE).
Returns a SYNTAX-RESOLUTION with the appropriate transform."
  (make-syntax-resolution
   :maintainer 'fix-single-branch-if
   :finding finding
   :kind :automatic
   :description "Replace single-branch IF with WHEN or UNLESS."
   :transform (lambda (raw-form)
                (let* ((test (second raw-form))
                       (then-branch (third raw-form))
                       (else-branch (fourth raw-form)))
                  (cond
                    ;; (IF TEST NIL ELSE) → (UNLESS TEST ELSE)
                    ((null then-branch)
                     `(unless ,test ,else-branch))
                    ;; (IF TEST THEN) or (IF TEST THEN NIL) → (WHEN TEST THEN)
                    (t
                     `(when ,test ,then-branch)))))))

;;;; End of file `fix-single-branch-if.lisp'
