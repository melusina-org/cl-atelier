;;;; check-bare-lambda.lisp — Bare lambda in higher-order function inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)

(defparameter *higher-order-functions*
  '(mapcar mapcan mapc
    remove-if remove-if-not
    find-if find-if-not
    count-if count-if-not
    every some notany notevery
    reduce sort stable-sort)
  "Higher-order functions where a bare LAMBDA expression as the first argument
is flagged by CHECK-BARE-LAMBDA.")

(define-syntax-inspector check-bare-lambda (form)
  "Check that calls to MAPCAR and similar higher-order functions do not use
bare LAMBDA expressions as the function argument.
Walk the entire top-level FORM recursively, returning a list of
BARE-LAMBDA-FINDING instances for each violation found, or NIL."
  (let ((findings nil))
    (labels ((walk (node)
               (when (cst:consp node)
                 (let ((operator (cst:raw (cst:first node))))
                   (when (member operator *higher-order-functions*)
                     ;; Check whether the first argument is a bare LAMBDA form
                     (let* ((args (cst:rest node))
                            (first-arg (when (cst:consp args) (cst:first args))))
                       (when (and first-arg
                                  (cst:consp first-arg)
                                  (eq 'lambda (cst:raw (cst:first first-arg))))
                         (push (make-syntax-finding-from-form
                                first-arg 'bare-lambda-finding
                                :inspector 'check-bare-lambda
                                :severity :style
                                :observation (format nil
                                                     "~S receives a bare LAMBDA; use a named FLET function instead."
                                                     operator)
                                :rationale "Bare lambdas passed to higher-order functions obscure intent; use FLET with a descriptive name.")
                               findings)))))
                 ;; Recurse into both branches of the cons
                 (walk (cst:first node))
                 (walk (cst:rest node)))))
      (walk form))
    (nreverse findings)))

;;;; End of file `check-bare-lambda.lisp'
