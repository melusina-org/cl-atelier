;;;; check-earmuffs.lisp — Earmuffs naming convention inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)

(define-syntax-inspector check-earmuffs (form)
  "Check that DEFVAR and DEFPARAMETER variable names use *earmuffs* convention.
Return a list containing an EARMUFFS-FINDING when a special variable name
lacks the surrounding asterisks, or NIL."
  (when (and (cst:consp form)
             (member (cst:raw (cst:first form)) '(defvar defparameter)))
    (let* ((rest (cst:rest form))
           (name-cst (when (cst:consp rest) (cst:first rest)))
           (name (when name-cst (cst:raw name-cst))))
      (when (and (symbolp name)
                 (flet ((has-earmuffs-p (name-str)
                          (and (> (length name-str) 2)
                               (char= (char name-str 0) #\*)
                               (char= (char name-str (1- (length name-str))) #\*))))
                   (not (has-earmuffs-p (symbol-name name)))))
        (list (make-syntax-finding-from-form
               name-cst 'earmuffs-finding
               :inspector 'check-earmuffs
               :severity :style
               :observation (format nil "~S lacks *earmuffs* convention." name)
               :rationale "Special variables should be named *like-this* to be visually distinct from lexical variables."))))))

;;;; End of file `check-earmuffs.lisp'
