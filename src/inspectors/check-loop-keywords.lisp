;;;; check-loop-keywords.lisp — Bare loop keyword inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)

(defparameter *loop-clause-keywords*
  '(for in across collect do when unless while until return
    finally with repeat initially append nconc count sum maximize minimize)
  "Bare symbols that are LOOP clause keywords and should instead be keyword
symbols (e.g. :FOR, :IN, :COLLECT) per the project style convention.")

(define-syntax-inspector check-loop-keywords (form)
  "Check that LOOP forms use keyword symbols for clause keywords.
Walk the entire top-level FORM recursively. For each LOOP form found,
check every clause keyword position for bare symbols that should be
keyword symbols. Return a list of BARE-LOOP-KEYWORD-FINDING instances, or NIL."
  (let ((findings nil))
    (labels ((check-loop-body (node)
               ;; NODE is the CDR of the LOOP form — iterate over its elements.
               (loop :while (cst:consp node)
                     :for element = (cst:first node)
                     :when (and (typep element 'concrete-syntax-tree:atom-cst)
                                (symbolp (cst:raw element))
                                (not (keywordp (cst:raw element)))
                                (member (cst:raw element) *loop-clause-keywords*
                                        :test #'string-equal))
                       :do (push (make-syntax-finding-from-form
                                  element 'bare-loop-keyword-finding
                                  :inspector 'check-loop-keywords
                                  :severity :style
                                  :observation (format nil
                                                       "LOOP clause keyword ~S should be ~S."
                                                       (cst:raw element)
                                                       (intern (symbol-name (cst:raw element))
                                                               :keyword))
                                  :rationale "LOOP clause keywords should be keyword symbols (:FOR, :IN, :COLLECT) for consistency and portability.")
                                 findings)
                     :do (setf node (cst:rest node))))
             (walk (node)
               (when (cst:consp node)
                 (when (eq 'loop (cst:raw (cst:first node)))
                   (check-loop-body (cst:rest node)))
                 ;; Recurse to find nested LOOP forms
                 (walk (cst:first node))
                 (walk (cst:rest node)))))
      (walk form))
    (nreverse findings)))

;;;; End of file `check-loop-keywords.lisp'
