;;;; check-single-form-progn.lisp — Single-form PROGN inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)

(define-syntax-inspector check-single-form-progn (form)
  "Check that PROGN forms do not wrap a single body form. Walk the entire
top-level FORM recursively. For each (PROGN BODY) with exactly one body
form, report a SINGLE-FORM-PROGN-FINDING. (PROGN) and (PROGN A B ...) are
not reported; an empty PROGN is a separate concern."
  (let ((findings nil))
    (labels ((splicing-unquote-p (cst-node)
               ;; Return T if CST-NODE is (ECLECTOR.READER:UNQUOTE-SPLICING ...).
               ;; Such a form may expand to multiple forms at macro-expansion
               ;; time, so (PROGN ,@BODY) is not a single-form PROGN.
               (and (cst:consp cst-node)
                    (eq 'eclector.reader:unquote-splicing
                        (cst:raw (cst:first cst-node)))))
             (single-body-p (progn-node)
               ;; PROGN-NODE is known to be a CST cons whose first element is PROGN.
               ;; There must be exactly one body form — i.e., (cst:rest progn-node)
               ;; is a non-empty cons whose own rest is the empty-list terminator.
               ;; Additionally, the single form must not be a splicing unquote.
               (let ((body (cst:rest progn-node)))
                 (and (cst:consp body)
                      (not (cst:consp (cst:rest body)))
                      (not (splicing-unquote-p (cst:first body))))))
             (walk (node)
               (when (cst:consp node)
                 (when (and (eq 'progn (cst:raw (cst:first node)))
                            (single-body-p node))
                   (push (make-syntax-finding-from-form
                          node 'single-form-progn-finding
                          :inspector 'check-single-form-progn
                          :severity :style
                          :observation "PROGN wraps a single body form; the wrapper is redundant."
                          :rationale "A PROGN with one body form adds no sequencing and no scope; the body form can stand alone.")
                         findings))
                 (walk (cst:first node))
                 (walk (cst:rest node)))))
      (walk form))
    (nreverse findings)))

;;;; End of file `check-single-form-progn.lisp'
