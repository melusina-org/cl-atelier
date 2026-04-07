;;;; check-constant-naming.lisp — Constant naming convention inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:atelier)

(define-syntax-inspector check-constant-naming (form)
  "Check that DEFCONSTANT constant names use the +plus-surrounded+ convention.
Return a list containing a CONSTANT-NAMING-FINDING when a constant name
lacks the surrounding plus signs, or NIL."
  (when (and (cst:consp form)
             (eq 'defconstant (cst:raw (cst:first form))))
    (let* ((rest (cst:rest form))
           (name-cst (when (cst:consp rest) (cst:first rest)))
           (name (when name-cst (cst:raw name-cst))))
      (when (and (symbolp name)
                 (flet ((has-plus-convention-p (name-str)
                          (and (> (length name-str) 2)
                               (char= (char name-str 0) #\+)
                               (char= (char name-str (1- (length name-str))) #\+))))
                   (not (has-plus-convention-p (symbol-name name)))))
        (list (make-syntax-finding-from-form
               name-cst 'constant-naming-finding
               :inspector 'check-constant-naming
               :severity :style
               :observation (format nil "~S lacks +plus-surrounded+ convention." name)
               :rationale "Constants should be named +like-this+ to be visually distinct from variables."))))))

;;;; End of file `check-constant-naming.lisp'
