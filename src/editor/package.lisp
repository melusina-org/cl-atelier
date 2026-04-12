;;;; package.lisp — Package definition for the Atelier projectional editor

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(defpackage #:atelier/editor
  (:use #:cl)
  (:export
   ;; Class + constructor + readers
   #:toplevel-form
   #:make-toplevel-form
   #:toplevel-form-kind
   #:toplevel-form-name
   #:toplevel-form-body
   #:toplevel-form-eval-when
   #:toplevel-form-source-text
   #:toplevel-form-ast

   ;; Read / Write
   #:read-toplevel-form-from-string
   #:write-toplevel-form-to-string

   ;; Pipeline
   #:normalize-toplevel-form

   ;; Conditions
   #:unexpected-toplevel-form
   #:unexpected-toplevel-form-source
   #:unexpected-toplevel-form-reason

   ;; Restart name
   #:decompose)
  (:documentation "Projectional editor for managed Common Lisp files.
Agents write semantic forms; the editor writes files. The canonical
representation is an Eclector CST preserving reader conditionals.
See product/reference/projectional-editor-design.md."))

;;;; End of file `package.lisp'
