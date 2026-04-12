;;;; canonicalize-form.lisp — atelier:canonicalize-form MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

;;; The canonicalize-form tool runs entirely in the parent MCP image.
;;; No child connection is created or used. It calls the editor's
;;; normalize-toplevel-form pipeline and returns the result as JSON.

(defun %finding-to-alist (finding)
  "Convert a finding to an alist for JSON serialization."
  (list (cons "description" (princ-to-string finding))))

(define-tool canonicalize-form (&key form)
  (:description
   "Canonicalize a toplevel Common Lisp form: pretty-print it and
    apply Atelier maintainers (earmuffs, loop keywords, bare lambda, etc.).
    Returns the canonicalized form and any findings raised during the process.
    Runs in the parent image — no child SBCL is needed.")
  (declare (type string form))
  (handler-case
      (let ((toplevel (atelier/editor:read-toplevel-form-from-string form)))
        (multiple-value-bind (normalized findings)
            (atelier/editor:normalize-toplevel-form toplevel)
          (list (cons "canonicalized"
                      (atelier/editor:write-toplevel-form-to-string normalized))
                (cons "findings"
                      (mapcar #'%finding-to-alist findings)))))
    (atelier/editor:unexpected-toplevel-form (c)
      (error 'mcp-error
             :message (format nil "Forbidden toplevel form: ~A" c)))))

;;;; End of file `canonicalize-form.lisp'
