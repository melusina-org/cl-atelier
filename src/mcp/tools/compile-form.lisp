;;;; compile-form.lisp — lisp:compile-form MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool compile-form (&key form)
  (:description
   "Compile a Common Lisp form in the child SBCL and return compiler
    diagnostics (notes, warnings, errors). Uses (compile nil (lambda ()
    FORM)) to avoid side effects in the child image.")
  (declare (type string form))
  (unless *current-server*
    (error 'mcp-error :message "No server context for compile-form."))
  (let ((conn (ensure-child-connection *current-server*)))
    (handler-case
        (let ((result (connection-eval
                       conn
                       (format nil "(atelier/child-worker:compile-form-data ~S)"
                               form))))
          (read-from-string result))
      (error (c)
        (list (cons "form" form)
              (cons "error" (format nil "~A" c)))))))

;;;; End of file `compile-form.lisp'
