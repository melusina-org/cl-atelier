;;;; macroexpand-form.lisp — lisp:macroexpand-form MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool macroexpand-form (&key form fully)
  (:description
   "Macroexpand a Common Lisp form in the child SBCL. By default
    performs a single expansion step (macroexpand-1). Set fully to
    true for full recursive expansion.")
  (declare (type string form))
  (unless *current-server*
    (error 'mcp-error :message "No server context for macroexpand-form."))
  (let ((conn (ensure-child-connection *current-server*)))
    (handler-case
        (let ((result (connection-eval
                       conn
                       (format nil "(atelier/child-worker:macroexpand-data ~S ~:[~;:fully t~])"
                               form fully))))
          (read-from-string result))
      (error (c)
        (list (cons "form" form)
              (cons "error" (format nil "~A" c)))))))

;;;; End of file `macroexpand-form.lisp'
