;;;; who-macroexpands.lisp — atelier:who-macroexpands MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool who-macroexpands (&key symbol-designator)
  (:description
   "Find all functions that expand the macro named by
    SYMBOL-DESIGNATOR. Returns a list of expansion sites with source
    file and form path.")
  (declare (type string symbol-designator))
  (unless *current-server*
    (error 'mcp-error :message "No server context for who-macroexpands."))
  (let ((conn (ensure-child-connection *current-server*)))
    (let ((result (connection-eval
                   conn
                   (format nil "(atelier/child-worker:who-macroexpands-data ~S)"
                           symbol-designator))))
      (read-from-string result))))

;;;; End of file `who-macroexpands.lisp'
