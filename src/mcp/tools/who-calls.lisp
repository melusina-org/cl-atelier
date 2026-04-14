;;;; who-calls.lisp — atelier:who-calls MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool who-calls (&key symbol-designator)
  (:description
   "Find all functions that call the function named by SYMBOL-DESIGNATOR.
    Returns a list of callers with source file and form path.
    SYMBOL-DESIGNATOR is a string like \"cl:car\" or \"atelier:lint-system\".")
  (declare (type string symbol-designator))
  (unless *current-server*
    (error 'mcp-error :message "No server context for who-calls."))
  (let ((conn (ensure-child-connection *current-server*)))
    (let ((result (connection-eval
                   conn
                   (format nil "(atelier/child-worker:who-calls-data ~S)"
                           symbol-designator))))
      (read-from-string result))))

;;;; End of file `who-calls.lisp'
