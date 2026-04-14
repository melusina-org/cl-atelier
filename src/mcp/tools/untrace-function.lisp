;;;; untrace-function.lisp — atelier:untrace-function MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool untrace-function (&key symbol-designator)
  (:description
   "Remove tracing from the function named by SYMBOL-DESIGNATOR in
    the child SBCL image.")
  (declare (type string symbol-designator))
  (unless *current-server*
    (error 'mcp-error :message "No server context for untrace-function."))
  (let ((conn (ensure-child-connection *current-server*)))
    (let ((result (connection-eval
                   conn
                   (format nil "(atelier/child-worker:untrace-function-data ~S)"
                           symbol-designator))))
      (read-from-string result))))

;;;; End of file `untrace-function.lisp'
