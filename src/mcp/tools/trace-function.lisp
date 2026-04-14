;;;; trace-function.lisp — atelier:trace-function MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool trace-function (&key symbol-designator)
  (:description
   "Enable tracing for the function named by SYMBOL-DESIGNATOR in
    the child SBCL image. Once traced, subsequent eval-form calls
    exercising the function will include trace output in stdout.")
  (declare (type string symbol-designator))
  (unless *current-server*
    (error 'mcp-error :message "No server context for trace-function."))
  (let ((conn (ensure-child-connection *current-server*)))
    (let ((result (connection-eval
                   conn
                   (format nil "(atelier/child-worker:trace-function-data ~S)"
                           symbol-designator))))
      (read-from-string result))))

;;;; End of file `trace-function.lisp'
