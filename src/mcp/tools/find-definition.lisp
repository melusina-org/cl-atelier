;;;; find-definition.lisp — atelier:find-definition MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool find-definition (&key symbol-designator)
  (:description
   "Find the source location of a symbol's definition in the child
    SBCL image. Returns source-file and form-path, or null if the
    source location is not available.")
  (declare (type string symbol-designator))
  (unless *current-server*
    (error 'mcp-error :message "No server context for find-definition."))
  (let ((conn (ensure-child-connection *current-server*)))
    (let ((result (connection-eval
                   conn
                   (format nil "(atelier/child-worker:find-definition-data ~S)"
                           symbol-designator))))
      (let ((data (read-from-string result)))
        (or data 'cl:null)))))

;;;; End of file `find-definition.lisp'
