;;;; who-specializes.lisp — atelier:who-specializes MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool who-specializes (&key symbol-designator)
  (:description
   "Find all methods that specialize on the class named by
    SYMBOL-DESIGNATOR. Returns a list of methods with generic function
    name, qualifiers, and specializer lists.")
  (declare (type string symbol-designator))
  (unless *current-server*
    (error 'mcp-error :message "No server context for who-specializes."))
  (let ((conn (ensure-child-connection *current-server*)))
    (let ((result (connection-eval
                   conn
                   (format nil "(atelier/child-worker:who-specializes-data ~S)"
                           symbol-designator))))
      (read-from-string result))))

;;;; End of file `who-specializes.lisp'
