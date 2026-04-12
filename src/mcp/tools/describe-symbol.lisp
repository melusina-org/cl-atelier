;;;; describe-symbol.lisp — atelier:describe-symbol MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool describe-symbol (&key symbol-designator)
  (:description
   "Describe a symbol in the child SBCL image. Returns function
    signature, class slots, generic methods, and documentation.
    SYMBOL-DESIGNATOR is a string like \"cl:car\" or \"alexandria:flatten\".")
  (declare (type string symbol-designator))
  (unless *current-server*
    (error 'mcp-error :message "No server context for describe-symbol."))
  (let ((conn (ensure-child-connection *current-server*)))
    (let ((result (connection-eval
                   conn
                   (format nil "(atelier/child-worker:describe-symbol-data ~S)"
                           symbol-designator))))
      (read-from-string result))))

;;;; End of file `describe-symbol.lisp'
