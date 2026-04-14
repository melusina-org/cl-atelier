;;;; inspect-class.lisp — atelier:inspect-class MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool inspect-class (&key symbol-designator)
  (:description
   "Inspect a CLOS class in detail. Returns superclasses, subclasses,
    slots (with readers, writers, initargs, type, initform), and direct
    methods. SYMBOL-DESIGNATOR is a string like \"cl:hash-table\" or
    \"atelier:finding\".")
  (declare (type string symbol-designator))
  (unless *current-server*
    (error 'mcp-error :message "No server context for inspect-class."))
  (let ((conn (ensure-child-connection *current-server*)))
    (let ((result (connection-eval
                   conn
                   (format nil "(atelier/child-worker:inspect-class-data ~S)"
                           symbol-designator))))
      (read-from-string result))))

;;;; End of file `inspect-class.lisp'
