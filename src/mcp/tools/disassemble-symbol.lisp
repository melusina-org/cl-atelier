;;;; disassemble-symbol.lisp — lisp:disassemble-symbol MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool disassemble-symbol (&key symbol-designator)
  (:description
   "Disassemble a function in the child SBCL. Returns the machine
    code disassembly as text. SYMBOL-DESIGNATOR is a string like
    \"cl:car\" or \"alexandria:flatten\".")
  (declare (type string symbol-designator))
  (unless *current-server*
    (error 'mcp-error :message "No server context for disassemble-symbol."))
  (let ((conn (ensure-child-connection *current-server*)))
    (handler-case
        (let ((result (connection-eval
                       conn
                       (format nil "(atelier/child-worker:disassemble-data ~S)"
                               symbol-designator))))
          (read-from-string result))
      (error (c)
        (list (cons "symbol" symbol-designator)
              (cons "error" (format nil "~A" c)))))))

;;;; End of file `disassemble-symbol.lisp'
