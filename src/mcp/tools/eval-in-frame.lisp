;;;; eval-in-frame.lisp — atelier:eval-in-frame MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool eval-in-frame (&key expression frame-index)
  (:description
   "Evaluate an expression in the context of a specific backtrace frame.
    The frame's local variables are visible to the expression. Use after
    eval-form returns a debug state to inspect the state at the error point.")
  (declare (type string expression)
           (type integer frame-index))
  (unless *current-server*
    (error 'mcp-error :message "No server context for eval-in-frame."))
  (let* ((conn (ensure-child-connection *current-server*))
         (state (connection-debug-state conn)))
    (unless state
      (error 'mcp-error :message "No active debugger."))
    (handler-case
        (let ((result (swank-eval-in-frame
                       (child-connection-swank-conn conn)
                       expression frame-index
                       :thread (debug-state-thread state))))
          (list (cons "value" (or result ""))))
      (error (c)
        (error 'mcp-error
               :message (format nil "Eval-in-frame error: ~A" c))))))

;;;; End of file `eval-in-frame.lisp'
