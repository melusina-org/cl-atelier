;;;; abort-debug.lisp — atelier:abort MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool abort-debug ()
  (:description
   "Abort the current debug session. Invokes the ABORT restart at
    the innermost (highest) debug level. The child returns to its
    toplevel and is ready for new evals.")
  (unless *current-server*
    (error 'mcp-error :message "No server context for abort."))
  (let* ((conn (ensure-child-connection *current-server*))
         (state (connection-debug-state conn)))
    (unless state
      (error 'mcp-error :message "No active debugger."))
    (let ((level (debug-state-level state))
          (condition-text (debug-state-condition state)))
      (handler-case
          (swank-invoke-restart (child-connection-swank-conn conn) level 0
                               :thread (debug-state-thread state))
        (stream-error (c)
          (declare (ignore c))
          (ignore-errors (swank-disconnect (child-connection-swank-conn conn)))
          (setf (child-connection-swank-conn conn) nil))
        (error (c)
          (declare (ignore c))
          nil))
      (setf (connection-debug-state conn) nil)
      (list (cons "aborted" t)
            (cons "condition" condition-text)))))

;;;; End of file `abort-debug.lisp'
