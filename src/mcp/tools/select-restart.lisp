;;;; select-restart.lisp — atelier:select-restart MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool select-restart (&key index level)
  (:description
   "Invoke a restart at the given debug level. Use after eval-form
    returns a debug state. INDEX is the restart number (from the
    restarts array), LEVEL is the debug level. If the restart resolves
    the error, returns the result. If it causes a new error, returns
    a new debug state. If it aborts, returns an aborted indicator.")
  (declare (type integer index)
           (type integer level))
  (unless *current-server*
    (error 'mcp-error :message "No server context for select-restart."))
  (let* ((conn (ensure-child-connection *current-server*))
         (state (connection-debug-state conn)))
    (unless state
      (error 'mcp-error :message "No active debugger."))
    ;; Validate restart index
    (let ((restart-count (length (debug-state-restarts state))))
      (when (or (minusp index) (>= index restart-count))
        (error 'mcp-error
               :message (format nil "Restart index ~D out of range (0..~D)."
                                index (1- restart-count)))))
    (multiple-value-bind (status result output)
        (handler-case
            (swank-invoke-restart (child-connection-swank-conn conn) level index
                                 :thread (debug-state-thread state))
          (stream-error (c)
            (ignore-errors (swank-disconnect (child-connection-swank-conn conn)))
            (setf (child-connection-swank-conn conn) nil)
            (setf (connection-debug-state conn) nil)
            (error 'mcp-error
                   :message (format nil "SWANK connection lost, child will respawn: ~A" c)))
          (error (c)
            (setf (connection-debug-state conn) nil)
            (error 'mcp-error
                   :message (format nil "Restart error: ~A" c))))
      (declare (ignore output))
      (case status
        (:aborted
         ;; Abort clears the debug state
         (let ((condition-text (debug-state-condition state)))
           (setf (connection-debug-state conn) nil)
           (list (cons "aborted" t)
                 (cons "condition" condition-text))))
        (:debug
         ;; New debugger level — update the stored state
         (setf (connection-debug-state conn) result)
         (list (cons "in_debugger" t)
               (cons "condition" (debug-state-condition result))
               (cons "restarts" (debug-state-restarts result))
               (cons "backtrace" (debug-state-backtrace result))
               (cons "level" (debug-state-level result))))
        (:ok
         ;; Restart resolved the error — eval completed
         (setf (connection-debug-state conn) nil)
         (list (cons "value" (or result ""))
               (cons "stdout" "")))))))

;;;; End of file `select-restart.lisp'
