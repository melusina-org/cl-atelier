;;;; backtrace.lisp — atelier:backtrace MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool backtrace-frames (&key start end)
  (:description
   "Get backtrace frames from the current debug session. START and END
    control pagination (default 0..50). Each frame includes index and
    description. Use after eval-form returns a debug state.")
  (declare (type integer start)
           (type integer end))
  (unless *current-server*
    (error 'mcp-error :message "No server context for backtrace."))
  (let* ((conn (ensure-child-connection *current-server*))
         (state (connection-debug-state conn)))
    (unless state
      (error 'mcp-error :message "No active debugger."))
    (handler-case
        (let ((frames (swank-backtrace-frames
                       (child-connection-swank-conn conn)
                       (or start 0)
                       (or end 50)
                       :thread (debug-state-thread state))))
          (list (cons "frames" frames)
                (cons "level" (debug-state-level state))))
      (stream-error (c)
        (ignore-errors (swank-disconnect (child-connection-swank-conn conn)))
        (setf (child-connection-swank-conn conn) nil)
        (setf (connection-debug-state conn) nil)
        (error 'mcp-error
               :message (format nil "SWANK connection lost, child will respawn: ~A" c)))
      (error (c)
        (error 'mcp-error
               :message (format nil "Backtrace error: ~A" c))))))

;;;; End of file `backtrace.lisp'
