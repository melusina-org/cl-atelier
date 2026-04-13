;;;; eval-form.lisp — atelier:eval-form MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

;;; The eval-form tool evaluates a form in the session's child SBCL
;;; image via SWANK. The child is created lazily on first call and
;;; reused across calls within the session.
;;;
;;; Slice 011: when the eval enters the debugger, the debug state
;;; (condition, restarts, backtrace) is returned instead of auto-aborting.
;;; The agent can then call select-restart, abort-debug, backtrace, or
;;; eval-in-frame to interact with the debugger.
;;; Optional timeout parameter triggers interrupt after N seconds.

(defvar *current-server* nil
  "The MCP server currently handling a request. Bound by the
   dispatcher during tool-call processing.")

(define-tool eval-form (&key form timeout)
  (:description
   "Evaluate a Common Lisp form in an isolated child SBCL image.
    The child is created lazily on the first call and reused across
    calls within the session. Returns the evaluation result, any
    captured stdout/stderr, and duration. When the eval enters the
    debugger, returns the debug state (condition, restarts, backtrace)
    instead of an error — use select-restart or abort-debug to continue.
    Optional timeout in seconds; when exceeded, the eval is interrupted
    and the debug state is returned.")
  (declare (type string form))
  (unless *current-server*
    (error 'mcp-error :message "No server context for eval-form."))
  (let ((conn (ensure-child-connection *current-server*)))
    ;; Reject if debugger is already active
    (when (connection-debug-state conn)
      (error 'debugger-active))
    (let* ((start (get-internal-real-time))
           (timeout-secs (when (and timeout (numberp timeout) (plusp timeout))
                           timeout)))
      (multiple-value-bind (status result output)
          (handler-case
              (swank-eval (child-connection-swank-conn conn) form
                          :timeout timeout-secs)
            (error (c)
              (error 'mcp-error
                     :message (format nil "Eval error: ~A" c))))
        (let ((duration-ms (round (* 1000 (- (get-internal-real-time) start))
                                  internal-time-units-per-second)))
          (case status
            (:ok
             ;; Normal completion — same shape as slice 010
             (list (cons "value" (or result ""))
                   (cons "stdout" (or output ""))
                   (cons "duration-ms" duration-ms)))
            (:debug
             ;; Debugger entered — store state on connection and return it
             (setf (connection-debug-state conn) result)
             (list (cons "in_debugger" t)
                   (cons "condition" (debug-state-condition result))
                   (cons "restarts" (debug-state-restarts result))
                   (cons "backtrace" (debug-state-backtrace result))
                   (cons "level" (debug-state-level result))
                   (cons "duration-ms" duration-ms)))))))))

;;;; End of file `eval-form.lisp'
