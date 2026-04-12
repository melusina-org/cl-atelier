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

(defvar *current-server* nil
  "The MCP server currently handling a request. Bound by the
   dispatcher during tool-call processing.")

(define-tool eval-form (&key form)
  (:description
   "Evaluate a Common Lisp form in an isolated child SBCL image.
    The child is created lazily on the first call and reused across
    calls within the session. Returns the evaluation result, any
    captured stdout/stderr, and the canonicalized form.")
  (declare (type string form))
  (unless *current-server*
    (error 'mcp-error :message "No server context for eval-form."))
  (let ((conn (ensure-child-connection *current-server*))
        (start (get-internal-real-time)))
    (multiple-value-bind (result output)
        (handler-case
            (connection-eval conn form)
          (error (c)
            (error 'mcp-error
                   :message (format nil "Eval error: ~A" c))))
      (let ((duration-ms (round (* 1000 (- (get-internal-real-time) start))
                                internal-time-units-per-second)))
        (list (cons "value" (or result ""))
              (cons "stdout" (or output ""))
              (cons "duration-ms" duration-ms))))))

;;;; End of file `eval-form.lisp'
