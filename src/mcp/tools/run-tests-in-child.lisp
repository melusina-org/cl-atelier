;;;; run-tests-in-child.lisp — atelier:run-tests-in-child MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

;;; run-tests-in-child evaluates the test suite in the session's
;;; existing child image. Faster than run-tests-fresh (no cold start)
;;; but non-hermetic (carries compiled state from prior evals).

(define-tool run-tests-in-child (&key system-name)
  (:description
   "Run a system's test suite in the session's existing child SBCL
    image. Faster than run-tests-fresh but non-hermetic: the child
    carries state from prior evaluations.")
  (declare (type string system-name))
  (unless *current-server*
    (error 'mcp-error :message "No server context for run-tests-in-child."))
  (let ((conn (ensure-child-connection *current-server*)))
    (let ((result (connection-eval
                   conn
                   (format nil "(atelier/child-worker:run-testsuite-data ~S)"
                           system-name))))
      (read-from-string result))))

;;;; End of file `run-tests-in-child.lisp'
