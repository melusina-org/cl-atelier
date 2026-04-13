;;;; run-testcase.lisp — atelier:run-testcase MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool run-testcase (&key testcase-designator)
  (:description
   "Run a specific Confidence testcase by name (e.g.
    \"atelier/testsuite:validate-parameter-replace\"). Returns the
    test output. The testcase must be loaded in the child image first.")
  (declare (type string testcase-designator))
  (unless *current-server*
    (error 'mcp-error :message "No server context for run-testcase."))
  (let ((conn (ensure-child-connection *current-server*)))
    (handler-case
        (let ((result (connection-eval
                       conn
                       (format nil "(atelier/child-worker:run-testcase-data ~S)"
                               testcase-designator))))
          (read-from-string result))
      (error (c)
        (error 'mcp-error
               :message (format nil "run-testcase error: ~A" c))))))

;;;; End of file `run-testcase.lisp'
