;;;; run-impacted.lisp — atelier:run-impacted MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool run-impacted (&key symbol-designator)
  (:description
   "Discover testcases impacted by the function named by
    SYMBOL-DESIGNATOR via who-tests, run each, and return per-testcase
    pass/fail results. Use this to quickly verify a change without
    running the full test suite.")
  (declare (type string symbol-designator))
  (unless *current-server*
    (error 'mcp-error :message "No server context for run-impacted."))
  (let ((conn (ensure-child-connection *current-server*)))
    (let ((result (connection-eval
                   conn
                   (format nil "(atelier/child-worker:run-impacted-data ~S)"
                           symbol-designator))))
      (read-from-string result))))

;;;; End of file `run-impacted.lisp'
