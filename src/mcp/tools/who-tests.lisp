;;;; who-tests.lisp — atelier:who-tests MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool who-tests (&key symbol-designator)
  (:description
   "Find Confidence testcases that directly call the function named
    by SYMBOL-DESIGNATOR. Returns a list of testcase names. This is
    the differentiator: it filters xref callers to those carrying the
    Confidence testcase property.")
  (declare (type string symbol-designator))
  (unless *current-server*
    (error 'mcp-error :message "No server context for who-tests."))
  (let ((conn (ensure-child-connection *current-server*)))
    (let ((result (connection-eval
                   conn
                   (format nil "(atelier/child-worker:who-tests-data ~S)"
                           symbol-designator))))
      (read-from-string result))))

;;;; End of file `who-tests.lisp'
