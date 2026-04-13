;;;; list-testcases.lisp — atelier:list-testcases MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool list-testcases (&key package-name)
  (:description
   "List Confidence testcases defined in PACKAGE-NAME. Returns an array
    of testcase names with documentation. The package must be loaded
    in the child image first.")
  (declare (type string package-name))
  (unless *current-server*
    (error 'mcp-error :message "No server context for list-testcases."))
  (let ((conn (ensure-child-connection *current-server*)))
    (handler-case
        (let ((result (connection-eval
                       conn
                       (format nil "(atelier/child-worker:list-testcases-data ~S)"
                               package-name))))
          (read-from-string result))
      (error (c)
        (error 'mcp-error
               :message (format nil "list-testcases error: ~A" c))))))

;;;; End of file `list-testcases.lisp'
