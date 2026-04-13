;;;; quickload.lisp — atelier:quickload MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool quickload (&key system-name)
  (:description
   "Load a system in the child image via Quicklisp. Handles missing
    dependencies automatically. Returns success status, output, and
    duration.")
  (declare (type string system-name))
  (unless *current-server*
    (error 'mcp-error :message "No server context for quickload."))
  (let ((conn (ensure-child-connection *current-server*)))
    (handler-case
        (let ((result (connection-eval
                       conn
                       (format nil "(atelier/child-worker:quickload-data ~S)"
                               system-name))))
          (read-from-string result))
      (error (c)
        (list (cons "system" system-name)
              (cons "success" nil)
              (cons "output" (format nil "~A" c))
              (cons "duration-ms" 0))))))

;;;; End of file `quickload.lisp'
