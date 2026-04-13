;;;; system-info.lisp — atelier:system-info MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool system-info (&key system-name)
  (:description
   "Get detailed information about an ASDF system: dependencies,
    components, version, author, license, and source directory.
    The system must be loaded in the child image first.")
  (declare (type string system-name))
  (unless *current-server*
    (error 'mcp-error :message "No server context for system-info."))
  (let ((conn (ensure-child-connection *current-server*)))
    (handler-case
        (let ((result (connection-eval
                       conn
                       (format nil "(atelier/child-worker:system-info-data ~S)"
                               system-name))))
          (read-from-string result))
      (error (c)
        (error 'mcp-error
               :message (format nil "system-info error: ~A" c))))))

;;;; End of file `system-info.lisp'
