;;;; list-package-symbols.lisp — atelier:list-package-symbols MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool list-package-symbols (&key package-name)
  (:description
   "List external symbols in a package in the child SBCL image.
    Returns symbol descriptors with name, kind, and documentation.")
  (declare (type string package-name))
  (unless *current-server*
    (error 'mcp-error :message "No server context for list-package-symbols."))
  (let ((conn (ensure-child-connection *current-server*)))
    (let ((result (connection-eval
                   conn
                   (format nil "(atelier/child-worker:list-package-symbols-data ~S)"
                           package-name))))
      (read-from-string result))))

;;;; End of file `list-package-symbols.lisp'
