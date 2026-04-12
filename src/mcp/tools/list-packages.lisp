;;;; list-packages.lisp — atelier:list-packages MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool list-packages ()
  (:description
   "List all packages in the child SBCL image with their names,
    nicknames, use lists, and symbol counts.")
  (unless *current-server*
    (error 'mcp-error :message "No server context for list-packages."))
  (let ((conn (ensure-child-connection *current-server*)))
    (let ((result (connection-eval conn "(atelier/child-worker:list-packages-data)")))
      (read-from-string result))))

;;;; End of file `list-packages.lisp'
