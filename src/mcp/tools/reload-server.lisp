;;;; reload-server.lisp — atelier:reload-server MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp-kernel)

(define-tool reload-server ()
  (:description
   "Reload the MCP server code from disk. Re-loads all ASDF systems
    that make up the server (atelier, editor, child-worker, mcp).
    New and modified tools become available immediately. The session's
    child connection is preserved — only the parent image is updated.
    Use after editing source files to pick up changes without
    rebuilding the binary or reconnecting.")
  (let ((results nil)
        (errors nil))
    (flet ((reload-one (system-name)
             (handler-case
                 (progn
                   (asdf:load-system system-name :force t)
                   (push (cons system-name "ok") results))
               (error (c)
                 (push (cons system-name (princ-to-string c)) errors)))))
      (reload-one "org.melusina.atelier")
      (reload-one "org.melusina.atelier/editor")
      (reload-one "org.melusina.atelier/child-worker")
      (reload-one "org.melusina.atelier/mcp"))
    (list (cons "reloaded" (mapcar #'car (reverse results)))
          (cons "tool-count" (hash-table-count *tool-registry*))
          (cons "errors" (if errors
                             (mapcar (lambda (e)
                                       (list (cons "system" (car e))
                                             (cons "error" (cdr e))))
                                     (reverse errors))
                             nil)))))

;;;; End of file `reload-server.lisp'
