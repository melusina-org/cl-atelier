;;;; apropos-search.lisp — atelier:apropos-search MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool apropos-search (&key search-string package-name)
  (:description
   "Search for symbols matching a substring across all packages (or a
    specific package). Returns symbol name, package, kind, and whether
    it is external.")
  (declare (type string search-string))
  (unless *current-server*
    (error 'mcp-error :message "No server context for apropos-search."))
  (let ((conn (ensure-child-connection *current-server*)))
    (handler-case
        (let ((result (connection-eval
                       conn
                       (if (and package-name (not (equal package-name "")))
                           (format nil "(atelier/child-worker:apropos-data ~S ~S)"
                                   search-string package-name)
                           (format nil "(atelier/child-worker:apropos-data ~S)"
                                   search-string)))))
          (read-from-string result))
      (error (c)
        (list (cons "error" (format nil "~A" c)))))))

;;;; End of file `apropos-search.lisp'
