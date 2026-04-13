;;;; system-apropos.lisp — atelier:system-apropos MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool system-apropos (&key search-string)
  (:description
   "Search for ASDF systems whose names contain SEARCH-STRING.
    Searches both the source registry and registered systems.
    No child image required.")
  (declare (type string search-string))
  (let ((search-down (string-downcase search-string))
        (result nil))
    ;; Search source registry
    (asdf/source-registry:ensure-source-registry)
    (let ((registry (symbol-value
                     (find-symbol "*SOURCE-REGISTRY*" :asdf/source-registry))))
      (when (hash-table-p registry)
        (loop :for name :being :the :hash-keys :of registry
              :do (when (search search-down (string-downcase name))
                    (pushnew name result :test #'string=)))))
    ;; Also search registered (already loaded) systems
    (dolist (name (asdf:registered-systems))
      (when (search search-down (string-downcase name))
        (pushnew name result :test #'string=)))
    (sort result #'string<)))

;;;; End of file `system-apropos.lisp'
