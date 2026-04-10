;;;; list-inspectors.lisp — atelier:list-inspectors tool/resource

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(defun %inspector-summary (designator)
  "Return an alist describing the inspector named by DESIGNATOR."
  (let ((inspector (atelier:find-inspector designator)))
    (when inspector
      (list (cons "name"        (string-downcase
                                  (symbol-name (atelier:inspector-name inspector))))
            (cons "level"       (string-downcase
                                  (symbol-name (atelier:inspector-level inspector))))
            (cons "description" (or (atelier:inspector-description inspector) ""))))))

(define-tool list-inspectors ()
  (:description
   "List all registered Atelier inspectors with their level and description.")
  (:resource :uri       "atelier://inspectors"
             :name      "Atelier inspector registry"
             :mime-type :application/json)
  (mapcar #'%inspector-summary (atelier:list-inspectors)))

;;;; End of file `list-inspectors.lisp'
