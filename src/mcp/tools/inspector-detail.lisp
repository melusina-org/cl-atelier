;;;; inspector-detail.lisp — atelier:inspector-detail tool/resource

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(defun %intern-inspector-name (name)
  "Resolve NAME (a string) to the corresponding inspector symbol in
   the ATELIER package, or NIL if not interned."
  (find-symbol (string-upcase name) (find-package :atelier)))

(defun %inspector-detail (name)
  "Return a detailed alist describing the inspector NAME (a string),
   or signal RESOURCE-NOT-FOUND when no such inspector exists."
  (let* ((symbol    (%intern-inspector-name name))
         (inspector (and symbol (atelier:find-inspector symbol))))
    (unless inspector
      (error 'resource-not-found
             :uri (concatenate 'string "atelier://inspectors/" name)
             :message (format nil "No inspector named ~A." name)))
    (list (cons "name"        name)
          (cons "level"       (string-downcase
                                (symbol-name (atelier:inspector-level inspector))))
          (cons "description" (or (atelier:inspector-description inspector) ""))
          (cons "package"     (package-name
                                (symbol-package
                                 (atelier:inspector-name inspector)))))))

(define-tool inspector-detail (&key name)
  (:description
   "Return the full metadata for one Atelier inspector.")
  (:resource :uri       "atelier://inspectors/{name}"
             :name      "Atelier inspector detail"
             :mime-type :application/json)
  (declare (type string name))
  (%inspector-detail name))

;;;; End of file `inspector-detail.lisp'
