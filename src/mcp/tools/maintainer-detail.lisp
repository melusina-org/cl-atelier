;;;; maintainer-detail.lisp — atelier:maintainer-detail tool/resource

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(defun %intern-maintainer-name (name)
  "Resolve NAME (a string) to the corresponding maintainer symbol in
   the ATELIER package, or NIL if not interned."
  (find-symbol (string-upcase name) (find-package :atelier)))

(defun %maintainer-detail (name)
  "Return a detailed alist describing the maintainer NAME (a string),
   or signal RESOURCE-NOT-FOUND when no such maintainer exists."
  (let* ((symbol     (%intern-maintainer-name name))
         (maintainer (and symbol (atelier:find-maintainer symbol))))
    (unless maintainer
      (error 'resource-not-found
             :uri (concatenate 'string "atelier://maintainers/" name)
             :message (format nil "No maintainer named ~A." name)))
    (list (cons "name"        name)
          (cons "kind"        (string-downcase
                                (symbol-name (atelier:maintainer-kind maintainer))))
          (cons "maturity"    (string-downcase
                                (symbol-name (atelier:maintainer-maturity maintainer))))
          (cons "supersedes"  (mapcar (lambda (s) (string-downcase (symbol-name s)))
                                      (atelier:maintainer-supersedes maintainer)))
          (cons "description" (or (atelier:maintainer-description maintainer) "")))))

(define-tool maintainer-detail (&key name)
  (:description
   "Return the full metadata for one Atelier maintainer.")
  (:resource :uri       "atelier://maintainers/{name}"
             :name      "Atelier maintainer detail"
             :mime-type :application/json)
  (declare (type string name))
  (%maintainer-detail name))

;;;; End of file `maintainer-detail.lisp'
