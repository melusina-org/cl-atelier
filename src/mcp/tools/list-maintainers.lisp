;;;; list-maintainers.lisp — atelier:list-maintainers tool/resource

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(defun %maintainer-summary (designator)
  "Return an alist describing the maintainer named by DESIGNATOR."
  (let ((maintainer (atelier:find-maintainer designator)))
    (when maintainer
      (list (cons "name"        (string-downcase
                                  (symbol-name (atelier:maintainer-name maintainer))))
            (cons "kind"        (string-downcase
                                  (symbol-name (atelier:maintainer-kind maintainer))))
            (cons "maturity"    (string-downcase
                                  (symbol-name (atelier:maintainer-maturity maintainer))))
            (cons "supersedes"  (mapcar (lambda (s) (string-downcase (symbol-name s)))
                                        (atelier:maintainer-supersedes maintainer)))
            (cons "description" (or (atelier:maintainer-description maintainer) ""))))))

(define-tool list-maintainers ()
  (:description
   "List all registered Atelier maintainers with their kind, maturity, and superseding relations.")
  (:resource :uri       "atelier://maintainers"
             :name      "Atelier maintainer registry"
             :mime-type :application/json)
  (mapcar #'%maintainer-summary (atelier:list-maintainers)))

;;;; End of file `list-maintainers.lisp'
