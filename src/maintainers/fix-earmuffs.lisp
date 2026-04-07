;;;; fix-earmuffs.lisp — Earmuffs naming automatic maintainer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)

(define-automatic-maintainer fix-earmuffs
    ((finding earmuffs-finding))
  "Add *earmuffs* to a DEFVAR or DEFPARAMETER variable name."
  (let* ((name (concrete-syntax-tree:raw (finding-cst-node finding)))
         (replacement (format nil "*~(~A~)*" (symbol-name name))))
    (make-text-resolution
     :maintainer 'fix-earmuffs
     :finding finding
     :kind :automatic
     :description (format nil "Add *earmuffs* to ~S." name)
     :replacement replacement)))

;;;; End of file 'fix-earmuffs.lisp'
