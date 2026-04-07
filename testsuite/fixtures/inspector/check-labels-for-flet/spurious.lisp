;;;; spurious-labels.lisp — Fixture: LABELS with acyclic call graph

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

;;;; SPDX-License-Identifier: MIT

(in-package #:cl-user)

;;; This LABELS form has an acyclic call graph: PROCESS-ITEM calls DOUBLE-VALUE
;;; but DOUBLE-VALUE does not call PROCESS-ITEM. Nested FLET is the correct form.
(defun process-list (items)
  (labels ((double-value (x)
             (* x 2))
           (process-item (item)
             (double-value item)))
    (mapcar #'process-item items)))

;;;; End of file `spurious-labels.lisp'
