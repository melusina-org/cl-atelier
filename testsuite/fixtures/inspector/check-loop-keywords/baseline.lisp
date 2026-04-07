;;;; bare-loop-keywords.lisp — Fixture: bare and keyword loop symbol variants

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

;;;; SPDX-License-Identifier: MIT

(in-package #:cl-user)

(defun with-bare-keywords (list)
  (loop for x in list collect x))

(defun with-keyword-symbols (list)
  (loop :for x :in list :collect x))

;;;; End of file `bare-loop-keywords.lisp'
