;;;; good.lisp — Fixture: empty PROGN is not flagged by this inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:cl-user)

(defun noop ()
  (progn))

;;;; End of file `good.lisp'
