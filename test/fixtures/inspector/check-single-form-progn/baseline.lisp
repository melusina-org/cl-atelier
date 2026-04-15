;;;; baseline.lisp — Fixture: PROGN wrapping a single body form

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:cl-user)

(defun process (value)
  (progn
    (format t "value: ~A" value)))

;;;; End of file `baseline.lisp'
