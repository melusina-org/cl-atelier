;;;; clean.lisp — Fixture: PROGN wrapping multiple body forms

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:cl-user)

(defun process (value)
  (progn
    (format t "value: ~A" value)
    (format t "done")))

;;;; End of file `clean.lisp'
