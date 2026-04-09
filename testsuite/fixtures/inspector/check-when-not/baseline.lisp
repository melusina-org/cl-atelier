;;;; baseline.lisp — Fixture: WHEN with negated test

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:cl-user)

(defun process (value)
  (when (not (zerop value))
    (format t "non-zero: ~A" value)))

;;;; End of file `baseline.lisp'
