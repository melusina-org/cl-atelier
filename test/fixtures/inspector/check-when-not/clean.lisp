;;;; clean.lisp — Fixture: WHEN with positive test

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:cl-user)

(defun process (value)
  (when (plusp value)
    (format t "positive: ~A" value)))

;;;; End of file `clean.lisp'
