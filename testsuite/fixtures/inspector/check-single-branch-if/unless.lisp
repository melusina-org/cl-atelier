;;;; unless.lisp — Fixture: IF with explicit NIL then branch

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:cl-user)

(defun process (value)
  (if (zerop value)
      nil
      (format t "non-zero")))

;;;; End of file `unless.lisp'
