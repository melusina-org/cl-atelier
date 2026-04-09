;;;; clean.lisp — Fixture: IF with both branches non-NIL

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:cl-user)

(defun process (value)
  (if (plusp value)
      (format t "positive")
      (format t "non-positive")))

;;;; End of file `clean.lisp'
