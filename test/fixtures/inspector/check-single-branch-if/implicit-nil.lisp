;;;; implicit-nil.lisp — Fixture: IF with implicit NIL else branch

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:cl-user)

(defun process (value)
  (if (plusp value)
      (format t "positive")))

;;;; End of file `implicit-nil.lisp'
