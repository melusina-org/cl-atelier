;;;; bare-lambda.lisp — Fixture: bare lambda and named function variants

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

;;;; SPDX-License-Identifier: MIT

(in-package #:cl-user)

(defun with-bare-lambda (list)
  (mapcar (lambda (x) (1+ x)) list))

(defun with-named-function (list)
  (flet ((increment (x)
           (1+ x)))
    (mapcar #'increment list)))

;;;; End of file `bare-lambda.lisp'
