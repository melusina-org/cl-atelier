;;;; bare-lambda.lisp — Fixture: bare lambda and named function variants

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

;;;; SPDX-License-Identifier: MIT

(in-package #:cl-user)

(defun with-bare-lambda (list)
  (mapcar (lambda (x) (1+ x)) list))

(defun with-named-function (list)
  (flet ((increment (x)
           (1+ x)))
    (mapcar #'increment list)))

;;;; End of file `bare-lambda.lisp'
