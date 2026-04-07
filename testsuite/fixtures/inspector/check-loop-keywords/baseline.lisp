;;;; bare-loop-keywords.lisp — Fixture: bare and keyword loop symbol variants

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

(defun with-bare-keywords (list)
  (loop for x in list collect x))

(defun with-keyword-symbols (list)
  (loop :for x :in list :collect x))

;;;; End of file `bare-loop-keywords.lisp'
