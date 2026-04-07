;;;; necessary-labels.lisp — Fixture: LABELS with mutual recursion

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

;;; This LABELS form is legitimate: EVEN-P and ODD-P call each other (mutual
;;; recursion). The call graph has a cycle, so LABELS is needed.
(defun classify-numbers (items)
  (labels ((even-p (n)
             (if (zerop n) t (odd-p (1- n))))
           (odd-p (n)
             (if (zerop n) nil (even-p (1- n)))))
    (remove-if #'odd-p items)))

;;; This function uses a self-recursive local function. LABELS is needed.
(defun sum-list (items)
  (labels ((sum-rest (remaining acc)
             (if (null remaining)
                 acc
                 (sum-rest (rest remaining) (+ acc (first remaining))))))
    (sum-rest items 0)))

;;;; End of file `necessary-labels.lisp'
