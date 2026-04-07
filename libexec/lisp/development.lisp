;;;; development.lisp — Project Development for Atelier

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:atelier/development
  (:use #:cl)
  (:export
   #:lint
   #+quicklisp
   #:reload))

(in-package #:atelier/development)

(defun lint ()
  "Lint the Atelier project using the new-style linter."
  (atelier:lint-system "org.melusina.atelier"))

#+quicklisp
(defun reload ()
  (ql:quickload '("org.melusina.confidence"
		  "org.melusina.atelier"
		  "org.melusina.atelier/development"
		  "org.melusina.atelier/testsuite")))


;;;;
;;;; Command Stock
;;;;

#+nil
(org.melusina.atelier/development:reload)

;;;; End of file `development.lisp'
