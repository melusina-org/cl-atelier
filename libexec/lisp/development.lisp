;;;; development.lisp — Project Development for Atelier

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(defpackage #:atelier/development
  (:use #:cl)
  (:import-from #:confidence
   #:define-testcase
   #:assert-condition
   #:assert-eq
   #:assert-nil
   #:assert-string=
   #:assert-t
   #:assert-t*
   #:assert=)
  (:export
   #:lint
   #+quicklisp
   #:reload))

(in-package #:atelier/development)

(defun project-configuration ()
  "Return the project configuration for Atelier, read from project-configuration.sexp."
  (let ((path (merge-pathnames #p"project-configuration.sexp"
                               (asdf:system-source-directory "org.melusina.atelier"))))
    (atelier:read-project-configuration path)))

(defun lint ()
  "Lint the Atelier project using the new-style linter."
  (atelier:lint-system "org.melusina.atelier"))

#+quicklisp
(defun reload ()
  (ql:quickload
   '("org.melusina.confidence"
     "org.melusina.atelier"
     "org.melusina.atelier/development"
     "org.melusina.atelier/testsuite")))


;;;;
;;;; Command Stock
;;;;

#+nil
(org.melusina.atelier/development:reload)

;;;; End of file `development.lisp'
