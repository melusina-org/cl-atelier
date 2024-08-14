;;;; development.lisp — Project Development for Atelier

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.atelier/development
  (:use #:cl)
  (:local-nicknames
   (#:atelier #:org.melusina.atelier))
  (:export
   #:lint
   #+quicklisp
   #:reload))

(in-package #:org.melusina.atelier/development)

(defun system-relative-pathname (pathname)
  (flet ((system-source-directory ()
	   (asdf:system-source-directory #.(string-downcase (package-name *package*)))))
    (merge-pathnames pathname (system-source-directory))))

(defun system-relative-pathnames (&rest pathnames)
  (mapcar #'system-relative-pathname pathnames))

(defparameter *parameter-bindings*
      '((:copyright-holder . "Michaël Le Barbier")
        (:copyright-year . "2017–2023")
	(:project-filename . "atelier")
        (:project-name . "Atelier")
	(:project-description . "Atelier for Lisp developers")
        (:project-long-description .
	 #.(concatenate 'string
	    "The atelier for Lisp developers is providing useful tools for Lisp developpers"
	    " such as project templates and a linter."))
        (:homepage . "https://github.com/melusina-org/cl-atelier")
        (:license . :mit)))

(defun lint ()
  (let ((atelier:*parameter-bindings* *parameter-bindings*))
    (atelier:lint
     (system-relative-pathnames
      #p"org.melusina.atelier.asd"
      #p"development"
      #p"doc"
      #p"docker"
      #p"docker/compose/cid.yml"
      #p"docker/image/linux/users.conf"
      #p"src"
      #p"subr"
      #p"support"
      #p"testsuite"
      #p"libexec/lisp/development.lisp"
      #p"libexec/setup"
      #p"libexec/testimage"))))

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
