;;;; migration.lisp — Project Migration for Atelier

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:atelier/migration
  (:use #:cl)
  (:export
   #:migrate-licences
   #:migrate-templates
))

(in-package #:atelier/migration)

(defun license-repository-write-definition (license)
  (flet ((write-yaml-separator ()
	   (write-line "---"))
	 (write-front-matter ()
	   (format t "name: ~A~%" (slot-value license 'atelier::license-name))
	   (format t "id: ~A~%" (slot-value license 'atelier::license-id)))
	 (write-header ()
	   (write-line
	    (string-right-trim
	     '(#\Newline #\Space)
	     (slot-value license 'atelier::license-header))))
	 (write-text ()
	   (write-line
	    (string-right-trim
	     '(#\Newline #\Space)
	     (slot-value license 'atelier::license-text)))))
    (with-open-file (*standard-output* (merge-pathnames
					(make-pathname :name (slot-value license 'atelier::license-id)
						       :type "text")
					atelier::*license-repository-pathname*)
				       :direction :output)
      (write-yaml-separator)
      (write-front-matter)
      (write-yaml-separator)
      (write-header)
      (write-yaml-separator)
      (write-text)
      (write-yaml-separator))))

(defun trimmed-slot-value (object slot-name)
  (string-right-trim '(#\Newline #\Space #\Tab) (slot-value object slot-name)))


(defun migrate-licences ()
  (atelier:initialize)
  (loop :for license :being :the :hash-values :of atelier::*license-repository*
	:do (license-repository-write-definition license)))

(defun template-repository-write-definition (template)
  (flet ((write-yaml-separator ()
	   (write-line "---"))
	 (write-yaml-end ()
	   (write-line "..."))
	 (write-front-matter ()
	   (format t "name: ~A~%" (trimmed-slot-value template 'atelier::template-name))
	   (when (or (slot-value template 'atelier::template-executable-p)
		     (member (slot-value template 'atelier::template-id)
			     '(:shell-script
			       :lisp-development-build
			       :lisp-development-lint
			       :lisp-development-makedoc
			       :lisp-development-testsuite)))
	     (format t "executable: true~%"))
	   (when (slot-value template 'atelier::template-description)
	     (format t "description: ~A~%" (trimmed-slot-value template 'atelier::template-description))))
	 (write-text ()
	   (write-line
	    (string-right-trim
	     '(#\Newline #\Space)
	     (slot-value template 'atelier::template-text)))))
    (with-open-file (*standard-output*
		     (merge-pathnames
		      (make-pathname :name (symbol-name (slot-value template 'atelier::template-id))
				     :type "text")
		      atelier::*template-repository-pathname*)
		     :direction :output
		     :if-exists :supersede)
      (write-yaml-separator)
      (write-front-matter)
      (write-yaml-separator)
      (write-text)
      (write-yaml-end))))

(defun migrate-templates ()
  (atelier:initialize)
  (loop :for template :being :the :hash-values :of atelier::*template-repository*
	:when (typep template 'atelier::file-template)
	:do (template-repository-write-definition template)))

;;;; End of file `migration.lisp'
