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


(defun migrate-licences ()
  (atelier:initialize)
  (loop :for license :being :the :hash-values :of atelier::*license-repository*
	:do (license-repository-write-definition license)))

;;;; End of file `migration.lisp'
