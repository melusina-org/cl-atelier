;;;; license.lisp — License for the Atelier Lisp System

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:atelier)

(defparameter *license-repository-pathname* (merge-pathnames "license/" *resourcedir*)
  "The pathname to our license repository.")

(defparameter *license-repository* (make-hash-table)
  "A hash-table with all licenses.")

(defclass license nil
  ((license-name
    :initarg :license-name
    :initform nil
    :documentation "The full name of the license.")
   (license-text
    :initarg :license-text
    :initform nil
    :documentation "The full text of the license.")
   (license-header
    :initarg :license-header
    :initform nil
    :documentation "The header of the license.")
   (license-id
    :initarg :license-id
    :initform nil
    :documentation "The ID of the license in the SPDX database."))
  (:documentation
   "This class presents the various characteristics of a software license."))


;;;
;;; License Repository
;;;

(defun license-repository-load-definition (definition-pathname)
  "Load the license definition found in DEFINITION-PATHNAME.
The file located at pathname must be a directory holding a file for each member value."
  (unless (uiop:directory-exists-p definition-pathname)
    (error "~A: License definition directory cannot be read." definition-pathname))
  (let ((members '(:license-name :license-text :license-header :license-id))
	(initargs nil))
    (flet ((load-initarg (member)
	     (let ((pathname
		     (merge-pathnames (string-upcase (symbol-name member))
				      definition-pathname)))
	       (when (probe-file pathname)
		 (push (read-file-into-string pathname) initargs)
		 (push member initargs)))))
      (loop for member in members
	    do (load-initarg member))
      (apply #'make-instance 'license initargs))))

(defun license-repository-list-licenses
    (&optional (license-repository-pathname *license-repository-pathname*))
  "List licenses held in LICENSE-REPOSITORY-PATHNAME."
  (mapcar
   (lambda (pathname)
     (car (last (pathname-directory pathname))))
   (uiop:subdirectories license-repository-pathname)))

(defun license-repository-load
    (&optional (license-repository-pathname *license-repository-pathname*))
  "Load all licenses on LICENSE-REPOSITORY-PATHNAME."
  (loop for name in (license-repository-list-licenses license-repository-pathname)
	for pathname = (merge-pathnames (concatenate 'string name "/") license-repository-pathname)
	for designator = (make-keyword name)
	do (setf (gethash designator *license-repository*)
		 (license-repository-load-definition pathname))))


;;;
;;; License
;;;

(defun find-license (designator)
  "Find license by DESIGNATOR in *LICENSE-REPOSITORY*."
  (cond
    ((typep designator 'license)
     designator)
    ((keywordp designator)
     (gethash designator *license-repository*))
    ((stringp designator)
     (gethash (make-symbol designator) *license-repository*))
    ((null designator)
     nil)
    (t
     (error "~A: This designator is not recognised." designator))))

(defun list-licenses ()
  "Returns a list containing the designators of known licenses."
  (hash-table-keys *license-repository*))

;;;; End of file `license.lisp'
