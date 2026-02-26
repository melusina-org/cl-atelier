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
  ((name
    :initarg :name
    :reader license-name
    :initform nil
    :documentation "The full name of the license.")
   (text
    :initarg :text
    :reader license-text
    :initform nil
    :documentation "The full text of the license.")
   (header
    :initarg :header
    :reader license-header
    :initform nil
    :documentation "The header of the license.")
   (identifier
    :initarg :identifier
    :reader license-id
    :initform nil
    :documentation "The ID of the license in the SPDX database."))
  (:documentation
   "This class presents the various characteristics of a software license."))


;;;
;;; License Repository
;;;

(defun license-repository-load-definition (pathname)
  "Load a license definition from a single text file with YAML Front Matter.
The file format is expected to have three documents separated by '---':
1. Front Matter (YAML-like key-value pairs)
2. License Header
3. License Text"
  (multiple-value-bind (front-matter documents) (read-file-documents-with-yaml-front-matter pathname)
    (make-instance
     'license
     :name (assoc-value front-matter :name)
     :identifier (or
		  (assoc-value front-matter :id)		  
		  (assoc-value front-matter :identifier)
		  (make-keyword (string-upcase (pathname-name pathname))))
     :header (join-lines (first documents))
     :text (join-lines (second documents)))))

(defun license-repository-list-licenses (&optional (license-repository-pathname *license-repository-pathname*))
  "List licenses held in LICENSE-REPOSITORY-PATHNAME."
  (mapcar #'pathname-name (uiop:directory-files license-repository-pathname "*.text")))

(defun license-repository-load (&optional (license-repository-pathname *license-repository-pathname*))
  "Load all licenses on LICENSE-REPOSITORY-PATHNAME."
  (loop :for pathname :in (uiop:directory-files license-repository-pathname "*.text")
	:for designator = (make-keyword (string-upcase (pathname-name pathname)))
	:do (setf (gethash designator *license-repository*)
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
     (loop :for identifier :being :the :hash-keys :of *license-repository*
	   :using (hash-value license)
	   :when (string-equal (symbol-name identifier) designator)
	   :return license))
    ((null designator)
     nil)
    (t
     (error "~A: This designator is not recognised." designator))))

(defun list-licenses ()
  "Returns a list containing the designators of known licenses."
  (hash-table-keys *license-repository*))

;;;; End of file `license.lisp'
