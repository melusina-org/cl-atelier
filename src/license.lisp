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

(defun license-repository-load-definition-from-text (definition-pathname)
  "Load a license definition from a single text file with YAML Front Matter.
The file format is expected to have three documents separated by '---':
1. Front Matter (YAML-like key-value pairs)
2. License Header
3. License Text"
  (let ((content
	  (read-file-into-string definition-pathname))
        (state
	  :start)
        (front-matter-lines
	  nil)
        (header-lines
	  nil)
        (text-lines
	  nil))
    (loop :for line :in (string-lines content)
          :for trimmed-line = (string-trim '(#\Space #\Tab #\Return) line)
          :do (cond
		((string= trimmed-line "---")
                 (setf state
                       (ecase state
                         (:start :front-matter)
                         (:front-matter :header)
                         (:header :text)
                         (:text :text))))
		(t
                 (case state
                   (:front-matter (push line front-matter-lines))
                   (:header (push line header-lines))
                   (:text (push line text-lines))))))
    (let ((license-name nil)
          (license-id nil))
      (loop :for line :in (nreverse front-matter-lines)
            :for colon-pos = (position #\: line)
            :when colon-pos
            :do (let ((key (string-trim '(#\Space #\Tab) (subseq line 0 colon-pos)))
                      (value (string-trim '(#\Space #\Tab) (subseq line (1+ colon-pos)))))
                  (cond
                    ((string-equal key "name") (setf license-name value))
                    ((string-equal key "id") (setf license-id value)))))
      (make-instance 'license
                     :license-name license-name
                     :license-id license-id
                     :license-header (string-trim '(#\Newline #\Return #\Space #\Tab)
                                                  (join-lines (nreverse header-lines)))
                     :license-text (string-trim '(#\Newline #\Return #\Space #\Tab)
                                                (join-lines (nreverse text-lines)))))))

(defun license-repository-load-definition (definition-pathname)
  "Load the license definition found in DEFINITION-PATHNAME.
The file located at pathname must be a directory holding a file for each member value
or a single text file with YAML Front Matter."
  (cond
    ((uiop:directory-exists-p definition-pathname)
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
    ((uiop:file-exists-p definition-pathname)
     (license-repository-load-definition-from-text definition-pathname))
    (t
     (error "~A: License definition cannot be read." definition-pathname))))

(defun license-repository-list-licenses
    (&optional (license-repository-pathname *license-repository-pathname*))
  "List licenses held in LICENSE-REPOSITORY-PATHNAME."
  (let ((directory-licenses
	  (mapcar
	   (lambda (pathname)
	     (car (last (pathname-directory pathname))))
	   (uiop:subdirectories license-repository-pathname)))
	(file-licenses
	  (mapcar
	   (lambda (pathname)
	     (pathname-name pathname))
	   (uiop:directory-files license-repository-pathname "*.txt"))))
    (nconc directory-licenses file-licenses)))

(defun license-repository-load
    (&optional (license-repository-pathname *license-repository-pathname*))
  "Load all licenses on LICENSE-REPOSITORY-PATHNAME."
  (let ((directory-licenses
	  (uiop:subdirectories license-repository-pathname))
	(file-licenses
	  (uiop:directory-files license-repository-pathname "*.txt")))
    (loop for pathname in directory-licenses
	  for name = (car (last (pathname-directory pathname)))
	  for designator = (make-keyword name)
	  do (setf (gethash designator *license-repository*)
		   (license-repository-load-definition pathname)))
    (loop for pathname in file-licenses
	  for name = (pathname-name pathname)
	  for designator = (make-keyword name)
	  do (setf (gethash designator *license-repository*)
		   (license-repository-load-definition pathname)))))


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
