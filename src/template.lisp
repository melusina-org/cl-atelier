;;;; template.lisp — Templates for the Atelier Lisp System

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

(defparameter *template-repository-pathname*
  (merge-pathnames "template/" *resourcedir*)
  "The pathname to our template repository.")

(defparameter *template-repository* (make-hash-table)
  "A hash-table with all templates.")

(defun template-repository-empty-p ()
  (zerop (hash-table-count *template-repository*)))

(defclass template nil
  ((template-name
    :initarg :template-name
    :initform nil
    :documentation "The full name of the template.")
   (template-id
    :initarg :template-id
    :initform nil
    :documentation "The ID of the template.")
   (template-description
    :initarg :template-description
    :initform nil
    :documentation "The long description of the template."))
  (:documentation
   "This class presents the various characteristics of a template."))

(defun find-template (designator)
  "Find template by DESIGNATOR in *TEMPLATE-REPOSITORY*."
  (when (template-repository-empty-p)
    (restart-case (error "The template repository is empty.")
      (initialize ()
	(initialize))))    
  (cond
    ((typep designator 'template)
     designator)
    ((keywordp designator)
     (gethash designator *template-repository*))
    ((stringp designator)
     (gethash (make-symbol (string-upcase designator)) *template-repository*))
    (t
     (error "~A: This designator is not recognised." designator))))

(defgeneric write-template (template pathname &optional environment)
  (:documentation "Write TEMPLATE into PATHNAME."))

(defgeneric list-template-parameters (template)
  (:documentation "List parameters used by TEMPLATE."))

(defun list-templates ()
  "Returns a list containing the designators of known templates."
  (hash-table-keys *template-repository*))

(defun template-environment (environment)
  "Prepare template environment based on ENVIRONMENT.
The prepared template environment features license information and *PARAMETER-BINDINGS*."
  (flet ((license-information ()
	   (let ((license
		   (find-license
		    (or (assoc-value environment :license
				     :test #'parameter-name-equal)
			(assoc-value *parameter-bindings* :license
				     :test #'parameter-name-equal)))))
	     (when license
	       (with-slots (license-name license-text license-header license-id) license
		 (list (cons :license-name license-name)
		       (cons :license-text license-text)
		       (cons :license-header license-header)
		       (cons :license-id license-id))))))
	 (lisp-environment ()
	   (let* ((project-filename
		    (or (assoc-value environment :project-filename
				     :test #'parameter-name-equal)
			(assoc-value *parameter-bindings* :project-filename
				     :test #'parameter-name-equal)))
		  (project-filename-last-dot-position
		    (position #\. project-filename
			      :from-end t))
		  (lisp-package-name
		    (cond (project-filename-last-dot-position
			   (subseq project-filename
				   (1+ project-filename-last-dot-position)))
			  (t project-filename))))
	     (list
	      (cons :lisp-system-name "${PROJECT_FILENAME}")
	      (cons :lisp-package-name lisp-package-name)
	      (cons :lisp-test-system-name "${LISP_SYSTEM_NAME}/test")
	      (cons :lisp-test-package-name "${LISP_PACKAGE_NAME}/test")
	      (cons :lisp-development-system-name "${LISP_SYSTEM_NAME}/development")
	      (cons :lisp-development-package-name "${LISP_PACKAGE_NAME}/development")
	      (cons :lisp-operation-system-name "${LISP_SYSTEM_NAME}/operation")
	      (cons :lisp-operation-package-name "${LISP_PACKAGE_NAME}/operation"))))
	 (authorship ()
	   '((:author . "${COPYRIGHT_HOLDER}"))))
    (reduce #'merge-parameter-bindings
	    (list environment
		  *parameter-bindings*
		  (license-information)
		  (lisp-environment)
		  (authorship)))))

(defmethod write-template ((designator symbol) pathname &optional environment)
  "Write the template identified by DESIGNATOR under PATHNAME.
If environment is provided, it is an alist which is added to the current
*PARAMETER-BINDINGS*.

The PATHNAME argument can actually be a pathname, a stream or the value T designating the
standard output."
  (write-template
   (find-template designator)
   pathname
   (template-environment environment)))


;;;
;;; File Template
;;;

(defclass file-template (template)
  ((template-text
    :initarg :template-text
    :reader template-text
    :initform (error "A file template requires a template text.")
    :documentation "The template text for the file template.")
   (template-executable-p
    :initarg :template-executable-p
    :reader template-executable-p
    :initform nil
    :documentation "Flag controlling the execute bit on created file."))
  (:documentation
   "This class represents a file template which creates a file when written."))

(defun make-file-template (&rest initargs &key template-id template-name template-text
					       template-executable-p)
  "Make a file template."
  (declare (ignore template-id template-name template-text template-executable-p))
  (apply #'make-instance 'file-template initargs))

(defmethod list-template-parameters ((template file-template))
  (with-slots (template-text) template
    (list-parameter-names template-text)))

(defmethod write-template ((template file-template) (pathname (eql t)) &optional environment)
  (write-template template *standard-output* environment))

(defmethod write-template ((template file-template) (pathname stream) &optional environment)
  (let* ((final-environment
	   (cond ((assoc :filename environment)
		  environment)
		 (t
		  (append '((:filename . "filename.out")
			    (:shell-namespace . "filename"))
			  environment))))
	 (template-instance
	   (parameter-replace (slot-value template 'template-text) final-environment)))
    (write-string template-instance pathname)))

(defmethod write-template ((template file-template) (pathname pathname) &optional environment) 
  (let ((filename-bindings
	  (list (cons :filename
		      (if (pathname-type pathname)
			  (concatenate 'string (pathname-name pathname) "." (pathname-type pathname))
			  (pathname-name pathname)))
		(cons :shell-namespace
		      (string-downcase (pathname-name pathname))))))
    (ensure-directories-exist pathname)
    (with-open-file (stream pathname :direction :output)
      (write-template template stream (append filename-bindings environment)))
    (when (slot-value template 'template-executable-p)
      (uiop:run-program (list "/bin/chmod" "a+x" (namestring pathname))))))


;;;
;;; Composite Template
;;;

(defclass composite-template (template)
  ((template-list
    :initarg :template-list
    :initform nil
    :documentation "List of templates that form the composite.
Each entry of the list is a list of the form

  (TEMPLATE-DESIGNATOR RELATIVE-PATHNAME ADDITIONAL-BINDINGS)

"))
  (:documentation
   "This class represents a composite of other template."))

(defmethod list-template-parameters ((template composite-template))
  (labels ((template-designator-list ()
	     (with-slots (template-list) template
	       (delete-duplicates (mapcar #'first template-list) :test #'string-equal)))
	   (template-pathname-list ()
	     (with-slots (template-list) template
	       (mapcar #'namestring (mapcar #'second template-list))))
	   (redundant-parameter-name-list ()
	     (apply #'append
		    (append
		     (mapcar #'list-template-parameters (template-designator-list))
		     (mapcar #'list-parameter-names (template-pathname-list))))))
    (delete-duplicates (redundant-parameter-name-list) :test #'parameter-name-equal)))

(defmethod write-template ((template composite-template) (pathname (eql t)) &optional environment)
  (write-template template *standard-output* environment))

(defmethod write-template ((template composite-template) (pathname pathname) &optional environment)
  (labels ((resolve-pathname (pathname)
	     (pathname (parameter-replace (namestring pathname) environment)))
	   (subtask-pathname (template-designator relative-pathname &optional additional-bindings)
	     (declare (ignore template-designator additional-bindings))
	     (merge-pathnames (resolve-pathname relative-pathname) pathname))
	   (write-plan ()
	     (remove-duplicates
	      (loop :for template-spec :in (slot-value template 'template-list)
		    :collect (let ((subtask-pathname
				     (apply #'subtask-pathname template-spec))
				   (subtask-template
				     (find-template (first (ensure-list template-spec)))))
			       (list subtask-template subtask-pathname environment)))
	      :key #'second
	      :test #'equal
	      :from-end t)))
    (loop :for subtask :in (write-plan)
	  :do (apply #'write-template subtask))))

(defparameter *composite-template-specs*
  '((:template-name "Atelier Project Files"
     :template-id :atelier-project-files
     :template-description "This template provides basic project files."
     :template-list
     ((:license #p"LICENSE")
      (:readme #p"README.md")))
    (:template-name "Atelier Lisp System Scaffolding"
     :template-id :atelier-lisp-system-scaffolding
     :template-description "This templates provides scaffolding to develop lisp systems.
The basisc structure of this scaffolding features an ASDF definition for
the system and a test, as well as common files for the implementation
and the test: package definition, utilities and entry points."
     :template-list
     ((:lisp-asdf #p"${PROJECT_FILENAME}.asd"
       ((:summary . "System definition for ${PROJECT_NAME}")))
      (:lisp-development #p"libexec/lisp/development.lisp"
       ((:summary . "Development System for ${PROJECT_NAME}")))
      (:lisp-docstrings #p"libexec/lisp/docstrings.lisp")
      (:lisp-system-package #p"src/package.lisp"
       ((:summary . "Package for ${PROJECT_NAME}")))
      (:lisp-system-entry-point #p"src/entry-point.lisp"
       ((:summary . "Entry point for ${PROJECT_NAME}")))
      (:lisp-source #p"src/utilities.lisp"
       ((:summary . "Utilities for ${PROJECT_NAME}")))
      (:lisp-test-package #p"test/package.lisp"
       ((:summary . "Package for ${PROJECT_NAME} test")))
      (:lisp-test-entry-point #p"test/entry-point.lisp"
       ((:summary . "Entry point for ${PROJECT_NAME} test")))
      (:lisp-source #p"test/utilities.lisp"
       ((:summary . "Utilities for ${PROJECT_NAME} test")
	(:lisp-package-name . "${LISP_TEST_PACKAGE_NAME}")))))
    (:template-name "Atelier DevOps Actions"
     :template-id :atelier-devops-actions
     :template-description "This template provides scripts for development and operations.
These scripts are merely place hodlers."
     :template-list
     ((:shell-stdlib #p"subr/stdlib.sh")
      (:shell-script #p"development/audit"
       ((:summary . "Audit ${PROJECT_NAME}")))
      (:shell-script #p"development/build"
       ((:summary . "Build ${PROJECT_NAME}")))
      (:shell-script #p"development/clean"
       ((:summary . "Clean ${PROJECT_NAME}")))
      (:shell-script #p"development/configure"
       ((:summary . "Configure ${PROJECT_NAME}")))
      (:shell-script #p"development/describe"
       ((:summary . "Describe ${PROJECT_NAME}")))
      (:shell-script #p"development/lint"
       ((:summary . "Lint ${PROJECT_NAME}")))
      (:shell-script #p"development/publish"
       ((:summary . "Publish ${PROJECT_NAME}")))
      (:shell-script #p"development/run"
       ((:summary . "Run ${PROJECT_NAME}")))
      (:shell-script #p"development/setup"
       ((:summary . "Setup workstation for ${PROJECT_NAME} development")))
      (:shell-script #p"development/test"
       ((:summary . "Test for ${PROJECT_NAME}")))
      (:shell-script #p"development/verify"
       ((:summary . "Verify ${PROJECT_NAME}")))
      (:shell-script #p"operation/audit"
       ((:summary . "Audit for ${PROJECT_NAME} deployments")))
      (:shell-script #p"operation/configure"
       ((:summary . "Configure ${PROJECT_NAME} deployments")))
      (:shell-script #p"operation/console"
       ((:summary . "Operation console for ${PROJECT_NAME} deployments")))
      (:shell-script #p"operation/create"
       ((:summary . "Create a ${PROJECT_NAME} deployment")))
      (:shell-script #p"operation/delete"
       ((:summary . "Delete a ${PROJECT_NAME} deployment")))
      (:shell-script #p"operation/describe"
       ((:summary . "Describe a ${PROJECT_NAME} deployment")))
      (:shell-script #p"operation/dump"
       ((:summary . "Dump the state of a ${PROJECT_NAME} deployment")))
      (:shell-script #p"operation/restore"
       ((:summary . "Restore the state of a ${PROJECT_NAME} deployment")))
      (:shell-script #p"operation/setup"
       ((:summary . "Setup workstation for ${PROJECT_NAME} deployments")))
      (:shell-script #p"operation/start"
       ((:summary . "Start a ${PROJECT_NAME} deployment")))
      (:shell-script #p"operation/status"
       ((:summary . "Probe the status of a ${PROJECT_NAME} deployment")))
      (:shell-script #p"operation/stop"
       ((:summary . "Stop a ${PROJECT_NAME} deployment")))
      (:shell-script #p"operation/update"
       ((:summary . "Update a ${PROJECT_NAME} deployment")))))
    (:template-name "Atelier Lisp Documentation"
     :template-id :atelier-lisp-documentation
     :template-description "This template provides scripts to build lisp documentation with texinfo"
     :template-list
     ((:lisp-development-makedoc #p"development/makedoc")
      (:texinfo #p"doc/${PROJECT_FILENAME}.texinfo")))
    (:template-name "Atelier Lisp DevOps Actions"
     :template-id :atelier-lisp-devops-actions
     :template-description "This template provides scripts for development and operations.
These scripts are specific to Lisp projects."
     :template-list
     ((:lisp-development-lint #p"development/lint")
      (:lisp-development-test #p"development/test")
      (:lisp-development-build #p"development/build")))
    (:template-name "Atelier Lisp Project"
     :template-id :atelier-lisp-project
     :template-description "A complete Lisp project template"
     :template-list (:atelier-project-files
		     :atelier-lisp-system-scaffolding
		     :atelier-devops-actions
		     :atelier-lisp-devops-actions
		     :atelier-lisp-documentation
		     (:lisp-git-ignore #p".gitignore")))))


;;;
;;; Template Repository
;;;

(defun template-repository-load-definition-file (pathname)
  "Load the file template from PATHNAME."
  (unless (uiop:file-exists-p pathname)
    (error "~A: Template definition cannot be read." pathname))
  (multiple-value-bind (front-matter documents) (read-file-documents-with-yaml-front-matter pathname)
    (make-instance
     'file-template
     :template-name (alexandria:assoc-value front-matter :name)
     :template-executable-p (let ((executable-field
				    (alexandria:assoc-value front-matter :executable)))
			      (when executable-field
				(string= executable-field "true")))
     :template-id (make-keyword (string-upcase (pathname-name pathname)))
     :template-text (join-lines (first documents)))))

(defun template-repository-list-templates (&optional (template-repository-pathname *template-repository-pathname*))
  "List templates held in TEMPLATE-REPOSITORY-PATHNAME."
  (mapcar #'pathname-name (uiop:directory-files template-repository-pathname "*.text")))

(defun template-repository-load (&optional (template-repository-pathname *template-repository-pathname*))
  "Load all templates on TEMPLATE-REPOSITORY-PATHNAME."
  (labels ((resolve-file (designator)
	     (typecase designator
	       (keyword
		(resolve-template-list
		 (slot-value (gethash designator *template-repository*) 'template-list)))
	       (t
		(list designator))))
	   (resolve-template-list (designators)
	     (loop :for designator :in designators
		   :append (resolve-file designator)))
	   (resolve-template-spec (spec)
	     (let ((spec-copy
		     (copy-list spec)))
	       (setf (getf spec-copy :template-list)
		     (resolve-template-list (getf spec-copy :template-list)))
	       spec-copy)))
    (loop :for pathname :in (uiop:directory-files template-repository-pathname "*.text")
	  :for designator = (make-keyword (string-upcase (pathname-name pathname)))
	  do (setf (gethash designator *template-repository*)
		   (template-repository-load-definition-file pathname)))
    (loop :for spec :in *composite-template-specs*
	  :for designator = (getf spec :template-id)
	  :do (setf (gethash designator *template-repository*)
		    (apply #'make-instance 'composite-template (resolve-template-spec spec))))))


;;;;
;;;; Lisp Projects
;;;;

(defun new-lisp-project (pathname
			 &key environment
			      copyright-holder copyright-year project-filename project-name
			      project-description project-long-description
			      homepage license)
  "Create a new lisp project in PATHNAME."
  (let* ((argv-parameter-bindings
	   `((:copyright-holder . ,copyright-holder)
             (:copyright-year . ,copyright-year)
	     (:project-filename . ,project-filename)
             (:project-name . ,project-name)
	     (:project-description . ,project-description)
	     (:project-long-description . ,project-long-description)
             (:homepage . ,homepage)
             (:license . ,license)))
	 (merged-parameter-bindings
	   (merge-parameter-bindings *parameter-bindings* argv-parameter-bindings)))
    (let ((*parameter-bindings* merged-parameter-bindings))
      (write-template :atelier-lisp-project pathname environment))))

(defun new-lisp-file (name summary)
  "Create a new file NAME with SUMMARY."
  (let ((source-pathname
	  (reduce #'merge-pathnames (list "src/" name) :from-end t))
	(test-pathname
	  (reduce #'merge-pathnames (list "test/" name) :from-end t))
	(source-environment
	  `((:summary . ,summary)
	    (:lisp-package-name . "#:${PROJECT_FILENAME}")))
	(test-environment
	  `((:summary . ,summary)
	    (:lisp-package-name . "#:${PROJECT_FILENAME}/test"))))
    (write-template :lisp-source source-pathname source-environment)
    (write-template :lisp-source test-pathname test-environment)))

;;;; End of file `template.lisp'
