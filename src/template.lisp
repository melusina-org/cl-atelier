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
  (= 0 (hash-table-count *template-repository*)))

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

(defgeneric really-write-template (template pathname template-environment)
  (:documentation "Write TEMPLATE into PATHNAME."))

(defgeneric really-list-template-parameter-names (template)
  (:documentation "List parameters used by TEMPLATE."))

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
     (gethash (make-symbol designator) *template-repository*))
    (t
     (error "~A: This designator is not recognised." designator))))

(defun list-templates ()
  "Returns a list containing the designators of known templates."
  (hash-table-keys *template-repository*))

(defun list-template-parameter-names (template-designator)
  "List the parameters consumed by TEMPLATE-DESIGNATOR."
  (really-list-template-parameter-names (find-template template-designator)))

(defun make-shell-namespace (filename)
  "Make a shell function namespace out of FILENAME.
The result is a word, without hyphen or dashes, that can be used as
a prefix when defining shell functions and variables."
  (subseq filename 0 (position #\. filename)))

(defun template-environment (environment)
  "Prepare template environment based on ENVIRONMENT.
The prepared template environment features license information and *PARAMETER-BINDINGS*."
  (labels
      ((license ()
	 (find-license
	  (cdr (or (assoc :license environment
			  :test #'parameter-name-equal)
		   (assoc :license *parameter-bindings*
			  :test #'parameter-name-equal)))))
       (license-information ()
	 (when (license)
	   (with-slots (license-name license-text license-header license-id) (license)
	     (list (cons :license-name license-name)
		   (cons :license-text license-text)
		   (cons :license-header license-header)
		   (cons :license-id license-id)))))
       (lisp-environment ()
	 '((:lisp-system-name . "${PROJECT_FILENAME}")
	   (:lisp-test-system-name . "${PROJECT_FILENAME}/testsuite")
	   (:lisp-package-name . "#:${PROJECT_FILENAME}")
	   (:lisp-test-package-name . "#:${PROJECT_FILENAME}/testsuite")))
       (shell-environment ()
	 (when (assoc :filename environment)
	   (list (cons :shell-namespace
		       (make-shell-namespace
			(cdr (assoc :filename environment)))))))
       (authorship ()
	 '((:author . "${COPYRIGHT_HOLDER}"))))
    (reduce #'merge-parameter-bindings
	    (list environment
		  *parameter-bindings*
		  (license-information)
		  (lisp-environment)
		  (shell-environment)))))

(defun write-template (template-designator pathname &optional environment)
  "Write the template identified by DESIGNATOR under PATHNAME.

If environment is provided, it is a plist which is added to the current
*PARAMETER-BINDINGS*.

The PATHNAME argument can actually be a pathname, a string instepreted as a UNIX path,
a stream or the value T."
  (let ((filename 
          (cond
	    ((assoc :filename environment)
	     nil)
	    ((eq pathname t)
             "filename.out")
	    ((pathname-type pathname)
	     (concatenate 'string (pathname-name pathname) "." (pathname-type pathname)))
	    (t
             (pathname-name pathname))))
	(local-environment
	  environment))
    (when filename
      (push (cons :filename filename) local-environment))
    (really-write-template
     (find-template template-designator)
     pathname
     (template-environment local-environment))))


;;;
;;; File Template
;;;

(defclass file-template (template)
  ((template-text
    :initarg :template-text
    :initform (error "A file template requires a template text.")
    :documentation "The template text for the file template."))
  (:documentation
   "This class represents a file template which creates a file when written."))

(defun make-file-template (&rest initargs
			   &key template-id template-name template-text)
  "Make a file template."
  (declare (ignore template-id template-name template-text))
  (apply #'make-instance 'file-template initargs))

(defun template-repository-load-definition-file (definition-pathname)
  "Load the file template from DEFINITION-PATHNAME."
  (unless (uiop:directory-exists-p definition-pathname)
    (error "~A: Template definition directory cannot be read." definition-pathname))
  (labels
      ((load-file (member)
	 (let ((pathname
		 (merge-pathnames
		  (string-upcase (symbol-name member))
		  definition-pathname)))
	   (when (probe-file pathname)
	     (read-file-into-string pathname))))
       (template-name ()
	 (load-file :template-name))
       (template-text ()
	 (load-file :template-text)))
    (make-instance 'file-template
		   :template-id (make-symbol (car (last (pathname-directory definition-pathname))))
		   :template-name (template-name)
		   :template-text (template-text))))

(defmethod really-list-template-parameter-names ((template file-template))
  (with-slots (template-text) template
    (list-parameter-names template-text)))

(defmethod really-write-template ((template file-template) pathname template-environment)
  (let ((template-instance
	  (parameter-replace (slot-value template 'template-text) template-environment)))
    (cond
      ((eq t pathname)
       (write-string template-instance *standard-output*))
      ((streamp pathname)
       (write-string template-instance pathname))
      ((or (stringp pathname) (pathnamep pathname))
       (ensure-directories-exist pathname)
       (with-open-file (stream pathname :direction :output)
	 (write-string template-instance stream))
       (when (member (slot-value template 'template-id)
		     '(:shell-script
		       :lisp-development-build
		       :lisp-development-lint
		       :lisp-development-makedoc
		       :lisp-development-testsuite)
		     :test #'string-equal)
	 (osicat-posix:chmod pathname #o755)))
      (t
       (error "Cannot write template instance to ~A." pathname)))))


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

(defmethod really-list-template-parameter-names ((template composite-template))
  (labels ((template-designator-list ()
	     (with-slots (template-list) template
	       (delete-duplicates (mapcar #'first template-list) :test #'string-equal)))
	   (template-pathname-list ()
	     (with-slots (template-list) template
	       (mapcar #'namestring (mapcar #'second template-list))))
	   (redundant-parameter-name-list ()
	     (apply #'append
		    (append
		     (mapcar #'list-template-parameter-names (template-designator-list))
		     (mapcar #'list-parameter-names (template-pathname-list))))))
    (delete-duplicates (redundant-parameter-name-list) :test #'parameter-name-equal)))


(defmethod really-write-template ((template composite-template) pathname template-environment)
  (labels
      ((resolve-pathname (pathname)
	 (pathname (parameter-replace (namestring pathname) template-environment)))
       (sub-filename (template-designator relative-pathname additional-bindings)
	 (declare (ignore template-designator additional-bindings))
	 (if (pathname-type relative-pathname)
	     (concatenate 'string (pathname-name relative-pathname) "." (pathname-type relative-pathname))
	     (pathname-name relative-pathname)))
       (sub-bindings (template-designator relative-pathname &optional additional-bindings)
	 (concatenate
	  'list
	  (list (cons :filename (sub-filename template-designator relative-pathname additional-bindings)))
	  additional-bindings
	  template-environment))
       (sub-pathname (template-designator relative-pathname &optional additional-bindings)
	 (declare (ignore template-designator additional-bindings))
	 (cond
	   ((eq t pathname)
	    t)
	   ((streamp pathname)
	    pathname)
	   ((or (stringp pathname) (pathnamep pathname))
	    (merge-pathnames (resolve-pathname relative-pathname) pathname))
	   (t
	    (error "Cannot write composite template to ~A." pathname)))))
    (let ((write-plan
	    (make-hash-table)))
      (loop :for template-spec :in (slot-value template 'template-list)
	    :do (let ((pathname
			(apply #'sub-pathname template-spec))
		      (bindings
			(apply #'sub-bindings template-spec))
		      (template
			(find-template (first template-spec))))
		  (setf (gethash pathname write-plan)
			(list template pathname bindings))))
      (loop :for write-spec :being :each :hash-value :of write-plan
	    :do (apply #'write-template write-spec)))))

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
the system and a testsuite, as well as common files for the implementation
and the testsuite: package definition, utilities and entrypoints."
     :template-list
     ((:lisp-asdf #p"${PROJECT_FILENAME}.asd"
       ((:summary . "System definition for ${PROJECT_NAME}")))
      (:lisp-development #p"libexec/lisp/development.lisp"
       ((:summary . "Development System for ${PROJECT_NAME}")))
      (:lisp-docstrings #p"libexec/lisp/docstrings.lisp")
      (:lisp-package #p"src/package.lisp"
       ((:summary . "Package for ${PROJECT_NAME}")))
      (:lisp-source #p"src/utilities.lisp"
       ((:summary . "Utilities for ${PROJECT_NAME}")))
      (:lisp-source #p"src/entrypoint.lisp"
       ((:summary . "Entrypoint for ${PROJECT_NAME}")))
      (:lisp-testsuite-package #p"testsuite/package.lisp"
       ((:summary . "Package for ${PROJECT_NAME} tests")
	(:lisp-package-name . "${LISP_TEST_PACKAGE_NAME}")))
      (:lisp-source #p"testsuite/utilities.lisp"
       ((:summary . "Utilities for ${PROJECT_NAME} tests")
	(:lisp-package-name . "${LISP_TEST_PACKAGE_NAME}")))
      (:lisp-testsuite-entrypoint #p"testsuite/entrypoint.lisp"
       ((:summary . "Entrypoint for ${PROJECT_NAME}")
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
      (:shell-script #p"development/testsuite"
       ((:summary . "Testsuite for ${PROJECT_NAME}")))
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
      (:lisp-development-testsuite #p"development/testsuite")
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

(defun template-repository-list-templates
    (&optional (template-repository-pathname *template-repository-pathname*))
  "List templates held in TEMPLATE-REPOSITORY-PATHNAME."
  (mapcar
   (lambda (pathname)
     (car (last (pathname-directory pathname))))
   (uiop:subdirectories template-repository-pathname)))

(defun template-repository-load
    (&optional (template-repository-pathname *template-repository-pathname*))
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
    (loop for name in (template-repository-list-templates template-repository-pathname)
	  for pathname = (merge-pathnames (concatenate 'string name "/") template-repository-pathname)
	  for designator = (make-keyword name)
	  do (setf (gethash designator *template-repository*)
		   (template-repository-load-definition-file pathname)))
    (loop for spec in *composite-template-specs*
	  for designator = (getf spec :template-id)
	  do (setf (gethash designator *template-repository*)
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
	(testsuite-pathname
	  (reduce #'merge-pathnames (list "testsuite/" name) :from-end t))
	(source-environment
	  `((:summary . ,summary)
	    (:lisp-package-name . "#:${PROJECT_FILENAME}")))
	(testsuite-environment
	  `((:summary . ,summary)
	    (:lisp-package-name . "#:${PROJECT_FILENAME}/testsuite"))))
    (write-template :lisp-source source-pathname source-environment)
    (write-template :lisp-source testsuite-pathname testsuite-environment)))

;;;; End of file `template.lisp'
