;;;; template.lisp — Templates for the Atelier Lisp System

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)

(defparameter *template-repository-pathname*
  (merge-pathnames "template/" *resourcedir*)
  "The pathname to our template repository.")

(defparameter *template-repository* (make-hash-table)
  "A hash-table with all templates.")

(defclass template nil
  ((name
    :initarg :name
    :reader template-name
    :initform nil
    :documentation "The full name of the template.")
   (identifier
    :initarg :identifier
    :reader template-identifier
    :initform nil
    :documentation "The ID of the template.")
   (description
    :initarg :description
    :reader template-description
    :initform nil
    :documentation "The long description of the template."))
  (:documentation
   "This class presents the various characteristics of a template."))

(defun find-template (designator)
  "Find template by DESIGNATOR in *TEMPLATE-REPOSITORY*."
  (unless (initialized-p)
    (restart-case (error "The template resources are not initialized.")
      (initialize ()
  (initialize))))
  (cond
    ((typep designator 'template)
     designator)
    ((symbolp designator)
     (gethash designator *template-repository*))
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
         (with-slots (name text header identifier) license
     (list (cons :license-name name)
           (cons :license-text text)
           (cons :license-header header)
           (cons :license-identifier (symbol-name identifier)))))))
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
      (remove :license *parameter-bindings*
        :key #'car)
      (license-information)
      (lisp-environment)
      (authorship)))))

(defmethod write-template ((designator symbol) pathname &optional environment)
  "Write the template identified by DESIGNATOR under PATHNAME.
If environment is provided, it is an alist which is added to the current
*PARAMETER-BINDINGS*.

The PATHNAME argument can actually be a pathname, a stream or the value T designating the
standard output."
  (let ((template
    (find-template designator)))
    (unless template
      (error "Cannot find template designated by ~S." designator))
    (write-template template pathname (template-environment environment))))


;;;
;;; File Template
;;;

(defclass file-template (template)
  ((text
    :initarg :text
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

(defun make-file-template (&rest initargs &key identifier name text
                 template-executable-p)
  "Make a file template."
  (declare (ignore identifier name text template-executable-p))
  (apply #'make-instance 'file-template initargs))

(defmethod list-template-parameters ((template file-template))
  (with-slots (text) template
    (list-parameter-names text)))

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
     (parameter-replace (slot-value template 'text) final-environment)))
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
    (with-open-file (stream pathname :direction :output :if-exists :supersede)
      (write-template template stream (append filename-bindings environment)))
    (when (slot-value template 'template-executable-p)
      (uiop:run-program (list "/bin/chmod" "a+x" (namestring pathname))))))


;;;
;;; Composite Template
;;;

(defclass composite-template (template)
  ((components
    :initarg :components
    :reader template-components
    :initform nil
    :documentation "List of templates that form the composite.
Each entry of the list is a list of the form

  (TEMPLATE-DESIGNATOR RELATIVE-PATHNAME ADDITIONAL-BINDINGS)

"))
  (:documentation
   "This class represents a composite of other template."))

(defmethod list-template-parameters ((template composite-template))
  (loop :for component :in (template-components template)
  :for designator = (if (listp component) (first component) component)
  :for pathname = (when (listp component) (second component))
  :append (list-template-parameters designator) :into parameters
  :append (when pathname (list-parameter-names (namestring pathname))) :into parameters
  :finally (return (delete-duplicates parameters
              :test #'parameter-name-equal))))

(defmethod write-template ((template composite-template) (pathname (eql t)) &optional environment)
  (write-template template *standard-output* environment))

(defmethod write-template ((template composite-template) (pathname pathname) &optional environment)
  (labels ((file-component (designator relative-pathname &optional additional-bindings)
       (flet ((resolve-pathname (pathname)
          (pathname (parameter-replace (namestring pathname) environment)))
        (resolve-environment (additional-bindings)
          (let ((resolved-additional-bindings
            (loop :for (key . value) :in additional-bindings
            :for resolved-value = (parameter-replace value environment)
            :collect (cons key resolved-value))))
      (merge-parameter-bindings resolved-additional-bindings environment))))
               (list (or (find-template designator)
       (error "Cannot find template designated by ~S." designator))
         (merge-pathnames (resolve-pathname relative-pathname) pathname)
         (resolve-environment additional-bindings))))
     (composite-component (designator)
       (let ((template
         (find-template designator)))
         (unless template
     (error "Cannot find template designated by ~S." designator))
               (loop :for template-spec :in (template-components template)
         :when (symbolp template-spec)
         :append (composite-component template-spec)
         :when (listp template-spec)
         :collect (apply #'file-component template-spec))))
     (plan ()
       (remove-duplicates
              (composite-component template)
              :key #'second
              :test #'equal
              :from-end t)))
    (loop :for subtask :in (plan)
    :do (apply #'write-template subtask))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-composite-template (identifier &rest initargs &key name description components)
    (declare (ignore name description components))
    (check-type identifier symbol)
    `(setf (gethash ',identifier *template-repository*)
           (make-instance 'composite-template ,':identifier ',identifier ,@initargs))))

(define-composite-template project-files
  :name "Atelier Project Files"
  :description "This template provides basic project files."
  :components
  '((license #p"LICENSE")
    (readme #p"README.md")))

(define-composite-template lisp-system-scaffolding
  :name "Atelier Lisp System Scaffolding"
  :description "This templates provides scaffolding to develop lisp systems.
The basisc structure of this scaffolding features an ASDF definition for
the system and a test, as well as common files for the implementation
and the test: package definition, utilities and entry points."
  :components
  '((lisp-asdf #p"${PROJECT_FILENAME}.asd"
     ((:summary . "System definition for ${PROJECT_NAME}")))
    (lisp-development #p"libexec/lisp/development.lisp"
     ((:summary . "Development System for ${PROJECT_NAME}")))
    (lisp-docstrings #p"libexec/lisp/docstrings.lisp")
    (lisp-system-package #p"src/package.lisp"
     ((:summary . "Package for ${PROJECT_NAME}")))
    (lisp-system-entry-point #p"src/entry-point.lisp"
     ((:summary . "Entry point for ${PROJECT_NAME}")))
    (lisp-source #p"src/utilities.lisp"
     ((:summary . "Utilities for ${PROJECT_NAME}")))
    (lisp-test-package #p"test/package.lisp"
     ((:summary . "Package for ${PROJECT_NAME} test")))
    (lisp-test-entry-point #p"test/entry-point.lisp"
     ((:summary . "Entry point for ${PROJECT_NAME} test")))
    (lisp-source #p"test/utilities.lisp"
     ((:summary . "Utilities for ${PROJECT_NAME} test")
      (:lisp-package-name . "${LISP_TEST_PACKAGE_NAME}")))))

(define-composite-template devops-actions
  :name "Atelier DevOps Actions"
  :description "This template provides scripts for development and operations.
These scripts are merely place hodlers."
  :components
  '((shell-stdlib #p"subr/stdlib.sh")
    (shell-script #p"development/audit"
     ((:summary . "Audit ${PROJECT_NAME}")))
    (shell-script #p"development/build"
     ((:summary . "Build ${PROJECT_NAME}")))
    (shell-script #p"development/clean"
     ((:summary . "Clean ${PROJECT_NAME}")))
    (shell-script #p"development/configure"
     ((:summary . "Configure ${PROJECT_NAME}")))
    (shell-script #p"development/describe"
     ((:summary . "Describe ${PROJECT_NAME}")))
    (shell-script #p"development/lint"
     ((:summary . "Lint ${PROJECT_NAME}")))
    (shell-script #p"development/publish"
     ((:summary . "Publish ${PROJECT_NAME}")))
    (shell-script #p"development/run"
     ((:summary . "Run ${PROJECT_NAME}")))
    (shell-script #p"development/setup"
     ((:summary . "Setup workstation for ${PROJECT_NAME} development")))
    (shell-script #p"development/test"
     ((:summary . "Test for ${PROJECT_NAME}")))
    (shell-script #p"development/verify"
     ((:summary . "Verify ${PROJECT_NAME}")))
    (shell-script #p"operation/audit"
     ((:summary . "Audit for ${PROJECT_NAME} deployments")))
    (shell-script #p"operation/configure"
     ((:summary . "Configure ${PROJECT_NAME} deployments")))
    (shell-script #p"operation/console"
     ((:summary . "Operation console for ${PROJECT_NAME} deployments")))
    (shell-script #p"operation/create"
     ((:summary . "Create a ${PROJECT_NAME} deployment")))
    (shell-script #p"operation/delete"
     ((:summary . "Delete a ${PROJECT_NAME} deployment")))
    (shell-script #p"operation/describe"
     ((:summary . "Describe a ${PROJECT_NAME} deployment")))
    (shell-script #p"operation/dump"
     ((:summary . "Dump the state of a ${PROJECT_NAME} deployment")))
    (shell-script #p"operation/restore"
     ((:summary . "Restore the state of a ${PROJECT_NAME} deployment")))
    (shell-script #p"operation/setup"
     ((:summary . "Setup workstation for ${PROJECT_NAME} deployments")))
    (shell-script #p"operation/start"
     ((:summary . "Start a ${PROJECT_NAME} deployment")))
    (shell-script #p"operation/status"
     ((:summary . "Probe the status of a ${PROJECT_NAME} deployment")))
    (shell-script #p"operation/stop"
     ((:summary . "Stop a ${PROJECT_NAME} deployment")))
    (shell-script #p"operation/update"
     ((:summary . "Update a ${PROJECT_NAME} deployment")))))

(define-composite-template lisp-documentation
  :name "Atelier Lisp Documentation"
  :description "This template provides scripts to build lisp documentation with texinfo"
  :components
  '((lisp-development-makedoc #p"development/makedoc")
    (texinfo #p"doc/${PROJECT_FILENAME}.texinfo")))

(define-composite-template lisp-devops-actions
  :name "Atelier Lisp DevOps Actions"
  :description "This template provides scripts for development and operations.
These scripts are specific to Lisp projects."
  :components
  '((lisp-development-lint #p"development/lint")
    (lisp-development-test #p"development/test")
    (lisp-development-build #p"development/build")))

(define-composite-template lisp-project
  :name "Atelier Lisp Project"
  :description "A complete Lisp project template"
  :components '(project-files
    lisp-system-scaffolding
    devops-actions
    lisp-devops-actions
    lisp-documentation
    (lisp-git-ignore #p".gitignore")))


;;;
;;; Template Repository
;;;

(defun template-repository-load-definition-file (pathname)
  "Load the file template from PATHNAME."
  (unless (uiop:file-exists-p pathname)
    (error "~A: Template definition cannot be read." pathname))
  (multiple-value-bind (front-matter documents)
      (read-file-documents-with-yaml-front-matter pathname)
    (make-instance
     'file-template
     :name (alexandria:assoc-value front-matter :name)
     :template-executable-p (let ((executable-field
            (alexandria:assoc-value front-matter :executable)))
            (when executable-field
        (string= executable-field "true")))
     :identifier (make-keyword (string-upcase (pathname-name pathname)))
     :text (join-lines (first documents)))))

(defun template-repository-list-templates (&optional (template-repository-pathname *template-repository-pathname*))
  "List templates held in TEMPLATE-REPOSITORY-PATHNAME."
  (mapcar #'pathname-name (uiop:directory-files template-repository-pathname "*.text")))

(defun template-repository-load (&key pathname package)
  "Load all templates on TEMPLATE-REPOSITORY-PATHNAME."
  (unless pathname
    (setf pathname *template-repository-pathname*))
  (unless package
    (setf package (symbol-package '*template-repository-pathname*)))
  (loop :for pathname :in (uiop:directory-files pathname "*.text")
  :for designator = (intern (string-upcase (pathname-name pathname)) package)
  :do (setf (gethash designator *template-repository*)
      (template-repository-load-definition-file pathname))))


;;;;
;;;; Lisp Projects
;;;;

(defun sanitize-project-pathname (pathname)
  "Sanitize PATHNAME."
  (truename
   (uiop:ensure-directory-pathname pathname)))

(defun new-lisp-project (pathname &key environment
               copyright-holder copyright-year project-filename project-name
               project-description project-long-description
               homepage license)
  "Create a new lisp project in PATHNAME."
  (let* ((argv-parameter-bindings
           (list (cons :copyright-holder copyright-holder)
     (cons :copyright-year copyright-year)
     (cons :project-filename project-filename)
     (cons :project-name project-name)
     (cons :project-description project-description)
     (cons :project-long-description project-long-description)
     (cons :homepage homepage)
     (cons :license license)))
   (merged-parameter-bindings
     (merge-parameter-bindings *parameter-bindings* argv-parameter-bindings)))
    (let ((*parameter-bindings* merged-parameter-bindings))
      (write-template 'lisp-project (sanitize-project-pathname pathname) environment))))

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
    (write-template 'lisp-source source-pathname source-environment)
    (write-template 'lisp-source test-pathname test-environment)))

;;;; End of file `template.lisp'
