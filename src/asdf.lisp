;;;; asdf.lisp — ASDF integration for Atelier

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:atelier)


;;;;
;;;; Project Configuration
;;;;

(defclass project-configuration ()
  ((copyright-holder
    :initarg :copyright-holder
    :reader project-configuration-copyright-holder
    :type (or null string)
    :initform nil
    :documentation "The copyright holder name.")
   (copyright-year
    :initarg :copyright-year
    :reader project-configuration-copyright-year
    :type (or null string)
    :initform nil
    :documentation "The copyright year or year range.")
   (project-filename
    :initarg :project-filename
    :reader project-configuration-project-filename
    :type (or null string)
    :initform nil
    :documentation "The project filename stem used in .asd and directory names.")
   (project-name
    :initarg :project-name
    :reader project-configuration-project-name
    :type (or null string)
    :initform nil
    :documentation "The human-readable project name.")
   (project-description
    :initarg :project-description
    :reader project-configuration-project-description
    :type (or null string)
    :initform nil
    :documentation "A short one-line project description.")
   (project-long-description
    :initarg :project-long-description
    :reader project-configuration-project-long-description
    :type (or null string)
    :initform nil
    :documentation "A longer project description.")
   (homepage
    :initarg :homepage
    :reader project-configuration-homepage
    :type (or null string)
    :initform nil
    :documentation "The project homepage URL.")
   (license
    :initarg :license
    :reader project-configuration-license
    :type (or null string)
    :initform nil
    :documentation "The SPDX license identifier."))
  (:documentation "Project-level configuration for Atelier tools.
Read from a .sexp file declared as an ASDF component. Carries the same
properties as *PARAMETER-BINDINGS* in development.lisp."))

(defun make-project-configuration (&rest initargs
                                   &key copyright-holder copyright-year
                                        project-filename project-name
                                        project-description project-long-description
                                        homepage license)
  "Create and return a PROJECT-CONFIGURATION."
  (declare (ignore copyright-holder copyright-year
                   project-filename project-name
                   project-description project-long-description
                   homepage license))
  (apply #'make-instance 'project-configuration initargs))

(defmethod print-object ((instance project-configuration) stream)
  "Print an unreadable representation of INSTANCE to STREAM."
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~A ~A"
            (or (project-configuration-project-name instance) "no-name")
            (or (project-configuration-license instance) "no-license"))))

(defmethod describe-object ((instance project-configuration) stream)
  "Describe INSTANCE to STREAM."
  (format stream "~&~A is a PROJECT-CONFIGURATION.~%" instance)
  (format stream "~&  Copyright holder: ~A~%" (project-configuration-copyright-holder instance))
  (format stream "~&  Copyright year:   ~A~%" (project-configuration-copyright-year instance))
  (format stream "~&  Project filename: ~A~%" (project-configuration-project-filename instance))
  (format stream "~&  Project name:     ~A~%" (project-configuration-project-name instance))
  (format stream "~&  Description:      ~A~%" (project-configuration-project-description instance))
  (format stream "~&  Homepage:         ~A~%" (project-configuration-homepage instance))
  (format stream "~&  License:          ~A~%" (project-configuration-license instance))
  (terpri stream))

(defun read-project-configuration (pathname)
  "Read a PROJECT-CONFIGURATION from the .sexp file at PATHNAME.
The file must contain a plist. It is read with *READ-EVAL* bound to NIL."
  (declare (type pathname pathname)
           (values project-configuration))
  (let* ((*read-eval* nil)
         (plist (with-open-file (stream pathname :direction :input)
                  (read stream))))
    (apply #'make-project-configuration plist)))


;;;;
;;;; Linter Configuration
;;;;

(defclass linter-configuration ()
  ((disabled-inspectors
    :initarg :disabled-inspectors
    :reader linter-configuration-disabled-inspectors
    :type list
    :initform nil
    :documentation "List of inspector name symbols to skip during linting.")
   (severity-overrides
    :initarg :severity-overrides
    :reader linter-configuration-severity-overrides
    :type list
    :initform nil
    :documentation "Alist of (inspector-name . severity) overrides.")
   (indentation-style
    :initarg :indentation-style
    :reader linter-configuration-indentation-style
    :type (member :spaces :tabs)
    :initform :spaces
    :documentation "The indentation style for the project: :SPACES or :TABS."))
  (:documentation "Linter policy configuration for an ASDF system.
Read from a .sexp file declared as an ASDF component."))

(defun make-linter-configuration (&rest initargs
                                  &key disabled-inspectors severity-overrides
                                       indentation-style)
  "Create and return a LINTER-CONFIGURATION."
  (declare (ignore disabled-inspectors severity-overrides indentation-style))
  (apply #'make-instance 'linter-configuration initargs))

(defmethod print-object ((instance linter-configuration) stream)
  "Print an unreadable representation of INSTANCE to STREAM."
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~D disabled, ~D overrides"
            (length (linter-configuration-disabled-inspectors instance))
            (length (linter-configuration-severity-overrides instance)))))

(defmethod describe-object ((instance linter-configuration) stream)
  "Describe INSTANCE to STREAM."
  (format stream "~&~A is a LINTER-CONFIGURATION.~%" instance)
  (format stream "~&  Disabled inspectors: ~S~%"
          (linter-configuration-disabled-inspectors instance))
  (format stream "~&  Severity overrides:  ~S~%"
          (linter-configuration-severity-overrides instance))
  (terpri stream))

(defun read-linter-configuration (pathname)
  "Read a LINTER-CONFIGURATION from the .sexp file at PATHNAME.
The file must contain a plist. It is read with *READ-EVAL* bound to NIL."
  (declare (type pathname pathname)
           (values linter-configuration))
  (let* ((*read-eval* nil)
         (plist (with-open-file (stream pathname :direction :input)
                  (read stream))))
    (apply #'make-linter-configuration plist)))


;;;;
;;;; ASDF Component Types
;;;;

(defclass asdf-project-configuration (asdf:static-file)
  ()
  (:default-initargs :type "sexp")
  (:documentation "An ASDF component pointing to a project configuration .sexp file."))

(defclass asdf-linter-configuration (asdf:static-file)
  ()
  (:default-initargs :type "sexp")
  (:documentation "An ASDF component pointing to a linter configuration .sexp file."))


;;;;
;;;; Linter Operation
;;;;

(defclass linter-op (asdf:operation)
  ()
  (:documentation "An ASDF operation that lints source files in a system."))

(defvar *linter-findings* nil
  "Accumulator for findings produced during a linter-op run.")

(defvar *current-project-configuration* nil
  "The project configuration for the system currently being linted.")

(defvar *current-linter-configuration* nil
  "The linter configuration for the system currently being linted.")

(defmethod asdf:perform ((operation linter-op) (component asdf:cl-source-file))
  "Inspect the source file of COMPONENT with all registered file-level inspectors."
  (let ((pathname (asdf:component-pathname component)))
    (let ((findings (run-file-inspectors pathname
                                        *current-project-configuration*
                                        *current-linter-configuration*)))
      (setf *linter-findings* (nconc *linter-findings* findings)))))

(defmethod asdf:perform ((operation linter-op) (component asdf:static-file))
  "Do nothing for static files."
  (declare (ignore operation component))
  nil)

(defmethod asdf:perform ((operation linter-op) (component asdf:module))
  "Do nothing for modules — ASDF traverses children automatically."
  (declare (ignore operation component))
  nil)

(defmethod asdf:component-depends-on ((operation linter-op) (component t))
  "Linter-op has no compile/load dependencies."
  nil)

(defun find-system-configuration-component (system component-class)
  "Find the first component of COMPONENT-CLASS in SYSTEM, or NIL."
  (declare (type asdf:system system))
  (flet ((matching-component-p (component)
           (typep component component-class)))
    (find-if #'matching-component-p (asdf:component-children system))))

(defun lint-system (system-designator)
  "Lint all source files in the system designated by SYSTEM-DESIGNATOR.
Return a list of findings. Read project-configuration and linter-configuration
from ASDF components if present."
  (let* ((system (asdf:find-system system-designator))
         (project-config-component
           (find-system-configuration-component system 'asdf-project-configuration))
         (linter-config-component
           (find-system-configuration-component system 'asdf-linter-configuration))
         (*current-project-configuration*
           (when project-config-component
             (read-project-configuration
               (asdf:component-pathname project-config-component))))
         (*current-linter-configuration*
           (when linter-config-component
             (read-linter-configuration
               (asdf:component-pathname linter-config-component))))
         (*linter-findings* nil))
    (asdf:operate 'linter-op system-designator)
    *linter-findings*))

;;;; End of file `asdf.lisp'
