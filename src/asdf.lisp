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
    :documentation "The indentation style for the project: :SPACES or :TABS.")
   (maintainer-overrides
    :initarg :maintainer-overrides
    :reader linter-configuration-maintainer-overrides
    :type list
    :initform nil
    :documentation "Alist of (maintainer-name . disposition) overrides.
Disposition is :AUTO, :INTERACTIVE, or :SKIP."))
  (:documentation "Linter policy configuration for an ASDF system.
Read from a .sexp file declared as an ASDF component."))

(defun make-linter-configuration (&rest initargs
                                  &key disabled-inspectors severity-overrides
                                       indentation-style maintainer-overrides)
  "Create and return a LINTER-CONFIGURATION."
  (declare (ignore disabled-inspectors severity-overrides
                   indentation-style maintainer-overrides))
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
;;;; Parameter Bindings from Project Configuration
;;;;

(defun project-configuration-parameter-bindings (config)
  "Derive *PARAMETER-BINDINGS* alist from a PROJECT-CONFIGURATION.
Maps configuration slots to the parameter names used by the template system."
  (declare (type project-configuration config)
           (values list))
  (list (cons :copyright-holder (project-configuration-copyright-holder config))
        (cons :copyright-year (project-configuration-copyright-year config))
        (cons :project-filename (project-configuration-project-filename config))
        (cons :project-name (project-configuration-project-name config))
        (cons :project-description (project-configuration-project-description config))
        (cons :project-long-description (project-configuration-project-long-description config))
        (cons :homepage (project-configuration-homepage config))
        (cons :license (let ((license (project-configuration-license config)))
                         (when license (make-keyword (string-upcase license)))))))


;;;;
;;;; Resolution Proposed Condition
;;;;

(define-condition resolution-proposed ()
  ((resolution
    :initarg :resolution
    :reader resolution-proposed-resolution
    :documentation "The resolution being proposed.")
   (finding
    :initarg :finding
    :reader resolution-proposed-finding
    :documentation "The finding this resolution addresses.")
   (maintainer-name
    :initarg :maintainer-name
    :reader resolution-proposed-maintainer-name
    :documentation "The symbol naming the maintainer that produced this resolution."))
  (:report (lambda (condition stream)
             (format stream "Resolution proposed by ~S: ~A"
                     (resolution-proposed-maintainer-name condition)
                     (resolution-description
                      (resolution-proposed-resolution condition)))))
  (:documentation "Signalled during autofix before applying a resolution.
Provides APPLY-RESOLUTION and SKIP-RESOLUTION restarts."))

(defun effective-maintainer-disposition (maintainer-name)
  "Return the effective disposition for MAINTAINER-NAME.
Checks *LINTER-CONFIGURATION* overrides first, then falls back to the
maintainer's maturity: :STABLE → :AUTO, :EXPERIMENTAL → :INTERACTIVE."
  (declare (type symbol maintainer-name))
  (let ((override (when *linter-configuration*
                    (assoc maintainer-name
                           (linter-configuration-maintainer-overrides *linter-configuration*)
                           :test #'eq))))
    (if override
        (cdr override)
        (let ((maintainer (find-maintainer maintainer-name)))
          (if (and maintainer (eq :experimental (maintainer-maturity maintainer)))
              :interactive
              :auto)))))


;;;;
;;;; Lint System
;;;;

(defun find-system-configuration-component (system component-class)
  "Find the first component of COMPONENT-CLASS in SYSTEM, or NIL."
  (declare (type asdf:system system))
  (flet ((matching-component-p (component)
           (typep component component-class)))
    (find-if #'matching-component-p (asdf:component-children system))))

(defun atelier-own-system-p (system-name)
  "Return T if SYSTEM-NAME names an Atelier system that needs no configuration warning.
Atelier's own systems have no configuration components by design."
  (declare (type string system-name))
  (or (string= system-name "org.melusina.atelier")
      (string-prefix-p "org.melusina.atelier/" system-name)))

(defun collect-system-source-files (system)
  "Collect all CL-SOURCE-FILE pathnames in SYSTEM, walking the component tree."
  (declare (type asdf:system system))
  (let ((result nil))
    (labels ((walk (component)
               (typecase component
                 (asdf:cl-source-file
                  (push (asdf:component-pathname component) result))
                 (t
                  (dolist (child (asdf:component-children component))
                    (walk child))))))
      (walk system))
    (nreverse result)))

(defun load-system-project-configuration (system)
  "Return a PROJECT-CONFIGURATION for SYSTEM.
Reads the ASDF-PROJECT-CONFIGURATION component when present. For Atelier's
own systems, reads project-configuration.sexp from the system source directory.
Otherwise returns a default instance with a warning."
  (declare (type asdf:system system))
  (let ((component
          (find-system-configuration-component system 'asdf-project-configuration)))
    (cond
      (component
       (read-project-configuration (asdf:component-pathname component)))
      ((atelier-own-system-p (asdf:component-name system))
       (let ((path (merge-pathnames #p"project-configuration.sexp"
                                    (asdf:system-source-directory system))))
         (if (probe-file path)
             (read-project-configuration path)
             (make-project-configuration))))
      (t
       (warn "System ~A declares no project-configuration component; ~
              using defaults. Add an ASDF-PROJECT-CONFIGURATION component ~
              to suppress this warning."
             (asdf:component-name system))
       (make-project-configuration)))))

(defun load-system-linter-configuration (system)
  "Return a LINTER-CONFIGURATION for SYSTEM.
Reads the ASDF-LINTER-CONFIGURATION component when present. For Atelier's
own systems, reads linter-configuration.sexp from the system source directory.
Otherwise returns a default instance with a warning."
  (declare (type asdf:system system))
  (let ((component
          (find-system-configuration-component system 'asdf-linter-configuration)))
    (cond
      (component
       (read-linter-configuration (asdf:component-pathname component)))
      ((atelier-own-system-p (asdf:component-name system))
       (let ((path (merge-pathnames #p"linter-configuration.sexp"
                                    (asdf:system-source-directory system))))
         (if (probe-file path)
             (read-linter-configuration path)
             (make-linter-configuration))))
      (t
       (warn "System ~A declares no linter-configuration component; ~
              using defaults. Add an ASDF-LINTER-CONFIGURATION component ~
              to suppress this warning."
             (asdf:component-name system))
       (make-linter-configuration)))))

(defun lint-system (system-designator &key autofix)
  "Lint all source files in the system designated by SYSTEM-DESIGNATOR.
Return a list of findings. Reads project and linter configuration from
ASDF components when present; otherwise uses sensible defaults.
When AUTOFIX is true, applies automatic resolutions for each finding
that has a registered maintainer, grouped by file, then re-inspects
each modified file until no further fixable findings remain or the
per-file pass limit is reached. Returns the full findings list from
the final inspection pass."
  (let* ((system (asdf:find-system system-designator))
         (*project-configuration* (load-system-project-configuration system))
         (*linter-configuration* (load-system-linter-configuration system)))
    (flet ((inspect-system ()
             ;; Collect findings from every source file in the system.
             (loop :for pathname :in (collect-system-source-files system)
                   :nconc (perform-inspection pathname)))
           (resolutions-for-findings (findings)
             ;; Only line-findings carry line/column positions for TEXT-RESOLUTION.
             ;; Group non-nil production resolutions by file, signalling
             ;; RESOLUTION-PROPOSED for each and respecting dispositions.
             (flet ((production-resolution-p (resolution)
                      (let ((pkg-name (package-name
                                       (symbol-package (resolution-maintainer resolution)))))
                        (not (search "TEST" pkg-name :test #'char-equal))))
                    (accept-resolution-p (resolution)
                      ;; Signal resolution-proposed with restarts.
                      ;; Returns T if the resolution should be applied.
                      (let* ((maint-name (resolution-maintainer resolution))
                             (disposition (effective-maintainer-disposition maint-name)))
                        (case disposition
                          (:skip nil)
                          (:interactive
                           ;; Experimental or interactive: signal as warning.
                           (restart-case
                               (progn
                                 (warn 'resolution-proposed
                                       :resolution resolution
                                       :finding (resolution-finding resolution)
                                       :maintainer-name maint-name)
                                 ;; If warning is not handled, skip in batch.
                                 nil)
                             (apply-resolution () :report "Apply this resolution." t)
                             (skip-resolution () :report "Skip this resolution." nil)))
                          (otherwise
                           ;; :auto — signal and auto-apply.
                           (restart-case
                               (progn
                                 (signal 'resolution-proposed
                                         :resolution resolution
                                         :finding (resolution-finding resolution)
                                         :maintainer-name maint-name)
                                 t)
                             (apply-resolution () :report "Apply this resolution." t)
                             (skip-resolution () :report "Skip this resolution." nil)))))))
               (let ((by-file (make-hash-table :test 'equal)))
                 (dolist (finding findings)
                   (when (typep finding 'line-finding)
                     (dolist (resolution (resolve-finding finding))
                       (when (and (production-resolution-p resolution)
                                  (accept-resolution-p resolution))
                         (push resolution (gethash (finding-file finding) by-file))))))
                 by-file)))
           (apply-all-resolutions (resolutions-by-file)
             ;; Write each file back with its resolutions applied end-to-start.
             (maphash (lambda (pathname resolutions)
                        (apply-resolutions-to-file pathname (nreverse resolutions)))
                      resolutions-by-file)))
      (if (not autofix)
          (inspect-system)
          ;; Autofix loop: inspect → resolve → apply → re-inspect until clean.
          ;; The pass limit guards against a maintainer that reintroduces the
          ;; finding it was supposed to fix.
          (loop :with findings = (inspect-system)
                :repeat 10
                :for resolutions-by-file = (resolutions-for-findings findings)
                :while (plusp (hash-table-count resolutions-by-file))
                :do (apply-all-resolutions resolutions-by-file)
                    (setf findings (inspect-system))
                :finally (return findings))))))


;;;;
;;;; Linter Operation
;;;;

(defclass linter-op (asdf:operation)
  ()
  (:documentation "An ASDF operation that lints source files in a system.
Delegates to LINT-SYSTEM for the actual inspection work."))

(defvar *linter-findings* nil
  "Accumulator for findings produced during a linter-op run.")

(defmethod asdf:perform ((operation linter-op) (component asdf:system))
  "Lint COMPONENT by delegating to LINT-SYSTEM."
  (setf *linter-findings*
        (lint-system (asdf:component-name component))))

(defmethod asdf:perform ((operation linter-op) (component asdf:static-file))
  "Do nothing for static files."
  (declare (ignore operation component))
  nil)

(defmethod asdf:perform ((operation linter-op) (component asdf:module))
  "Do nothing for modules."
  (declare (ignore operation component))
  nil)

(defmethod asdf:component-depends-on ((operation linter-op) (component t))
  "Linter-op has no compile/load dependencies."
  nil)

;;;; End of file `asdf.lisp'
