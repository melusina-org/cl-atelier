;;;; asdf.lisp — ASDF integration for Atelier

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)


;;;;
;;;; Dynamic Context for LINT
;;;;
;;;; These DEFVAR forms must appear before LINT's and INSPECT-LINT-FILES'
;;;; LET* bindings. They were previously in runner.lisp, but asdf.lisp is
;;;; loaded before runner.lisp in the ASDF serial order. Without the
;;;; DEFVAR, SBCL compiles the LET* bindings as lexical and they are
;;;; invisible to callees (check-test-mirror, mirror-eligible-p, etc.).

(defvar *project-configuration* nil
  "The project configuration for the system currently being linted.")

(defvar *linter-configuration* nil
  "The linter configuration for the system currently being linted.")


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
Disposition is :AUTO, :INTERACTIVE, or :SKIP.")
   (mirror-excluded-components
    :initarg :mirror-excluded-components
    :reader linter-configuration-mirror-excluded-components
    :type list
    :initform nil
    :documentation "List of component name strings to exclude from test mirror comparison.
Components listed here are transparent to both MISSING-TEST-COMPONENT-FINDING and
TEST-COMPONENT-ORDER-FINDING. A source component excluded here does not require a
test counterpart; a test component excluded here does not require a source counterpart.
Excluded components are also removed before checking order.")
   (inspector-file-exclusions
    :initarg :inspector-file-exclusions
    :reader linter-configuration-inspector-file-exclusions
    :type list
    :initform nil
    :documentation "Alist mapping inspector-name symbol to a list of file path
strings. A file whose namestring ends with any of the listed paths is skipped
by the corresponding inspector. Paths are compared as suffixes of the absolute
namestring so that paths relative to the project source directory match
naturally."))
  (:documentation "Linter policy configuration for an ASDF system.
Read from a .sexp file declared as an ASDF component."))

(defun make-linter-configuration (&rest initargs
                                  &key disabled-inspectors severity-overrides
                                       indentation-style maintainer-overrides
                                       mirror-excluded-components
                                       inspector-file-exclusions)
  "Create and return a LINTER-CONFIGURATION."
  (declare (ignore disabled-inspectors severity-overrides
                   indentation-style maintainer-overrides
                   mirror-excluded-components
                   inspector-file-exclusions))
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
The file must contain a plist. It is read with *READ-EVAL* bound to NIL
and *PACKAGE* bound to :ATELIER so that bare symbols such as inspector
and maintainer names resolve to the Atelier package without requiring
explicit package qualification."
  (declare (type pathname pathname)
           (values linter-configuration))
  (let* ((*read-eval* nil)
         (*package* (find-package :atelier))
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

(define-condition resolution-proposed (warning)
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
             (let* ((resolution (resolution-proposed-resolution condition))
                    (finding (resolution-proposed-finding condition))
                    (maint-name (resolution-proposed-maintainer-name condition)))
               (format stream "~A proposes: ~A~%  Finding: ~A~%  File: ~A~%  ~
                               Invoke APPLY-RESOLUTION to accept this fix, ~
                               SKIP-RESOLUTION to leave the code unchanged."
                       maint-name
                       (resolution-description resolution)
                       (finding-observation finding)
                       (when (typep finding 'file-finding)
                         (finding-file finding))))))
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

(defun collect-sibling-systems (system)
  "Return all ASDF systems defined in the same .asd file as SYSTEM.
Includes SYSTEM itself. Returns a list of ASDF:SYSTEM objects."
  (declare (type asdf:system system))
  (let* ((asd-file (asdf:system-source-file system))
         (asd-truename (when asd-file (truename asd-file)))
         (siblings nil))
    (when asd-truename
      (asdf:map-systems
       (lambda (sys)
         (let ((sys-file (asdf:system-source-file sys)))
           (when (and sys-file
                      (equal (truename sys-file) asd-truename))
             (push sys siblings))))))
    (nreverse siblings)))

(defun collect-all-source-files (system)
  "Collect source files from all systems in SYSTEM's .asd file, plus the .asd file itself.
Returns a deduplicated list of pathnames. The .asd file is included first so that
file-level inspectors (e.g. check-system-naming) can inspect the system definitions."
  (declare (type asdf:system system))
  (let* ((asd-file (asdf:system-source-file system))
         (siblings (collect-sibling-systems system))
         (seen (make-hash-table :test 'equal))
         (result nil))
    ;; Include the .asd file first
    (when asd-file
      (let ((asd-truename (truename asd-file)))
        (setf (gethash asd-truename seen) t)
        (push asd-truename result)))
    ;; Collect source files from all sibling systems
    (dolist (sibling siblings)
      (dolist (pathname (collect-system-source-files sibling))
        (let ((truename (truename pathname)))
          (unless (gethash truename seen)
            (setf (gethash truename seen) t)
            (push truename result)))))
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

(defun find-component-file (system deprecated-name)
  "Find the source file for a component named DEPRECATED-NAME in SYSTEM.
Walks the source directory looking for DEPRECATED-NAME.lisp files.
Returns a list of matching pathnames."
  (declare (type asdf:system system)
           (type string deprecated-name)
           (values list))
  (let ((source-dir (asdf:system-source-directory system))
        (results nil))
    (flet ((collect-match (pathname)
             (when (string-equal deprecated-name (pathname-name pathname))
               (push pathname results))))
      (dolist (sibling (collect-sibling-systems system))
        (dolist (pathname (collect-system-source-files sibling))
          (collect-match pathname))))
    ;; Also check the source directory for files not in any system
    (let ((pattern (merge-pathnames
                    (make-pathname :directory '(:relative :wild-inferiors)
                                  :name deprecated-name
                                  :type "lisp")
                    source-dir)))
      (dolist (pathname (directory pattern))
        (unless (member pathname results :test #'equal)
          (push pathname results))))
    (nreverse results)))

(defun perform-component-renames (resolutions-by-file system)
  "Rename source files for accepted deprecated-component-name resolutions.
Scans RESOLUTIONS-BY-FILE for FIX-DEPRECATED-COMPONENT-NAME resolutions
and renames the corresponding .lisp files on disk."
  (declare (type hash-table resolutions-by-file)
           (type asdf:system system))
  (maphash
   (lambda (pathname resolutions)
     (declare (ignore pathname))
     (dolist (resolution resolutions)
       (when (eq 'fix-deprecated-component-name (resolution-maintainer resolution))
         (let* ((finding (resolution-finding resolution))
                (observation (finding-observation finding))
                (name-start (position #\" observation))
                (name-end (when name-start
                            (position #\" observation :start (1+ name-start))))
                (deprecated-name (when (and name-start name-end)
                                   (subseq observation (1+ name-start) name-end)))
                (replacement (when deprecated-name
                               (deprecated-component-replacement deprecated-name))))
           (when (and deprecated-name replacement)
             (dolist (old-path (find-component-file system deprecated-name))
               (let ((new-path (make-pathname :name replacement
                                              :defaults old-path)))
                 (unless (probe-file new-path)
                   (rename-file old-path new-path)))))))))
   resolutions-by-file))

;;;
;;; LINT is composed of four primitives:
;;;
;;;   COLLECT-LINT-FILES    system → list of pathnames.
;;;   INSPECT-LINT-FILES    pathnames → findings.
;;;   PLAN-RESOLUTIONS      findings → resolutions.
;;;   APPLY-LINT-RESOLUTIONS resolutions → list of rewritten pathnames.
;;;
;;; LINT itself is a thin orchestrator over these four. Each primitive is
;;; a pure-ish function (INSPECT-LINT-FILES, APPLY-LINT-RESOLUTIONS touch
;;; the filesystem; the other two do not) and may be composed by callers
;;; who need something other than the DWIM default.
;;;

(defun collect-lint-files (system-designator &key (scope :system))
  "Return the pathnames the linter inspects for SYSTEM-DESIGNATOR at SCOPE.
SCOPE is :SYSTEM (default — the requested system's source files plus
its .asd file) or :PROJECT (additionally, every source file of every
system declared in the same .asd). The .asd file is always included so
file-level inspectors such as CHECK-SYSTEM-NAMING and CHECK-TEST-MIRROR
continue to have the system declarations to work on."
  (declare (type (member :system :project) scope)
           (values list))
  (let ((system (asdf:find-system system-designator)))
    (ecase scope
      (:system
       (let ((asd-file (asdf:system-source-file system))
             (sources (collect-system-source-files system)))
         (if asd-file
             (cons (truename asd-file) sources)
             sources)))
      (:project
       (collect-all-source-files system)))))

(defun inspect-lint-files (pathnames &key system-designator)
  "Run the inspection pipeline over PATHNAMES and return the list of findings.
When SYSTEM-DESIGNATOR is supplied, project and linter configuration
are loaded from that system and bound dynamically while inspecting so
per-project disabled inspectors, severity overrides, and inspector
file exclusions apply. When NIL, inspectors run under whatever
*PROJECT-CONFIGURATION* and *LINTER-CONFIGURATION* the caller has
already bound (or the default NIL bindings)."
  (declare (type list pathnames)
           (values list))
  (flet ((inspect-all ()
           (loop :for pathname :in pathnames :nconc (perform-inspection pathname))))
    (if system-designator
        (let* ((system (asdf:find-system system-designator))
               (*project-configuration* (load-system-project-configuration system))
               (*linter-configuration* (load-system-linter-configuration system)))
          (inspect-all))
        (inspect-all))))

(defun plan-resolutions (findings)
  "Return the resolutions that the linter would apply for FINDINGS.
Walks each finding's registered maintainers via RESOLVE-FINDING. Only
LINE-FINDING instances yield resolutions because TEXT-RESOLUTION needs
line/column positions to compute its write-back span. Drops resolutions
produced by test-package maintainers (a safeguard against test fixtures
shadowing the production pipeline) and resolutions whose maintainer's
effective disposition is :SKIP. Returns the eligible resolutions in
finding order; does not signal RESOLUTION-PROPOSED. For interactive
acceptance see LINT :action :fix, which layers signalling on top of
this planning step.

The effective disposition is read from *LINTER-CONFIGURATION*, which
the caller must have bound (LINT binds it from the system designator
before calling this function)."
  (declare (type list findings)
           (values list))
  (flet ((production-resolution-p (resolution)
           (let ((pkg (symbol-package (resolution-maintainer resolution))))
             (and pkg
                  (not (search "TEST" (package-name pkg) :test #'char-equal)))))
         (eligible-resolution-p (resolution)
           (not (eq :skip (effective-maintainer-disposition
                           (resolution-maintainer resolution))))))
    (loop :for finding :in findings
          :when (typep finding 'line-finding)
            :nconc (loop :for resolution :in (resolve-finding finding)
                         :when (and (production-resolution-p resolution)
                                    (eligible-resolution-p resolution))
                           :collect resolution))))

(defun apply-lint-resolutions (resolutions)
  "Apply RESOLUTIONS to their files atomically and return the rewritten pathnames.
Resolutions are grouped by their finding's FINDING-FILE; each file is
rewritten once with every applicable resolution applied end-to-start
via APPLY-RESOLUTIONS-TO-FILE. Returns a list of the pathnames that
were written. Does not perform post-write file renames — those are the
responsibility of LINT :action :fix which knows the system context
needed for component-name renames."
  (declare (type list resolutions)
           (values list))
  (let ((by-file (make-hash-table :test 'equal))
        (written nil))
    (dolist (resolution resolutions)
      (push resolution
            (gethash (finding-file (resolution-finding resolution)) by-file)))
    (maphash (lambda (pathname file-resolutions)
               (apply-resolutions-to-file pathname (nreverse file-resolutions))
               (push pathname written))
             by-file)
    (nreverse written)))

(defun interactively-accept-resolutions (resolutions)
  "Signal RESOLUTION-PROPOSED for each resolution and return those accepted.
:AUTO-disposed maintainers SIGNAL and default to accepted; :INTERACTIVE-
disposed maintainers WARN and default to rejected. Both expose
APPLY-RESOLUTION and SKIP-RESOLUTION restarts so a handler-bind in the
caller can override the default. Used internally by LINT :action :fix."
  (declare (type list resolutions)
           (values list))
  (flet ((accepted-p (resolution)
           (let* ((finding (resolution-finding resolution))
                  (maint-name (resolution-maintainer resolution))
                  (disposition (effective-maintainer-disposition maint-name))
                  (description (resolution-description resolution))
                  (apply-report
                    (format nil "Apply fix: ~A" description))
                  (skip-report
                    (format nil "Skip ~A on ~A."
                            maint-name
                            (when (typep finding 'file-finding)
                              (file-namestring (finding-file finding))))))
             (case disposition
               (:skip nil)
               (:interactive
                (restart-case
                    (progn
                      (warn 'resolution-proposed
                            :resolution resolution
                            :finding finding
                            :maintainer-name maint-name)
                      nil)
                  (apply-resolution ()
                    :report (lambda (s) (write-string apply-report s))
                    t)
                  (skip-resolution ()
                    :report (lambda (s) (write-string skip-report s))
                    nil)))
               (otherwise
                (restart-case
                    (progn
                      (signal 'resolution-proposed
                              :resolution resolution
                              :finding finding
                              :maintainer-name maint-name)
                      t)
                  (apply-resolution ()
                    :report (lambda (s) (write-string apply-report s))
                    t)
                  (skip-resolution ()
                    :report (lambda (s) (write-string skip-report s))
                    nil)))))))
    (remove-if-not #'accepted-p resolutions)))

(defun perform-component-renames-from-resolutions (resolutions system)
  "Rename source files on disk for accepted FIX-DEPRECATED-COMPONENT-NAME resolutions."
  (declare (type list resolutions)
           (type asdf:system system))
  (let ((by-file (make-hash-table :test 'equal)))
    (dolist (resolution resolutions)
      (push resolution
            (gethash (finding-file (resolution-finding resolution)) by-file)))
    (perform-component-renames by-file system)))

(defun lint (system-designator &key (action :fix) (scope :system))
  "Lint SYSTEM-DESIGNATOR and return findings or resolutions by ACTION.

ACTION is one of:
  :INSPECT — inspect only; return the findings list; write no files.
  :PREVIEW — inspect and plan resolutions; return the resolutions that
             would be applied under :AUTO disposition; write no files.
  :FIX     — inspect, plan, and apply resolutions; iterate until no
             further resolutions are accepted or the per-pass limit of
             10 is reached. Each resolution signals RESOLUTION-PROPOSED
             with APPLY-RESOLUTION and SKIP-RESOLUTION restarts (as a
             SIGNAL for :AUTO dispositions, as a WARN for :INTERACTIVE).
             Returns the remaining findings from the final pass.
             (Default.)

SCOPE is one of:
  :SYSTEM  — only the requested system's source files plus its .asd
             file. (Default.)
  :PROJECT — every source file of every system declared in the same
             .asd file.

Any other value for ACTION or SCOPE signals a TYPE-ERROR.

Project and linter configuration are loaded from SYSTEM-DESIGNATOR and
bound dynamically for the duration of the call."
  (declare (type (member :inspect :preview :fix) action)
           (type (member :system :project) scope)
           (values list))
  (let* ((system (asdf:find-system system-designator))
         (*project-configuration* (load-system-project-configuration system))
         (*linter-configuration* (load-system-linter-configuration system))
         (pathnames (collect-lint-files system :scope scope)))
    (ecase action
      (:inspect
       (inspect-lint-files pathnames))
      (:preview
       (plan-resolutions (inspect-lint-files pathnames)))
      (:fix
       ;; Autofix loop: inspect → plan → accept → apply → re-inspect.
       ;; The pass limit guards against a maintainer that reintroduces
       ;; the finding it was supposed to fix.
       (loop :with findings = (inspect-lint-files pathnames)
             :repeat 10
             :for accepted = (interactively-accept-resolutions
                              (plan-resolutions findings))
             :while accepted
             :do (apply-lint-resolutions accepted)
                 (perform-component-renames-from-resolutions accepted system)
                 (setf findings (inspect-lint-files pathnames))
             :finally (return findings))))))


;;;;
;;;; Lint Operation
;;;;

(defclass lint-op (asdf:non-propagating-operation)
  ()
  (:documentation "An ASDF operation that lints source files in a system.
Delegates to LINT for the actual inspection work. Inheriting from
ASDF:NON-PROPAGATING-OPERATION tells the plan walker the operation has
no dependencies — linting a system does not require linting its
dependencies — which is required by ASDF 3.1 and silences the
propagation-scheme warning."))

(defvar *linter-findings* nil
  "Accumulator for findings produced during a lint-op run.")

(defmethod asdf:perform ((operation lint-op) (component asdf:system))
  "Lint COMPONENT by delegating to LINT with the default :FIX action and :SYSTEM scope."
  (setf *linter-findings*
        (lint (asdf:component-name component))))

(defmethod asdf:perform ((operation lint-op) (component asdf:static-file))
  "Do nothing for static files."
  (declare (ignore operation component))
  nil)

(defmethod asdf:perform ((operation lint-op) (component asdf:module))
  "Do nothing for modules."
  (declare (ignore operation component))
  nil)

;;;; End of file `asdf.lisp'
