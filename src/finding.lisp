;;;; finding.lisp — Finding class hierarchy for Atelier

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
;;;; Finding
;;;;

(defclass finding ()
  ((inspector
    :initarg :inspector
    :reader finding-inspector
    :type symbol
    :documentation "The inspector symbol that produced this finding.")
   (severity
    :initarg :severity
    :reader finding-severity
    :type (member :error :warning :info :style)
    :documentation "The severity of this finding.")
   (observation
    :initarg :observation
    :reader finding-observation
    :type string
    :documentation "Per-instance objective statement of what was measured.")
   (rationale
    :initarg :rationale
    :reader finding-rationale
    :type string
    :documentation "Why this measurement warrants attention. Per-class by convention."))
  (:documentation "Base class for all diagnostic findings."))


;;;;
;;;; File Finding
;;;;

(defclass file-finding (finding)
  ((file
    :initarg :file
    :reader finding-file
    :type pathname
    :documentation "The pathname of the affected file."))
  (:documentation "A finding that applies to a whole file."))

(defun make-file-finding (&rest initargs &key inspector severity observation rationale file)
  "Create and return a FILE-FINDING."
  (declare (ignore inspector severity observation rationale file))
  (apply #'make-instance 'file-finding initargs))

(defmethod print-object ((instance file-finding) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~S ~S ~A"
            (finding-severity instance)
            (finding-inspector instance)
            (finding-file instance))))

(defmethod describe-object ((instance file-finding) stream)
  (format stream "~&~A is a FILE-FINDING.~%" instance)
  (format stream "~&  Inspector:   ~S~%" (finding-inspector instance))
  (format stream "~&  Severity:    ~S~%" (finding-severity instance))
  (format stream "~&  File:        ~A~%" (finding-file instance))
  (format stream "~&  Observation: ~A~%" (finding-observation instance))
  (format stream "~&  Rationale:   ~A~%" (finding-rationale instance))
  (terpri stream))


;;;;
;;;; Line Finding
;;;;

(defclass line-finding (file-finding)
  ((line
    :initarg :line
    :reader finding-line
    :type (integer 1)
    :documentation "The line number of the finding.")
   (column
    :initarg :column
    :reader finding-column
    :type (integer 0)
    :documentation "The column number of the finding.")
   (end-line
    :initarg :end-line
    :reader finding-end-line
    :type (integer 1)
    :documentation "The end line number of the finding.")
   (end-column
    :initarg :end-column
    :reader finding-end-column
    :type (integer 0)
    :documentation "The end column number of the finding.")
   (source-text
    :initarg :source-text
    :reader finding-source-text
    :type string
    :documentation "The source text at the finding location."))
  (:documentation "A finding that applies to a specific line range in a file."))

(defun make-line-finding (&rest initargs
                          &key inspector severity observation rationale file
                               line column end-line end-column source-text)
  "Create and return a LINE-FINDING."
  (declare (ignore inspector severity observation rationale file
                   line column end-line end-column source-text))
  (apply #'make-instance 'line-finding initargs))

(defmethod print-object ((instance line-finding) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~S ~S ~A:~D"
            (finding-severity instance)
            (finding-inspector instance)
            (finding-file instance)
            (finding-line instance))))

(defmethod describe-object ((instance line-finding) stream)
  (format stream "~&~A is a LINE-FINDING.~%" instance)
  (format stream "~&  Inspector:   ~S~%" (finding-inspector instance))
  (format stream "~&  Severity:    ~S~%" (finding-severity instance))
  (format stream "~&  File:        ~A~%" (finding-file instance))
  (format stream "~&  Line:        ~D–~D~%" (finding-line instance) (finding-end-line instance))
  (format stream "~&  Column:      ~D–~D~%" (finding-column instance) (finding-end-column instance))
  (format stream "~&  Source:      ~S~%" (finding-source-text instance))
  (format stream "~&  Observation: ~A~%" (finding-observation instance))
  (format stream "~&  Rationale:   ~A~%" (finding-rationale instance))
  (terpri stream))


;;;;
;;;; Region Finding
;;;;

(defclass region-finding (file-finding)
  ((start-line
    :initarg :start-line
    :reader finding-start-line
    :type (integer 1)
    :documentation "The start line of the region.")
   (end-line
    :initarg :end-line
    :reader finding-end-line
    :type (integer 1)
    :documentation "The end line of the region.")
   (source-text
    :initarg :source-text
    :reader finding-source-text
    :type string
    :documentation "The source text of the region."))
  (:documentation
   "A finding that applies to a multi-line region.
Used by external tool wrappers (ShellCheck, tflint) that report
diagnostics spanning multiple lines without precise column information."))

(defun make-region-finding (&rest initargs
                            &key inspector severity observation rationale file
                                 start-line end-line source-text)
  "Create and return a REGION-FINDING."
  (declare (ignore inspector severity observation rationale file
                   start-line end-line source-text))
  (apply #'make-instance 'region-finding initargs))

(defmethod print-object ((instance region-finding) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~S ~S ~A:~D-~D"
            (finding-severity instance)
            (finding-inspector instance)
            (finding-file instance)
            (finding-start-line instance)
            (finding-end-line instance))))

(defmethod describe-object ((instance region-finding) stream)
  (format stream "~&~A is a REGION-FINDING.~%" instance)
  (format stream "~&  Inspector:   ~S~%" (finding-inspector instance))
  (format stream "~&  Severity:    ~S~%" (finding-severity instance))
  (format stream "~&  File:        ~A~%" (finding-file instance))
  (format stream "~&  Lines:       ~D–~D~%" (finding-start-line instance) (finding-end-line instance))
  (format stream "~&  Source:      ~S~%" (finding-source-text instance))
  (format stream "~&  Observation: ~A~%" (finding-observation instance))
  (format stream "~&  Rationale:   ~A~%" (finding-rationale instance))
  (terpri stream))


;;;;
;;;; Syntax Finding
;;;;

(defclass syntax-finding (line-finding)
  ((cst-node
    :initarg :cst-node
    :reader finding-cst-node
    :type concrete-syntax-tree:cst
    :documentation "The CST node where the finding was detected.")
   (cst-root
    :initarg :cst-root
    :reader finding-cst-root
    :type concrete-syntax-tree:cst
    :documentation "The root CST node of the parsed file."))
  (:documentation "A finding at the concrete syntax tree level."))

(defun make-syntax-finding (&rest initargs
                            &key inspector severity observation rationale file
                                 line column end-line end-column source-text
                                 cst-node cst-root)
  "Create and return a SYNTAX-FINDING."
  (declare (ignore inspector severity observation rationale file
                   line column end-line end-column source-text
                   cst-node cst-root))
  (apply #'make-instance 'syntax-finding initargs))

(defmethod print-object ((instance syntax-finding) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~S ~S ~A:~D"
            (finding-severity instance)
            (finding-inspector instance)
            (finding-file instance)
            (finding-line instance))))

(defmethod describe-object ((instance syntax-finding) stream)
  (format stream "~&~A is a SYNTAX-FINDING.~%" instance)
  (format stream "~&  Inspector:   ~S~%" (finding-inspector instance))
  (format stream "~&  Severity:    ~S~%" (finding-severity instance))
  (format stream "~&  File:        ~A~%" (finding-file instance))
  (format stream "~&  Line:        ~D–~D~%" (finding-line instance) (finding-end-line instance))
  (format stream "~&  CST Node:    ~S~%" (finding-cst-node instance))
  (format stream "~&  Observation: ~A~%" (finding-observation instance))
  (format stream "~&  Rationale:   ~A~%" (finding-rationale instance))
  (terpri stream))


;;;;
;;;; Concrete Finding Subclasses
;;;;

(defmacro define-finding (name parent documentation)
  "Define a concrete finding subclass NAME inheriting from PARENT."
  `(defclass ,name (,parent) () (:documentation ,documentation)))

(defmacro define-findings (&rest specs)
  "Define multiple concrete finding subclasses.
Each SPEC is (PARENT NAME DOCUMENTATION), ordered so that lines sort
meaningfully by parent class."
  `(progn
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (parent name documentation) spec
                   `(define-finding ,name ,parent ,documentation)))
               specs)))

(define-findings
  (file-finding   encoding-finding             "A file is not valid UTF-8.")
  (file-finding   spdx-license-header-finding  "A file has a missing or incorrect SPDX license identifier.")
  (line-finding   line-too-long-finding        "A source line exceeds the maximum length.")
  (line-finding   mixed-indentation-finding    "A source line uses the wrong indentation character.")
  (line-finding   trailing-whitespace-finding  "A source line has trailing whitespace.")
  (syntax-finding bare-lambda-finding          "A higher-order function call uses a bare LAMBDA instead of a named FLET function.")
  (syntax-finding bare-loop-keyword-finding    "A LOOP form uses a bare symbol for a clause keyword instead of a keyword symbol.")
  (syntax-finding constant-naming-finding      "A DEFCONSTANT constant name lacks the +plus-surrounded+ convention.")
  (syntax-finding earmuffs-finding             "A DEFVAR or DEFPARAMETER variable name lacks the *earmuffs* convention."))

;;;; End of file `finding.lisp'
