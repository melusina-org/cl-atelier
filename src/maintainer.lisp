;;;; maintainer.lisp — Maintainer registry with superseding for Atelier

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
;;;; Maintainer
;;;;

(defclass maintainer ()
  ((name
    :initarg :name
    :reader maintainer-name
    :type symbol
    :documentation "The symbol identifying this maintainer.")
   (supersedes
    :initarg :supersedes
    :reader maintainer-supersedes
    :type list
    :initform nil
    :documentation "List of maintainer names this maintainer supersedes.")
   (kind
    :initarg :kind
    :reader maintainer-kind
    :type (member :automatic :agent)
    :documentation "Whether this maintainer is automatic or agent-assisted.")
   (description
    :initarg :description
    :reader maintainer-description
    :type (or null string)
    :initform nil
    :documentation "A human-readable description of the maintainer."))
  (:documentation "Base class for all maintainers.
Each concrete maintainer is a subclass with a singleton instance in
the registry. Maintainers participate in a superseding partial order."))

(defmethod print-object ((instance maintainer) stream)
  "Print an unreadable representation of INSTANCE to STREAM."
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~S ~S"
            (maintainer-kind instance)
            (maintainer-name instance))))

(defmethod describe-object ((instance maintainer) stream)
  "Describe INSTANCE to STREAM."
  (format stream "~&~A is a MAINTAINER.~%" instance)
  (format stream "~&  Name:        ~S~%" (maintainer-name instance))
  (format stream "~&  Kind:        ~S~%" (maintainer-kind instance))
  (format stream "~&  Supersedes:  ~S~%" (maintainer-supersedes instance))
  (format stream "~&  Description: ~A~%" (maintainer-description instance))
  (terpri stream))


;;;;
;;;; Kind Subclasses
;;;;

(defclass automatic-maintainer (maintainer)
  ()
  (:default-initargs :kind :automatic)
  (:documentation "A maintainer that can apply fixes automatically."))

(defclass agent-maintainer (maintainer)
  ()
  (:default-initargs :kind :agent)
  (:documentation "A maintainer that produces prompts for an LLM or coding agent."))


;;;;
;;;; Registry
;;;;

(defvar *maintainers* (make-hash-table :test 'eq)
  "Hash table mapping maintainer names to maintainer singleton instances.")

(defun find-maintainer (designator)
  "Find the maintainer designated by DESIGNATOR.
DESIGNATOR is either a MAINTAINER instance (returned as-is) or a symbol
used to look up the registry. Return NIL when no maintainer is found."
  (declare (type (or maintainer symbol) designator))
  (etypecase designator
    (maintainer designator)
    (symbol (gethash designator *maintainers*))))

(defun symbol-maintainer (symbol)
  "Return the maintainer registered under SYMBOL.
Signal an error if no maintainer is registered under SYMBOL."
  (declare (type symbol symbol))
  (multiple-value-bind (instance present-p) (gethash symbol *maintainers*)
    (if present-p instance (error "UNBOUND-MAINTAINER ~S" symbol))))

(defun (setf symbol-maintainer) (new-value symbol)
  "Register NEW-VALUE as the maintainer for SYMBOL."
  (setf (gethash symbol *maintainers*) new-value))

(defun list-maintainers ()
  "Return a list of all registered maintainer symbols."
  (loop :for name :being :the :hash-key :of *maintainers*
        :collect name))


;;;;
;;;; Resolution Protocol
;;;;

(defgeneric prepare-resolution (maintainer finding)
  (:documentation
   "Return a RESOLUTION if MAINTAINER can handle FINDING, or NIL.
The default method returns NIL. DEFINE-MAINTAINER generates methods
specialised on both the maintainer subclass and the finding type.")
  (:method ((maintainer maintainer) (finding finding))
    nil))


;;;;
;;;; DEFINE-MAINTAINER Macro
;;;;

(defmacro define-maintainer (name direct-superclasses lambda-list &body body)
  "Define and register a maintainer named NAME.
DIRECT-SUPERCLASSES is a list of superclasses (must include a kind
class such as AUTOMATIC-MAINTAINER). LAMBDA-LIST is the method
parameter list for PREPARE-RESOLUTION. BODY begins with an optional
docstring, followed by keyword options, then body forms.

Keyword options:
  (:supersedes LIST-OF-NAMES) — maintainer names this one supersedes.

Example:
  (define-maintainer fix-spdx-license-header (automatic-maintainer)
      ((finding spdx-license-header-finding))
    \"Insert the correct SPDX-License-Identifier header.\"
    (:supersedes '(basic-license-fixer))
    (make-text-resolution ...))"
  (check-type name symbol)
  (multiple-value-bind (docstring options body-forms)
      (parse-define-body body)
    (let ((supersedes (second (find :supersedes options :key #'first))))
      `(progn
         (defclass ,name (,@direct-superclasses)
           ()
           ,@(when docstring
               `((:documentation ,docstring))))
         (setf (gethash ',name *maintainers*)
               (make-instance ',name
                 :name ',name
                 :supersedes ,supersedes
                 :description ,docstring))
         ,@(when body-forms
             `((defmethod prepare-resolution ((maintainer ,name) ,@lambda-list)
                 ,@(when docstring (list docstring))
                 ,@body-forms)))
         ',name))))

(defmacro define-automatic-maintainer (name lambda-list &body body)
  "Define an automatic maintainer. Convenience wrapper for DEFINE-MAINTAINER."
  `(define-maintainer ,name (automatic-maintainer) ,lambda-list ,@body))

(defmacro define-agent-maintainer (name lambda-list &body body)
  "Define an agent-assisted maintainer. Convenience wrapper for DEFINE-MAINTAINER."
  `(define-maintainer ,name (agent-maintainer) ,lambda-list ,@body))


;;;;
;;;; Superseding
;;;;

(defun check-superseding-cycles (name supersedes)
  "Signal an error if adding NAME with SUPERSEDES would create a cycle."
  (declare (type symbol name)
           (type list supersedes))
  (labels ((walk (current visited)
             (when (eq current name)
               (error "Superseding cycle detected: ~S eventually supersedes itself." name))
             (unless (member current visited :test #'eq)
               (let ((current-maintainer (find-maintainer current)))
                 (when current-maintainer
                   (let ((new-visited (cons current visited)))
                     (loop :for superseded-name :in (maintainer-supersedes current-maintainer)
                           :do (walk superseded-name new-visited))))))))
    (loop :for superseded-name :in supersedes
          :do (walk superseded-name nil))))

(defun maximal-maintainers (candidates)
  "Return the maximal elements of CANDIDATES under the superseding partial order.
A maintainer is maximal if no other candidate supersedes it."
  (declare (type list candidates))
  (flet ((superseded-p (candidate)
           (loop :for other :in candidates
                 :thereis (member (maintainer-name candidate)
                                  (maintainer-supersedes other)
                                  :test #'eq))))
    (remove-if #'superseded-p candidates)))

(defun resolve-finding (finding)
  "Collect resolutions from all maximal maintainers for FINDING.
Walk the maintainer registry, compute the maximal elements of the
superseding partial order, call PREPARE-RESOLUTION on each singleton
instance, and return the list of non-NIL resolutions. Finding-type
dispatch is handled by CLOS method specialisation on the maintainer
and finding classes."
  (declare (type finding finding))
  (let* ((all-maintainers
           (loop :for name :being :the :hash-key :of *maintainers*
                 :collect (gethash name *maintainers*)))
         (maximal (maximal-maintainers all-maintainers)))
    (loop :for current-maintainer :in maximal
          :for resolution = (prepare-resolution current-maintainer finding)
          :when resolution
          :collect resolution)))

;;;; End of file `maintainer.lisp'
