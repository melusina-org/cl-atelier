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
  (:documentation "A registered maintainer that can resolve findings.
Maintainers participate in a superseding partial order. When two
maintainers can both handle a finding, only the maximal elements
of the superseding order run."))

(defun make-maintainer (&rest initargs &key name supersedes kind description)
  "Create and return a MAINTAINER."
  (declare (ignore name supersedes kind description))
  (apply #'make-instance 'maintainer initargs))

(defmethod print-object ((instance maintainer) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~S ~S"
            (maintainer-kind instance)
            (maintainer-name instance))))

(defmethod describe-object ((instance maintainer) stream)
  (format stream "~&~A is a MAINTAINER.~%" instance)
  (format stream "~&  Name:        ~S~%" (maintainer-name instance))
  (format stream "~&  Kind:        ~S~%" (maintainer-kind instance))
  (format stream "~&  Supersedes:  ~S~%" (maintainer-supersedes instance))
  (format stream "~&  Description: ~A~%" (maintainer-description instance))
  (terpri stream))


;;;;
;;;; Registry
;;;;

(defvar *maintainers* (make-hash-table :test 'eq)
  "Hash table mapping maintainer names to maintainer instances.")

(defun find-maintainer (designator)
  "Find the maintainer designated by DESIGNATOR.
DESIGNATOR is either a MAINTAINER instance (returned as-is) or a symbol
used to look up the registry."
  (etypecase designator
    (maintainer designator)
    (symbol (gethash designator *maintainers*))))

(defun symbol-maintainer (symbol)
  "Return the maintainer registered under SYMBOL.
Signals an error if no maintainer is registered under SYMBOL."
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

(defgeneric prepare-resolution (maintainer-name finding)
  (:documentation
   "Return a RESOLUTION if MAINTAINER-NAME can handle FINDING, or NIL.
MAINTAINER-NAME is a symbol identifying the maintainer. The default
method returns NIL. DEFINE-MAINTAINER generates methods specialised
on both the maintainer name (via EQL) and the finding type.")
  (:method ((maintainer-name symbol) (finding finding))
    nil))


;;;;
;;;; DEFINE-MAINTAINER Macro
;;;;

(defun parse-maintainer-body (body)
  "Parse the body of DEFINE-MAINTAINER into docstring, options, and body forms.
Returns three values: the docstring (or NIL), an alist of keyword options,
and the remaining body forms."
  (declare (type list body))
  (let ((docstring nil)
        (options nil)
        (remaining body))
    (when (stringp (first remaining))
      (setf docstring (first remaining))
      (setf remaining (rest remaining)))
    (loop :while (and remaining
                      (consp (first remaining))
                      (keywordp (car (first remaining))))
          :do (push (first remaining) options)
              (setf remaining (rest remaining)))
    (values docstring (nreverse options) remaining)))

(defmacro define-maintainer (name lambda-list &body body)
  "Define and register a maintainer named NAME that resolves findings.
LAMBDA-LIST is a single-element specialised list ((FINDING-VAR FINDING-TYPE)).
BODY begins with an optional docstring, followed by keyword options, followed
by body forms that return a RESOLUTION or NIL.

Keyword options:
  (:supersedes LIST-OF-NAMES) — maintainer names this one supersedes.
  (:kind :AUTOMATIC|:AGENT)  — defaults to :AUTOMATIC.

Example:
  (define-maintainer fix-trailing-whitespace ((finding line-finding))
    \"Remove trailing whitespace from source lines.\"
    (:supersedes '(basic-whitespace-fixer))
    (make-text-resolution ...))"
  (check-type name symbol)
  (destructuring-bind ((finding-variable finding-type)) lambda-list
    (multiple-value-bind (docstring options body-forms)
        (parse-maintainer-body body)
      (let ((supersedes (second (find :supersedes options :key #'first)))
            (kind (or (second (find :kind options :key #'first))
                      :automatic)))
        `(progn
           (setf (gethash ',name *maintainers*)
                 (make-maintainer :name ',name
                                  :supersedes ,supersedes
                                  :kind ,kind
                                  :description ,docstring))
           (defmethod prepare-resolution
               ((maintainer-name (eql ',name))
                (,finding-variable ,finding-type))
             ,@(when docstring (list docstring))
             ,@body-forms)
           ',name)))))


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
Walks the maintainer registry, computes the maximal elements of the
superseding partial order, calls PREPARE-RESOLUTION on each maintainer
name, and returns the list of non-NIL resolutions. Finding-type
dispatch is handled by CLOS method specialisation."
  (declare (type finding finding))
  (let* ((all-maintainers
           (loop :for name :being :the :hash-key :of *maintainers*
                 :collect (gethash name *maintainers*)))
         (maximal (maximal-maintainers all-maintainers)))
    (loop :for current-maintainer :in maximal
          :for resolution = (prepare-resolution
                             (maintainer-name current-maintainer)
                             finding)
          :when resolution
          :collect resolution)))

;;;; End of file `maintainer.lisp'
