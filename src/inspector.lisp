;;;; inspector.lisp — Inspector registry for Atelier

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
;;;; Inspector
;;;;

(defclass inspector ()
  ((name
    :initarg :name
    :reader inspector-name
    :type symbol
    :documentation "The symbol identifying this inspector.")
   (level
    :initarg :level
    :reader inspector-level
    :type (member :file :line :region :syntax)
    :documentation "The analysis level of this inspector.")
   (description
    :initarg :description
    :reader inspector-description
    :type (or null string)
    :initform nil
    :documentation "A human-readable description of the inspector."))
  (:documentation "Base class for all inspectors.
Each concrete inspector is a subclass with a singleton instance in the
registry. DEFINE-INSPECTOR generates the subclass and registers it."))

(defmethod print-object ((instance inspector) stream)
  "Print an unreadable representation of INSTANCE to STREAM."
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~S ~S"
            (inspector-level instance)
            (inspector-name instance))))

(defmethod describe-object ((instance inspector) stream)
  "Describe INSTANCE to STREAM."
  (format stream "~&~A is an INSPECTOR.~%" instance)
  (format stream "~&  Name:        ~S~%" (inspector-name instance))
  (format stream "~&  Level:       ~S~%" (inspector-level instance))
  (format stream "~&  Description: ~A~%" (inspector-description instance))
  (terpri stream))


;;;;
;;;; Level Subclasses
;;;;

(defclass file-inspector (inspector)
  ()
  (:default-initargs :level :file)
  (:documentation "An inspector that operates at the file level."))

(defclass line-inspector (inspector)
  ()
  (:default-initargs :level :line)
  (:documentation "An inspector that operates at the line level."))

(defclass region-inspector (inspector)
  ()
  (:default-initargs :level :region)
  (:documentation "An inspector that operates at the region level."))

(defclass syntax-inspector (inspector)
  ()
  (:default-initargs :level :syntax)
  (:documentation "An inspector that operates at the syntax (CST) level."))


;;;;
;;;; Registry
;;;;

(defvar *inspectors* (make-hash-table :test 'eq)
  "Hash table mapping inspector names to inspector singleton instances.")

(defun find-inspector (designator)
  "Find the inspector designated by DESIGNATOR.
DESIGNATOR is either an INSPECTOR instance (returned as-is) or a symbol
used to look up the registry. Return NIL when no inspector is found."
  (declare (type (or inspector symbol) designator))
  (etypecase designator
    (inspector designator)
    (symbol (gethash designator *inspectors*))))

(defun symbol-inspector (symbol)
  "Return the inspector registered under SYMBOL.
Signal an error if no inspector is registered under SYMBOL."
  (declare (type symbol symbol))
  (multiple-value-bind (instance present-p) (gethash symbol *inspectors*)
    (if present-p instance (error "UNBOUND-INSPECTOR ~S" symbol))))

(defun (setf symbol-inspector) (new-value symbol)
  "Register NEW-VALUE as the inspector for SYMBOL."
  (setf (gethash symbol *inspectors*) new-value))

(defun list-inspectors ()
  "Return a list of all registered inspector symbols."
  (loop :for name :being :the :hash-key :of *inspectors*
        :collect name))


;;;;
;;;; DEFINE-INSPECTOR Macro
;;;;

(defun parse-define-body (body)
  "Parse the body of DEFINE-INSPECTOR or DEFINE-MAINTAINER.
Return three values: the docstring (or NIL), an alist of keyword
options, and the remaining body forms."
  (declare (type list body)
           (values (or null string) list list))
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

(defmacro define-inspector (name direct-superclasses lambda-list &body body)
  "Define and register an inspector named NAME.
DIRECT-SUPERCLASSES is a list of superclasses (must include a level
class such as FILE-INSPECTOR). LAMBDA-LIST is the method parameter
list for INSPECT-FILE — typically just ((PATHNAME PATHNAME)). BODY
begins with an optional docstring, followed by keyword options, then
body forms for the INSPECT-FILE method.

The generated method declares INSPECTOR as IGNORABLE. Configuration
is available via *CURRENT-PROJECT-CONFIGURATION* and
*CURRENT-LINTER-CONFIGURATION*.

Example:
  (define-file-inspector check-file-encoding ((pathname pathname))
    \"Check that source files are valid UTF-8.\"
    ...)"
  (check-type name symbol)
  (multiple-value-bind (docstring options body-forms)
      (parse-define-body body)
    (declare (ignore options))
    `(progn
       (defclass ,name (,@direct-superclasses)
         ()
         ,@(when docstring
             `((:documentation ,docstring))))
       (setf (gethash ',name *inspectors*)
             (make-instance ',name
               :name ',name
               :description ,docstring))
       ,@(when body-forms
           `((defmethod inspect-file ((inspector ,name) ,@lambda-list)
               ,@(when docstring (list docstring))
               (declare (ignorable inspector))
               ,@body-forms)))
       ',name)))

(defmacro define-file-inspector (name lambda-list &body body)
  "Define a file-level inspector. Convenience wrapper for DEFINE-INSPECTOR."
  `(define-inspector ,name (file-inspector) ,lambda-list ,@body))

(defmacro define-line-inspector (name lambda-list &body body)
  "Define a line-level inspector.
LAMBDA-LIST should be ((LINE STRING) (LINE-NUMBER INTEGER)).
The generated method is on INSPECT-LINE. The base INSPECT-FILE
method on LINE-INSPECTOR iterates lines and calls INSPECT-LINE
on each. Return a list of findings for the line, or NIL."
  (check-type name symbol)
  (multiple-value-bind (docstring options body-forms)
      (parse-define-body body)
    (declare (ignore options))
    `(progn
       (defclass ,name (line-inspector)
         ()
         ,@(when docstring
             `((:documentation ,docstring))))
       (setf (gethash ',name *inspectors*)
             (make-instance ',name
               :name ',name
               :description ,docstring))
       ,@(when body-forms
           `((defmethod inspect-line ((inspector ,name) ,@lambda-list)
               ,@(when docstring (list docstring))
               (declare (ignorable inspector))
               ,@body-forms)))
       ',name)))

(defmacro define-region-inspector (name lambda-list &body body)
  "Define a region-level inspector. Convenience wrapper for DEFINE-INSPECTOR."
  `(define-inspector ,name (region-inspector) ,lambda-list ,@body))

(defmacro define-syntax-inspector (name lambda-list &body body)
  "Define a syntax-level inspector. Convenience wrapper for DEFINE-INSPECTOR."
  `(define-inspector ,name (syntax-inspector) ,lambda-list ,@body))

;;;; End of file `inspector.lisp'
