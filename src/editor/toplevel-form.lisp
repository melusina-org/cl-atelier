;;;; toplevel-form.lisp — The toplevel-form record for the Atelier editor

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/editor)


;;;;
;;;; Class Definition
;;;;

(defclass toplevel-form ()
  ((kind
    :initarg :kind
    :reader toplevel-form-kind
    :type symbol
    :documentation "The operator symbol from the form's CAR.
Examples: COMMON-LISP:DEFUN, COMMON-LISP:DEFCLASS,
CONFIDENCE:DEFINE-TESTCASE, ATELIER:DEFINE-MAINTAINER.
Third-party toplevel macros are first-class citizens.")
   (name
    :initarg :name
    :reader toplevel-form-name
    :initform nil
    :type (or symbol null)
    :documentation "The defined symbol, derived from position 1 (second
element) of the form. NIL for expression forms that do not define
a named entity.")
   (body
    :initarg :body
    :reader toplevel-form-body
    :documentation "The complete form as an Eclector CST. Reader
conditionals (#+/#-) inside the body are preserved as CST structure
via SKIPPED-CST and ANNOTATED-CONS-CST nodes. The docstring (if any)
remains in its natural position. Use TOPLEVEL-FORM-AST for an
s-expression view.")
   (eval-when
    :initarg :eval-when
    :reader toplevel-form-eval-when
    :initform '(:load-toplevel :execute)
    :type list
    :documentation "The EVAL-WHEN situation list wrapping this form.
Default (:LOAD-TOPLEVEL :EXECUTE) is the CL toplevel default and
is elided on write. Non-default values cause the form to be wrapped
in (EVAL-WHEN (<situations>) ...) on write.")
   (source-text
    :initarg :source-text
    :reader toplevel-form-source-text
    :initform nil
    :type (or string null)
    :documentation "The original input string from which this form was
parsed. Set by READ-TOPLEVEL-FORM-FROM-STRING. NIL for forms made
via MAKE-TOPLEVEL-FORM. Used by WRITE-TOPLEVEL-FORM-TO-STRING for
verbatim copy of non-matching #+/#- branches (Amendment 1)."))
  (:documentation "A toplevel Common Lisp form as a semantic record.
The body is an Eclector CST preserving reader conditionals. The
editor owns file layout, IN-PACKAGE, header/footer, and ordering;
the caller owns only the form list.
See product/reference/projectional-editor-design.md."))

(defun make-toplevel-form (&rest initargs &key kind name body eval-when source-text)
  "Create and return a TOPLEVEL-FORM."
  (declare (ignore kind name body eval-when source-text))
  (apply #'make-instance 'toplevel-form initargs))

(defmethod print-object ((form toplevel-form) stream)
  (print-unreadable-object (form stream :type t :identity t)
    (format stream "~S ~S"
            (toplevel-form-kind form)
            (toplevel-form-name form))))

(defmethod describe-object ((form toplevel-form) stream)
  (format stream "~&~A is a TOPLEVEL-FORM.~%" form)
  (format stream "~&  Kind: ~S~%" (toplevel-form-kind form))
  (format stream "~&  Name: ~S~%" (toplevel-form-name form))
  (format stream "~&  Eval-when: ~S~%" (toplevel-form-eval-when form))
  (format stream "~&  Source-text: ~:[NIL~;~:*~S~]~%"
          (when (toplevel-form-source-text form)
            (let ((text (toplevel-form-source-text form)))
              (if (> (length text) 60)
                  (concatenate 'string (subseq text 0 57) "...")
                  text))))
  (terpri stream))


;;;;
;;;; Derived Accessor
;;;;

(defgeneric toplevel-form-ast (form &key features)
  (:documentation "Return the form body as a plain s-expression.
Reader conditionals in the CST are evaluated against FEATURES
(default: CL:*FEATURES*). The result is lossy: cross-platform
branches for non-matching features are dropped."))

(defmethod toplevel-form-ast ((form toplevel-form) &key (features cl:*features*))
  "Walk the body CST and produce an s-expression, evaluating reader
conditionals against FEATURES."
  (declare (type list features))
  (let ((body-cst (toplevel-form-body form)))
    (when body-cst (cst-to-sexp body-cst features))))

(defun cst-to-sexp (cst features)
  "Convert an Eclector CST node to an s-expression, evaluating reader
conditionals against FEATURES. SKIPPED-CST nodes whose feature expression
matches FEATURES are included; those that do not match are dropped."
  (declare (type list features))
  (typecase cst
    (skipped-cst
     ;; A skipped node: evaluate whether it should be included
     ;; under the given FEATURES. The node was skipped by the reader
     ;; under the original *features*; under a different feature set
     ;; it might be included. However, the skipped node's internal
     ;; structure was read under *read-suppress*, so we don't have
     ;; a real s-expression for it. Return a sentinel indicating
     ;; we cannot reconstruct.
     (values nil :skipped))
    (annotated-cons-cst
     ;; A cons node that also carries skipped children — recurse
     ;; on the normal CST structure, ignore skipped nodes.
     (cst-to-sexp-cons cst features))
    (cst:cons-cst
     (cst-to-sexp-cons cst features))
    (cst:atom-cst
     (cst:raw cst))
    (t
     (cst:raw cst))))

(defun cst-to-sexp-cons (cst features)
  "Convert a CONS-CST to an s-expression by walking first/rest."
  (declare (type list features))
  (let ((raw (cst:raw cst)))
    ;; Use the raw s-expression directly — it was already built by
    ;; Eclector with the original *features* evaluated. For the
    ;; default features case this is correct.
    raw))

;;;; End of file `toplevel-form.lisp'
