;;;; eclector-client.lisp — Custom Eclector client preserving #+/#- as CST nodes

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/editor)


;;;;
;;;; CST Node Subclasses for Skipped Input
;;;;

(defclass skipped-cst (cst:atom-cst)
  ((reason
    :initarg :reason
    :reader skipped-cst-reason
    :documentation "Cons of (context . feature-expression).
CONTEXT is :SHARPSIGN-PLUS or :SHARPSIGN-MINUS.
FEATURE-EXPRESSION is the feature expression symbol or list.")
   (skipped-children
    :initarg :skipped-children
    :reader skipped-cst-children
    :initform nil
    :documentation "Sub-parse-results collected under *READ-SUPPRESS*."))
  (:documentation "A CST node representing input skipped by #+/#-.
The SOURCE slot (inherited from CST:ATOM-CST) holds the source range
as a cons (START . END) of character offsets into the original string.
The original text can be recovered via (SUBSEQ source-text START END)."))

(defclass annotated-cons-cst (cst:cons-cst)
  ((skipped-nodes
    :initarg :skipped-nodes
    :reader annotated-cons-cst-skipped-nodes
    :initform nil
    :documentation "List of SKIPPED-CST nodes from #+/#- inside this form.
These nodes were filtered from the children before CST:RECONSTRUCT
and attached here so the write path can copy their source text verbatim."))
  (:documentation "A CONS-CST annotated with skipped #+/#- nodes from its
children. Created by the :AROUND method on MAKE-EXPRESSION-RESULT."))


;;;;
;;;; Custom Client
;;;;

(defclass preserving-cst-client (eclector.concrete-syntax-tree:cst-client)
  ()
  (:documentation "An Eclector CST client that preserves #+/#- skipped input
as SKIPPED-CST nodes attached to ANNOTATED-CONS-CST parents. Used by
READ-TOPLEVEL-FORM-FROM-STRING to produce CSTs where reader conditionals
are structural, not evaluated away."))

(defmethod eclector.parse-result:make-skipped-input-result
    ((client preserving-cst-client) stream reason children source)
  "Return a SKIPPED-CST node for input skipped by #+/#-.
REASON is (CONS CONTEXT FEATURE-EXPRESSION). SOURCE is (START . END)."
  (make-instance 'skipped-cst
    :raw nil
    :source source
    :reason reason
    :skipped-children children))

(defmethod eclector.parse-result:make-expression-result :around
    ((client preserving-cst-client) result children source)
  "Filter SKIPPED-CST nodes from CHILDREN before CST:RECONSTRUCT,
then annotate the resulting CONS-CST with the skipped nodes."
  (flet ((skipped-p (child)
           (typep child 'skipped-cst)))
    (let ((skipped (remove-if-not #'skipped-p children))
          (real (remove-if #'skipped-p children)))
      (let ((cst (call-next-method client result real source)))
        (when (and skipped (typep cst 'cst:cons-cst))
          (change-class cst 'annotated-cons-cst :skipped-nodes skipped))
        cst))))

;;;; End of file `eclector-client.lisp'
