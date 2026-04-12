;;;; read-form.lisp — Read a toplevel form from a string

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/editor)


;;;;
;;;; Parsing with the Preserving Client
;;;;

(defvar *preserving-client*
  (make-instance 'preserving-cst-client)
  "Singleton preserving CST client for READ-TOPLEVEL-FORM-FROM-STRING.")

(defun parse-with-preserving-client (string)
  "Parse STRING with the preserving CST client.
Return the first toplevel CST form, or NIL if the string is empty.
Reader conditionals are preserved as SKIPPED-CST / ANNOTATED-CONS-CST nodes."
  (declare (type string string)
           (values (or cst:cst null)))
  (let ((eclector.reader:*client* *preserving-client*))
    (eclector.concrete-syntax-tree:read-from-string string nil nil)))


;;;;
;;;; Operator Classification
;;;;

(defun expected-toplevel-operator-p (operator)
  "Return T if OPERATOR is an expected toplevel form operator.
Expected operators are: any symbol whose name starts with DEF or DEFINE-,
plus DECLAIM, PROCLAIM, EVAL-WHEN, EXPORT, and standard definition macros."
  (declare (type symbol operator))
  (let ((name (symbol-name operator)))
    (or (alexandria:starts-with-subseq "DEF" name)
        (alexandria:starts-with-subseq "DEFINE-" name)
        (member operator '(cl:declaim cl:proclaim cl:eval-when
                           cl:export cl:in-package cl:progn)
                :test #'eq))))

(defun classify-unexpected-reason (operator)
  "Return the UNEXPECTED-TOPLEVEL-FORM reason keyword for OPERATOR."
  (declare (type symbol operator))
  (cond
    ((eq operator 'cl:progn) :progn)
    ((eq operator 'cl:in-package) :in-package)
    (t :side-effect)))


;;;;
;;;; Name Derivation
;;;;

(defun derive-form-name (raw-sexp)
  "Derive the defined symbol name from the raw s-expression of a toplevel form.
Uses position 1 (second element) as a heuristic. Returns NIL if the
form is atomic, has fewer than two elements, or the second element is not
a symbol."
  (when (and (consp raw-sexp)
             (consp (cdr raw-sexp)))
    (let ((second (cadr raw-sexp)))
      (when (symbolp second)
        second))))


;;;;
;;;; Eval-When Peeling
;;;;

(defun peel-eval-when (cst)
  "If CST represents an (EVAL-WHEN (...) <body>) form, return
\(VALUES inner-cst situation-list). Otherwise return (VALUES CST default-situations)."
  (let ((raw (cst:raw cst)))
    (if (and (consp raw)
             (eq (car raw) 'cl:eval-when)
             (consp (cdr raw))
             (listp (cadr raw))
             (consp (cddr raw)))
        ;; Peel the eval-when: the inner form is the third element
        (let* ((situations (cadr raw))
               ;; Navigate the CST to the third child (the body form)
               ;; CST structure: (eval-when <situations> <body>)
               ;; As cons-cst: first=eval-when, rest=(situations . (body . nil))
               (rest1 (cst:rest cst))      ; (situations . (body . nil))
               (rest2 (cst:rest rest1))     ; (body . nil)
               (body-cst (cst:first rest2)))
          (values body-cst situations))
        (values cst '(:load-toplevel :execute)))))


;;;;
;;;; Entry Point
;;;;

(defun read-toplevel-form-from-string (string)
  "Parse STRING as a single toplevel Common Lisp form and return a
TOPLEVEL-FORM record.

EVAL-WHEN wrappers are peeled into the EVAL-WHEN slot. Reader conditionals
inside the body are preserved as Eclector CST nodes. NAME is derived from
position 1 (second element) of the form.

Signals UNEXPECTED-TOPLEVEL-FORM for:
  :PROGN — with a DECOMPOSE restart that returns a list of TOPLEVEL-FORM records
  :IN-PACKAGE — no restart
  :SIDE-EFFECT — no restart
  :READER-MACRO — no restart

Returns a single TOPLEVEL-FORM, or a list of TOPLEVEL-FORM records when the
DECOMPOSE restart is invoked for a PROGN."
  (declare (type string string)
           (values (or toplevel-form list)))
  (let ((cst (parse-with-preserving-client string)))
    (unless cst
      (error "Cannot parse ~S as a Common Lisp form." string))
    ;; Peel eval-when if present
    (multiple-value-bind (body-cst eval-when-situations)
        (peel-eval-when cst)
      (let* ((raw (cst:raw body-cst))
             (operator (when (consp raw) (car raw))))
        ;; Check for unexpected toplevel forms
        (when (and operator (not (expected-toplevel-operator-p operator)))
          (error 'unexpected-toplevel-form
                 :source raw
                 :reason (classify-unexpected-reason operator)))
        ;; Handle PROGN specially
        (when (and operator (eq operator 'cl:progn))
          (restart-case
              (error 'unexpected-toplevel-form
                     :source raw
                     :reason :progn)
            (decompose ()
              :report "Decompose the PROGN into individual toplevel forms."
              (return-from read-toplevel-form-from-string
                (loop :for child-raw :in (cdr raw)
                      :for child-string = (let ((*print-pretty* t)
                                                (*print-right-margin* 100))
                                            (write-to-string child-raw))
                      :collect (read-toplevel-form-from-string child-string))))))
        ;; Handle IN-PACKAGE
        (when (and operator (eq operator 'cl:in-package))
          (error 'unexpected-toplevel-form
                 :source raw
                 :reason :in-package))
        ;; Build the toplevel-form
        (make-toplevel-form
         :kind (or operator (if (consp raw) (car raw) t))
         :name (derive-form-name raw)
         :body body-cst
         :eval-when eval-when-situations
         :source-text string)))))

;;;; End of file `read-form.lisp'
