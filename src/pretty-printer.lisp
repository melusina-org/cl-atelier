;;;; pretty-printer.lisp — Pretty-printer dispatch table for Atelier

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)


;;;;
;;;; Pprint Dispatch Functions
;;;;

;;; Pprint dispatch functions take (STREAM FORM) as required by
;;; SET-PPRINT-DISPATCH.  They bind *STANDARD-OUTPUT* to the stream
;;; and delegate to internal helpers that print to *STANDARD-OUTPUT*
;;; using NIL as the stream argument, following CLtL2 conventions.
;;;
;;; All helpers use explicit PPRINT-LOGICAL-BLOCK, PPRINT-NEWLINE,
;;; PPRINT-INDENT, and WRITE calls — never FORMAT control strings.

(defun complex-form-p (form)
  (and (listp form)
       (some #'listp form)))

(defun lambda-list-keyword-p (item)
  "Return T when ITEM is a lambda-list keyword."
  (member item '(&optional &rest &key &aux &body &allow-other-keys
                 &whole &environment)))

(defun pprint-lambda-list (lambda-list)
  "Print LAMBDA-LIST with awareness of lambda-list keywords.
Required parameters use fill-style breaks (pack on line).  When a
lambda-list keyword (&key, &optional, etc.) is encountered, its
parameters use linear-style breaks aligned with the first one."
  (pprint-logical-block (nil lambda-list :prefix "(" :suffix ")")
    (loop (pprint-exit-if-list-exhausted)
          (let ((item (pprint-pop)))
            (cond
              ((lambda-list-keyword-p item)
               (write item)
               (pprint-exit-if-list-exhausted)
               (write-char #\Space)
               (pprint-indent :current 0)
               (loop (let ((param (pprint-pop)))
                       (cond
                         ((lambda-list-keyword-p param)
                          (write param)
                          (pprint-exit-if-list-exhausted)
                          (write-char #\Space)
                          (pprint-indent :current 0))
                         (t
                          (write param)
                          (pprint-exit-if-list-exhausted)
                          (write-char #\Space)
                          (pprint-newline :linear))))))
              (t
               (write item)
               (pprint-exit-if-list-exhausted)
               (write-char #\Space)
               (pprint-newline :fill)))))))

(defun pprint-slot-spec (slot)
  "Print a DEFCLASS slot specification.  Slot name on the first line,
each slot option on its own line aligned with the first option."
  (if (atom slot)
      (write slot)
      (pprint-logical-block (nil slot :prefix "(" :suffix ")")
        (write (pprint-pop))
        (loop (pprint-exit-if-list-exhausted)
              (pprint-newline :mandatory)
              (write (pprint-pop))
              (pprint-exit-if-list-exhausted)
              (write-char #\Space)
              (write (pprint-pop))))))

(defun pprint-slot-list (slots)
  "Print a DEFCLASS slot list.  Each slot on its own line."
  (pprint-logical-block (nil slots :prefix "(" :suffix ")")
    (loop (pprint-exit-if-list-exhausted)
          (pprint-slot-spec (pprint-pop))
          (pprint-exit-if-list-exhausted)
          (pprint-newline :mandatory))))

(defun pprint-option-form (option)
  "Print a DEFCLASS option form.  The option keyword and first value
on the first line, remaining values aligned.  When the single value
is a string (e.g. :DOCUMENTATION), print it on the same line
unconditionally — the string's own newlines handle line-breaking."
  (if (atom option)
      (write option)
      (pprint-logical-block (nil option :prefix "(" :suffix ")")
        (let ((keyword (pprint-pop)))
          (write keyword)
          (loop (pprint-exit-if-list-exhausted)
                (write-char #\Space)
                (let ((value (pprint-pop)))
                  (unless (stringp value)
                    (pprint-newline :linear))
                  (write value)))))))




;;;;
;;;; Body-style forms
;;;;

(defun pprint-body (form)
  "Print a form with OPERATOR + one distinguished arg + body.
Used for WHEN, UNLESS, EVAL-WHEN, UNWIND-PROTECT, etc.
Body forms always break to new lines at block indent 1."
  (pprint-logical-block (nil form :prefix "(" :suffix ")")
    (write (pprint-pop))
    (write-char #\Space)
    (write (pprint-pop))
    (pprint-indent :block 1)
    (loop (pprint-exit-if-list-exhausted)
          (pprint-newline :mandatory)
          (write (pprint-pop)))))

(defun pprint-progn (form)
  "Print PROGN: body forms with linear breaks.  Short bodies stay
on one line."
  (pprint-logical-block (nil form :prefix "(" :suffix ")")
    (write (pprint-pop))
    (pprint-indent :block 1)
    (loop (pprint-exit-if-list-exhausted)
          (write-char #\Space)
          (pprint-newline :linear)
          (write (pprint-pop)))))


;;;;
;;;; Definition forms
;;;;

(defun pprint-defun (form)
  "Print DEFUN/DEFMACRO following CLtL2 style.  Name aligned at
current position, lambda-list after fill break, body at block indent 1."
  (pprint-logical-block (nil form :prefix "(" :suffix ")")
    (write (pprint-pop))
    (write-char #\Space)
    (pprint-newline :miser)
    (pprint-indent :block 3)
    (write (pprint-pop))
    (write-char #\Space)
    (pprint-newline :fill)
    (pprint-lambda-list (pprint-pop))
    (pprint-indent :block 1)
    (loop (pprint-exit-if-list-exhausted)
          (write-char #\Space)
          (pprint-newline :linear)
          (write (pprint-pop)))))

(defun pprint-defmethod (form)
  "Print DEFMETHOD: name, optional qualifier, specialized lambda-list,
then body forms."
  (pprint-logical-block (nil form :prefix "(" :suffix ")")
    (write (pprint-pop))
    (write-char #\Space)
    (write (pprint-pop))
    (write-char #\Space)
    (let ((next (pprint-pop)))
      (if (listp next)
          (pprint-lambda-list next)
          (progn
            (write next)
            (write-char #\Space)
            (pprint-lambda-list (pprint-pop)))))
    (pprint-indent :block 1)
    (loop (pprint-exit-if-list-exhausted)
          (write-char #\Space)
          (pprint-newline :linear)
          (write (pprint-pop)))))

(defun pprint-defgeneric (form)
  "Print DEFGENERIC: name and lambda-list on the first line,
options on subsequent lines."
  (pprint-logical-block (nil form :prefix "(" :suffix ")")
    (write (pprint-pop))
    (write-char #\Space)
    (pprint-newline :miser)
    (pprint-indent :current 0)
    (write (pprint-pop))
    (write-char #\Space)
    (pprint-newline :fill)
    (pprint-lambda-list (pprint-pop))
    (pprint-indent :block 1)
    (loop (pprint-exit-if-list-exhausted)
          (write-char #\Space)
          (pprint-newline :linear)
          (pprint-option-form (pprint-pop)))))

(defun pprint-lambda (form)
  (pprint-logical-block (nil form :prefix "(" :suffix ")")
    (write (pprint-pop))
    (write-char #\Space)
    (pprint-newline :miser)
    (pprint-indent :current 0)
    (pprint-lambda-list (pprint-pop))
    (pprint-indent :block 1)
    (loop :with complex-p = (some #'complex-form-p (cddr form))
          :do (pprint-exit-if-list-exhausted)
              (write-char #\Space)
              (pprint-newline (if complex-p :mandatory :linear))
              (write (pprint-pop)))))

(defun pprint-defvar (form)
  "Print DEFVAR/DEFPARAMETER/DEFCONSTANT: name and short value on the
first line, long value and docstring on subsequent lines at body indent."
  (pprint-logical-block (nil form :prefix "(" :suffix ")")
    (write (pprint-pop))
    (write-char #\Space)
    (write (pprint-pop))
    (pprint-indent :block 1)
    (pprint-exit-if-list-exhausted)
    (write-char #\Space)
    (pprint-newline :fill)
    (write (pprint-pop))
    (loop (pprint-exit-if-list-exhausted)
          (pprint-newline :mandatory)
          (write (pprint-pop)))))

(defun pprint-defclass (form)
  "Print DEFCLASS: name and superclasses on the first line,
slot list and class options on subsequent lines."
  (pprint-logical-block (nil form :prefix "(" :suffix ")")
    (write (pprint-pop))
    (write-char #\Space)
    (write (pprint-pop))
    (write-char #\Space)
    (write (pprint-pop))
    (pprint-indent :block 1)
    (pprint-exit-if-list-exhausted)
    (pprint-newline :mandatory)
    (pprint-slot-list (pprint-pop))
    (loop (pprint-exit-if-list-exhausted)
          (pprint-newline :mandatory)
          (pprint-option-form (pprint-pop)))))

(defun pprint-defstruct-name-and-options (name-and-options)
  "Print a DEFSTRUCT name-and-options form.  When it is a simple symbol,
print it directly.  When it is a list, print name and each option on
its own line."
  (if (atom name-and-options)
      (write name-and-options)
      (pprint-logical-block (nil name-and-options :prefix "(" :suffix ")")
        (write (pprint-pop))
        (loop (pprint-exit-if-list-exhausted)
              (pprint-newline :mandatory)
              (write (pprint-pop))))))

(defun pprint-defstruct (form)
  "Print DEFSTRUCT: name-and-options on the first line, docstring
and slot descriptions on subsequent lines."
  (pprint-logical-block (nil form :prefix "(" :suffix ")")
    (write (pprint-pop))
    (write-char #\Space)
    (pprint-defstruct-name-and-options (pprint-pop))
    (pprint-indent :block 1)
    (loop (pprint-exit-if-list-exhausted)
          (pprint-newline :mandatory)
          (write (pprint-pop)))))

(defun pprint-defpackage (form)
  "Print DEFPACKAGE: name on the first line, each option on its own line."
  (flet ((pprint-defpackage-option (option)
           (cond ((atom option)
                  (write option))
                 ((eq :import-from (first option))
                  (pprint-logical-block (nil option :prefix "(" :suffix ")")
                    (write (pprint-pop))
                    (pprint-exit-if-list-exhausted)
                    (write-char #\Space)
                    (pprint-indent :current 0)
                    (write (pprint-pop))
                    (loop (pprint-exit-if-list-exhausted)
                          (pprint-newline :mandatory)
                          (write (pprint-pop)))))
                 ((eq :export (first option))
                  (pprint-logical-block (nil option :prefix "(" :suffix ")")
                    (write (pprint-pop))
                    (loop (pprint-exit-if-list-exhausted)
                          (pprint-newline :mandatory)
                          (write (pprint-pop)))))
                 (t
                  (pprint-logical-block (nil option :prefix "(" :suffix ")")
                    (write (pprint-pop))
                    (pprint-exit-if-list-exhausted)
                    (write-char #\Space)
                    (pprint-indent :current 0)
                    (write (pprint-pop))
                    (loop (pprint-exit-if-list-exhausted)
                          (pprint-newline :mandatory)
                          (write (pprint-pop))))))))
    (pprint-logical-block (nil form :prefix "(" :suffix ")")
      (write (pprint-pop))
      (write-char #\Space)
      (write (pprint-pop))
      (pprint-indent :block 1)
      (loop (pprint-exit-if-list-exhausted)
            (pprint-newline :mandatory)
            (pprint-defpackage-option (pprint-pop))))))

(defun pprint-deftype (form)
  "Print DEFTYPE: name and lambda-list on the first line, body after."
  (pprint-logical-block (nil form :prefix "(" :suffix ")")
    (write (pprint-pop))
    (write-char #\Space)
    (pprint-newline :miser)
    (pprint-indent :current 0)
    (write (pprint-pop))
    (write-char #\Space)
    (pprint-newline :fill)
    (pprint-lambda-list (pprint-pop))
    (pprint-indent :block 1)
    (loop (pprint-exit-if-list-exhausted)
          (write-char #\Space)
          (pprint-newline :linear)
          (write (pprint-pop)))))


;;;;
;;;; Binding forms
;;;;

(defun pprint-let (form)
  "Print LET/LET* following CLtL2 style.  Bindings in their own
logical block, body forms with linear breaks at block indent 1."
  (flet ((pprint-bindings (bindings)
           "Print a LET/LET* binding list following CLtL2 style.
Each binding is printed in its own logical block.  Short bindings
pack on the same line (fill breaks), long bindings break between
variable and init-form (linear breaks)."
           (pprint-logical-block (nil bindings :prefix "(" :suffix ")")
             (pprint-exit-if-list-exhausted)
             (loop (pprint-logical-block (nil (pprint-pop) :prefix "(" :suffix ")")
                     (pprint-exit-if-list-exhausted)
                     (pprint-indent :block 1)
                     (loop (write (pprint-pop))
                           (pprint-exit-if-list-exhausted)
                           (write-char #\Space)
                           (pprint-newline :mandatory)))
                   (pprint-exit-if-list-exhausted)
                   (write-char #\Space)
                   (pprint-newline :fill)))))
    (pprint-logical-block (nil form :prefix "(" :suffix ")")
      (write (pprint-pop))
      (write-char #\Space)
      (pprint-bindings (pprint-pop))
      (pprint-indent :block 1)
      (loop (pprint-exit-if-list-exhausted)
            (write-char #\Space)
            (pprint-newline :mandatory)
            (write (pprint-pop))))))


(defun pprint-flet (form)
  "Print FLET/LABELS: function bindings then body."
  (flet ((pprint-flet-binding (binding)
           "Print one FLET/LABELS function binding: name, lambda-list, then body."
           (pprint-logical-block (nil binding :prefix "(" :suffix ")")
             (write (pprint-pop))
             (write-char #\Space)
             (pprint-lambda-list (pprint-pop))
             (pprint-indent :block 1)
             (loop (pprint-exit-if-list-exhausted)
                   (pprint-newline :mandatory)
                   (write (pprint-pop))))))
    (pprint-logical-block (nil form :prefix "(" :suffix ")")
      (write (pprint-pop))
      (write-char #\Space)
      (let ((bindings (pprint-pop)))
        (pprint-logical-block (nil bindings :prefix "(" :suffix ")")
          (loop (pprint-exit-if-list-exhausted)
                (pprint-flet-binding (pprint-pop))
                (pprint-exit-if-list-exhausted)
                (pprint-newline :mandatory))))
      (pprint-indent :block 1)
      (loop (pprint-exit-if-list-exhausted)
            (pprint-newline :mandatory)
            (write (pprint-pop))))))


;;;;
;;;; Conditional and dispatch forms
;;;;


(defun pprint-cond (form)
  "Print COND: first clause on same line as cond, subsequent clauses
aligned with the first."
  (flet ((pprint-cond-clause (clause)
           "Print a COND clause.  Test on the first line, body forms on
subsequent lines aligned with the test.  Always breaks between
test and body."
           (if (atom clause)
               (write clause)
               (pprint-logical-block (nil clause :prefix "(" :suffix ")")
                 (write (pprint-pop))
                 (loop (pprint-exit-if-list-exhausted)
                       (pprint-newline :mandatory)
                       (write (pprint-pop)))))))
    (pprint-logical-block (nil form :prefix "(" :suffix ")")
      (write (pprint-pop))
      (write-char #\Space)
      (pprint-indent :current 0)
      (pprint-cond-clause (pprint-pop))
      (loop (pprint-exit-if-list-exhausted)
            (pprint-newline :mandatory)
            (pprint-cond-clause (pprint-pop))))))

(defun pprint-case (form)
  "Print CASE/ECASE/TYPECASE/ETYPECASE: keyform on same line,
clauses at body indent."
  (flet ((complex-clause-p (clause)
           (complex-form-p (rest clause)))
         (pprint-case-clause (clause &optional complex-p)
           "Print a CASE/TYPECASE clause.  Short single-form bodies stay on
the same line as the key.  Multi-form bodies break to next line."
           (if (atom clause)
               (write clause)
               (pprint-logical-block (nil clause :prefix "(" :suffix ")")
                 (write (pprint-pop))
                 (loop (pprint-exit-if-list-exhausted)
                       (write-char #\Space)
                       (pprint-newline (if complex-p :mandatory :linear))
                       (write (pprint-pop)))))))
    (pprint-logical-block (nil form :prefix "(" :suffix ")")
      (write (pprint-pop))
      (write-char #\Space)
      (write (pprint-pop))
      (pprint-indent :block 1)
      (pprint-newline :mandatory)
      (let ((complex-p
              (some #'complex-clause-p (cddr form))))
        (pprint-case-clause (pprint-pop) complex-p)
        (loop (pprint-exit-if-list-exhausted)
              (pprint-newline :mandatory)
              (pprint-case-clause (pprint-pop) complex-p))))))

(defun pprint-handler-clause (clause)
  "Print a HANDLER-CASE handler clause: (type (var) body...)."
  (if (atom clause)
      (write clause)
      (pprint-logical-block (nil clause :prefix "(" :suffix ")")
        (write (pprint-pop))
        (write-char #\Space)
        (write (pprint-pop))
        (pprint-indent :block 1)
        (loop (pprint-exit-if-list-exhausted)
              (pprint-newline :mandatory)
              (write (pprint-pop))))))

(defun pprint-handler-case (form)
  "Print HANDLER-CASE: expression then handler clauses."
  (pprint-logical-block (nil form :prefix "(" :suffix ")")
    (write (pprint-pop))
    (write-char #\Space)
    (write (pprint-pop))
    (pprint-indent :block 1)
    (loop (pprint-exit-if-list-exhausted)
          (pprint-newline :mandatory)
          (pprint-handler-clause (pprint-pop)))))

(defun pprint-restart-case (form)
  "Print RESTART-CASE: expression then restart clauses."
  (flet ((pprint-restart-clause (clause)
           "Print a RESTART-CASE restart clause: (name arglist options... body...).
Restart options like :REPORT stay on the same line as their value when
short."
           (if (atom clause)
               (write clause)
               (pprint-logical-block (nil clause :prefix "(" :suffix ")")
                 (write (pprint-pop))
                 (write-char #\Space)
                 (write (pprint-pop))
                 (pprint-indent :block 1)
                 (pprint-exit-if-list-exhausted)
                 (write-char #\Space)
                 (pprint-newline :linear)
                 (let ((first-body
                         (pprint-pop)))
                   (write first-body)
                   (when (eq first-body :report)
                     (pprint-exit-if-list-exhausted)
                     (write-char #\Space)
                     (write (pprint-pop))))
                 (loop (pprint-exit-if-list-exhausted)
                       (write-char #\Space)
                       (pprint-newline :linear)
                       (write (pprint-pop)))))))
    (pprint-logical-block (nil form :prefix "(" :suffix ")")
      (write (pprint-pop))
      (write-char #\Space)
      (write (pprint-pop))
      (pprint-indent :block 1)
      (loop (pprint-exit-if-list-exhausted)
            (pprint-newline :mandatory)
            (pprint-restart-clause (pprint-pop))))))


;;;;
;;;; SETF
;;;;

(defun pprint-setf (form)
  "Print SETF with place-value pairs aligned under the first place.
Short single-pair SETF stays on one line.  Multi-pair SETF breaks
between pairs with each pair on its own line."
  (pprint-logical-block (nil form :prefix "(" :suffix ")")
    (write (pprint-pop))
    (pprint-exit-if-list-exhausted)
    (write-char #\Space)
    (pprint-indent :current 0)
    (loop (write (pprint-pop))
          (pprint-exit-if-list-exhausted)
          (write-char #\Space)
          (pprint-newline :fill)
          (write (pprint-pop))
          (pprint-exit-if-list-exhausted)
          (write-char #\Space)
          (pprint-newline :linear))))


;;;;
;;;; LIST with plist detection
;;;;

(defun plist-args-p (args)
  "Return T when ARGS looks like keyword property-list arguments:
an even number of elements with keywords at every even position."
  (and (consp args)
       (keywordp (car args))
       (consp (cdr args))
       (loop :for rest :on args :by #'cddr
             :always (and (keywordp (car rest))
                          (consp (cdr rest))))))

(defun pprint-list (form)
  "Print a LIST call.
When the arguments form a keyword property list, print each key-value pair on its own line
with the first pair on the same line as LIST, aligned.  Otherwise use standard first-arg
alignment."
  (let ((args (cdr form)))
    (pprint-logical-block (nil form :prefix "(" :suffix ")")
      (write (pprint-pop))
      (pprint-exit-if-list-exhausted)
      (write-char #\Space)
      (pprint-indent :current 0)
      (if (plist-args-p args)
          (loop (write (pprint-pop))
                (write-char #\Space)
                (write (pprint-pop))
                (pprint-exit-if-list-exhausted)
                (pprint-newline :mandatory))
          (loop (write (pprint-pop))
                (pprint-exit-if-list-exhausted)
                (write-char #\Space)
                (pprint-newline :fill))))))


;;;;
;;;; Concatenate
;;;;

(defun pprint-concatenate (form)
  "Print CONCATENATE with body-style indentation: type on the first
line, remaining arguments indented as body.  Short argument lists
stay on one line."
  (pprint-logical-block (nil form :prefix "(" :suffix ")")
    (write (pprint-pop))
    (write-char #\Space)
    (write (pprint-pop))
    (pprint-indent :block 1)
    (loop :with previous-short-p = nil
          :do (pprint-exit-if-list-exhausted)
              (write-char #\Space)
              (pprint-newline (if previous-short-p :fill :mandatory))
              (let ((next
                      (pprint-pop)))
                (setf previous-short-p
                      (and (stringp next) (<= (length next) 5)))
                (write next)))))

(defun complex-format-arguments-p (format-arguments)
  (and (> (count-if #'listp format-arguments) 1)
       (some #'listp format-arguments)))

(defun pprint-format (form)
  (pprint-logical-block (nil form :prefix "(" :suffix ")")
    (write (pprint-pop))
    (write-char #\Space)
    (pprint-indent :current 0)
    (write (pprint-pop))
    (pprint-exit-if-list-exhausted)
    (write-char #\Space)
    (write (pprint-pop))
    (loop :with complex-p = (complex-format-arguments-p (cdddr form))
          :do (pprint-exit-if-list-exhausted)
              (write-char #\Space)
              (pprint-newline (if complex-p :mandatory :linear))
              (write (pprint-pop)))))

(defun pprint-function-call (form)
  "Print a generic function call.
The form stays on one line when it fits. When it does not fit, the break
point before the first argument indents at :BLOCK 0 — one column inside
the opening paren, matching the Emacs convention for single-argument
calls wrapped under the operator. Subsequent between-argument breaks
switch to :CURRENT 0 so continuation arguments align under the first
argument column. Trailing keyword/value pairs detected by PLIST-ARGS-P
are emitted one per line at the current alignment."
  (pprint-logical-block (nil form :prefix "(" :suffix ")")
    (write (pprint-pop))
    (pprint-exit-if-list-exhausted)
    (pprint-indent :block 0)
    (write-char #\Space)
    ;; For single-argument calls we allow a fill-break before the arg so the
    ;; engine can split tight contexts such as LOOP clauses. For multi-arg
    ;; calls we keep the first arg adjacent to the operator and let the
    ;; between-argument :FILL breaks handle wrapping.
    (if (null (cddr form))
        (pprint-newline :fill)
        (pprint-newline :miser))
    (pprint-indent :current 0)
    (loop :for rest-args :on (rest form)
          :until (plist-args-p rest-args)
          :do (write (pprint-pop))
              (pprint-exit-if-list-exhausted)
              (write-char #\Space)
              (pprint-newline :fill))
    (pprint-indent :current 0)
    (loop (write (pprint-pop))
          (write-char #\Space)
          (write (pprint-pop))
          (pprint-exit-if-list-exhausted)
          (pprint-newline :mandatory))))

;;;;
;;;; Dispatch Table
;;;;

;;; Each dispatch entry is a lambda that binds *STANDARD-OUTPUT* to the
;;; stream and calls the corresponding pprint-* function.

(defun function-call-operator-p (symbol)
  "Return T when SYMBOL is an fboundp operator safe for function-call dispatch.
Excludes operators whose printed representation is handled by hardcoded
paths in OUTPUT-OBJECT rather than the pprint dispatch table:
  QUOTE        — printed as 'X
  FUNCTION     — printed as #'X
  QUASIQUOTE   — printed as `X  (SBCL-specific SB-INT:QUASIQUOTE)
Intercepting any of these with a dispatch entry causes infinite recursion
because CALL-LOGICAL-BLOCK-PRINTER re-dispatches the same form through
OUTPUT-PRETTY-OBJECT."
  (not (member symbol '(quote function loop if declare
                        #+sbcl sb-int:quasiquote))))

(defmacro pprint-dispatch-entry (function-name)
  "Create a dispatch function that binds *STANDARD-OUTPUT* to the stream."
  `(lambda (stream form)
     (let ((*standard-output* stream))
       (,function-name form))))

(defparameter *atelier-pprint-dispatch*
  (let ((table (copy-pprint-dispatch nil)))
    ;; Body-style: operator + 1 distinguished arg + body
    (set-pprint-dispatch '(cons (member when unless eval-when unwind-protect
                                 handler-bind multiple-value-bind
                                 destructuring-bind with-open-file
                                 with-open-stream with-input-from-string
                                 with-output-to-string with-accessors
                                 with-slots do-symbols do-all-symbols
                                 do-external-symbols dolist dotimes))
                         (pprint-dispatch-entry pprint-body) 1 table)
    ;; Progn-style: operator + body only
    (set-pprint-dispatch '(cons (member progn locally block tagbody)) (pprint-dispatch-entry pprint-progn) 1 table)
    ;; Defun-like
    (set-pprint-dispatch '(cons (member defun defmacro define-compiler-macro)) (pprint-dispatch-entry pprint-defun) 1 table)
    (set-pprint-dispatch '(cons (member defmethod)) (pprint-dispatch-entry pprint-defmethod) 1 table)
    (set-pprint-dispatch '(cons (member defgeneric)) (pprint-dispatch-entry pprint-defgeneric) 1 table)
    (set-pprint-dispatch '(cons (member lambda)) (pprint-dispatch-entry pprint-lambda) 1 table)
    ;; Definitions
    (set-pprint-dispatch '(cons (member defvar defparameter defconstant)) (pprint-dispatch-entry pprint-defvar) 1 table)
    (set-pprint-dispatch '(cons (member defclass)) (pprint-dispatch-entry pprint-defclass) 1 table)
    (set-pprint-dispatch '(cons (member defstruct)) (pprint-dispatch-entry pprint-defstruct) 1 table)
    (set-pprint-dispatch '(cons (member defpackage)) (pprint-dispatch-entry pprint-defpackage) 1 table)
    (set-pprint-dispatch '(cons (member deftype)) (pprint-dispatch-entry pprint-deftype) 1 table)
    ;; Bindings
    (set-pprint-dispatch '(cons (member let let*)) (pprint-dispatch-entry pprint-let) 1 table)
    (set-pprint-dispatch '(cons (member flet labels)) (pprint-dispatch-entry pprint-flet) 1 table)
    ;; Conditionals
    (set-pprint-dispatch '(cons (member cond)) (pprint-dispatch-entry pprint-cond) 1 table)
    (set-pprint-dispatch '(cons (member case ecase ccase typecase etypecase ctypecase)) (pprint-dispatch-entry pprint-case) 1 table)
    ;; Exception handling
    (set-pprint-dispatch '(cons (member handler-case)) (pprint-dispatch-entry pprint-handler-case) 1 table)
    (set-pprint-dispatch '(cons (member restart-case)) (pprint-dispatch-entry pprint-restart-case) 1 table)
    ;; SETF
    (set-pprint-dispatch '(cons (member setf)) (pprint-dispatch-entry pprint-setf) 1 table)
    ;; List with plist detection
    (set-pprint-dispatch '(cons (member list)) (pprint-dispatch-entry pprint-list) 1 table)
    ;; Concatenate
    (set-pprint-dispatch '(cons (member concatenate)) (pprint-dispatch-entry pprint-concatenate) 1 table)
    (set-pprint-dispatch '(cons (member format)) (pprint-dispatch-entry pprint-format) 1 table)
    ;; Generic function call
    (set-pprint-dispatch '(cons (and symbol (satisfies function-call-operator-p))) (pprint-dispatch-entry pprint-function-call) -5 table)
    table)
  "Atelier's pprint dispatch table for code emission.
Copied from the initial (implementation-default) table at load time,
with Atelier-specific overrides for Common Lisp form indentation.
Bind dynamically inside code writers; never set globally.")


;;;;
;;;; Entry Point
;;;;

(defun pretty-print-form (form column &key right-margin)
  "Pretty-print FORM as a string using *ATELIER-PPRINT-DISPATCH*.
COLUMN is the insertion column in the target file. When COLUMN is
greater than zero, each continuation line (line 2 onwards) is prefixed
with COLUMN spaces so the form aligns correctly when placed at that
column. RIGHT-MARGIN controls the effective output width: when
provided, the pretty-printer uses (- RIGHT-MARGIN COLUMN) as
*PRINT-RIGHT-MARGIN*; when NIL, *PRINT-RIGHT-MARGIN* is NIL (unlimited).
Returns the formatted string without a trailing newline."
  (declare (type t form)
           (type (integer 0) column)
           (type (or null integer) right-margin)
           (values string))
  (let* ((effective-margin
           (when right-margin
             (max 1 (- right-margin column))))
         (raw
           (with-output-to-string (stream)
             (let ((*print-pprint-dispatch* *atelier-pprint-dispatch*)
                   (*print-pretty* t)
                   (*print-case* :downcase)
                   (*print-right-margin* effective-margin))
               (write form :stream stream)))))
    (if (zerop column)
        raw
        (flet ((indent-continuation-line (line)
                 (concatenate 'string (make-string column :initial-element #\Space) line)))
          (let ((lines (string-lines raw)))
            (join-lines
             (cons (first lines)
                   (mapcar #'indent-continuation-line (rest lines)))))))))

;;;;
;;;; File Reformatting
;;;;

(defun extract-file-header (content)
  "Return the file header portion of CONTENT.
The header is all text before the first line that begins with an
open parenthesis at column zero.  Returns the empty string when
the file starts with a toplevel form."
  (declare (type string content)
           (values string))
  (let ((lines (string-lines content)))
    (let ((header-lines
            (loop :for line :in lines
                  :while (or (zerop (length line))
                             (char/= #\( (char line 0)))
                  :collect line)))
      (if header-lines
          (concatenate 'string (join-lines header-lines) (string #\Newline))
          ""))))

(defun extract-file-footer (content)
  "Return the file footer portion of CONTENT.
The footer is the last line if it begins with four semicolons,
matching the project convention ';;;; End of file ...'.
Returns the empty string when no footer is present."
  (declare (type string content)
           (values string))
  (let* ((lines (string-lines content))
         (last (car (last lines))))
    (if (and last
             (>= (length last) 4)
             (string= ";;;;" last :end2 4))
        last
        "")))

(defun reformat-file (pathname)
  "Reformat PATHNAME using Atelier's pretty-printer.
Reads all toplevel forms, pretty-prints each via PRETTY-PRINT-FORM,
and writes the result back atomically.  Preserves the file header
\(all lines before the first toplevel form\) and footer.  Emits two
blank lines between toplevel forms.
Does not run the linter — purely read, pretty-print, write."
  (declare (type (or pathname string) pathname)
           (values pathname))
  (let* ((pathname (pathname pathname))
         (content (alexandria:read-file-into-string
                   pathname :external-format :utf-8))
         (header (extract-file-header content))
         (footer (extract-file-footer content))
         (*package* (find-package :common-lisp-user))
         (*read-eval* nil)
         (forms nil))
    ;; Read all toplevel forms from the body (after the header).
    (let ((body-start (length header)))
      (loop :with pos = body-start
            :for (form new-pos) = (multiple-value-list
                                   (read-from-string content nil content
                                                     :start pos))
            :until (eq form content)
            :do (when (and (consp form)
                           (eq (car form) 'cl:in-package))
                  (let ((pkg (find-package (cadr form))))
                    (when pkg (setf *package* pkg))))
                (push form forms)
                (setf pos new-pos)))
    (setf forms (nreverse forms))
    ;; Pretty-print each form.
    (flet ((format-form (form)
             (pretty-print-form form 0 :right-margin 120)))
      (let* ((formatted-forms (mapcar #'format-form forms))
             (body (join-lines formatted-forms
                               (coerce '(#\Newline #\Newline #\Newline)
                                       'string)))
             (result (concatenate 'string
                                  header
                                  body
                                  (string #\Newline)
                                  (when (plusp (length footer))
                                    (concatenate 'string
                                                 (string #\Newline)
                                                 footer
                                                 (string #\Newline))))))
        ;; Atomic write-back (INV-8).
        (let ((tmp (uiop:tmpize-pathname pathname)))
          (with-open-file (stream tmp :direction :output
                                      :if-exists :supersede
                                      :external-format :utf-8)
            (write-string result stream))
          (uiop:rename-file-overwriting-target tmp pathname)))))
  pathname)

(defun reformat-system (system-designator &key (sibling-systems t))
  "Reformat all Common Lisp source files in SYSTEM-DESIGNATOR.
When SIBLING-SYSTEMS is true (the default), includes all systems
defined in the same .asd file.  Calls REFORMAT-FILE on each
CL source file.  Does not reformat the .asd file itself."
  (declare (type (or string symbol) system-designator)
           (values null))
  (let* ((system (asdf:find-system system-designator))
         (files (if sibling-systems
                    (collect-all-source-files system)
                    (collect-system-source-files system))))
    (dolist (pathname files)
      (when (string-equal "lisp" (pathname-type pathname))
        (reformat-file pathname))))
  nil)

;;;; End of file `pretty-printer.lisp'
