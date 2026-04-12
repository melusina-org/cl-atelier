;;;; conditions.lisp — Conditions for the Atelier projectional editor

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/editor)


;;;;
;;;; Condition Reporter
;;;;

(defun report-unexpected-toplevel-form (condition stream)
  "Write a human-readable description of CONDITION to STREAM."
  (format stream "~@<Unexpected toplevel form (~A): ~S~@:>"
          (unexpected-toplevel-form-reason condition)
          (unexpected-toplevel-form-source condition)))


;;;;
;;;; Condition
;;;;

(define-condition unexpected-toplevel-form (error)
  ((source
    :initarg :source
    :reader unexpected-toplevel-form-source
    :documentation "The offending form as a string or s-expression.")
   (reason
    :initarg :reason
    :reader unexpected-toplevel-form-reason
    :type keyword
    :documentation "Why the form is unexpected.
One of :PROGN, :SIDE-EFFECT, :IN-PACKAGE, :READER-MACRO."))
  (:report report-unexpected-toplevel-form)
  (:documentation "Signalled by READ-TOPLEVEL-FORM-FROM-STRING when a
toplevel form does not fit the projectional editor's constrained model.
See product/reference/projectional-editor-design.md § Forbidden at toplevel.

For :PROGN, a DECOMPOSE restart is available that splits the PROGN into
individual TOPLEVEL-FORM records. For all other reasons, the caller must
handle the condition or let it propagate."))

;;;; End of file `conditions.lisp'
