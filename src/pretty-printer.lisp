;;;; pretty-printer.lisp — Pretty-printer dispatch table for Atelier

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)


;;;;
;;;; Dispatch Table
;;;;

(defvar *atelier-pprint-dispatch*
  (let ((table (copy-pprint-dispatch nil)))
    ;; WHEN and UNLESS: test on the same line, body indented 2 spaces
    ;; on subsequent lines. Always break after the test.
    (flet ((pprint-when-unless (stream form)
             (pprint-logical-block (stream form :prefix "(" :suffix ")")
               ;; Operator: WHEN or UNLESS
               (write (pprint-pop) :stream stream)
               (write-char #\Space stream)
               ;; Test form on the same line as the operator
               (write (pprint-pop) :stream stream)
               ;; Body forms indented 2 from the opening paren
               (pprint-indent :block 1 stream)
               (loop (pprint-exit-if-list-exhausted)
                     (pprint-newline :mandatory stream)
                     (write (pprint-pop) :stream stream)))))
      (set-pprint-dispatch '(cons (member when)) #'pprint-when-unless 0 table)
      (set-pprint-dispatch '(cons (member unless)) #'pprint-when-unless 0 table))
    table)
  "Atelier's pprint dispatch table for code emission.
Copied from the initial (implementation-default) table at load time,
with Atelier-specific overrides for WHEN/UNLESS indentation.
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
                 ;; Prepend COLUMN spaces to each continuation line.
                 (concatenate 'string (make-string column :initial-element #\Space) line)))
          (let ((lines (string-lines raw)))
            (join-lines
             (cons (first lines)
                   (mapcar #'indent-continuation-line (rest lines)))))))))

;;;; End of file `pretty-printer.lisp'
