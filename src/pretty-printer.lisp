;;;; pretty-printer.lisp — Pretty-printer dispatch table for Atelier

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
;;;; Dispatch Table
;;;;

(defvar *atelier-pprint-dispatch*
  (copy-pprint-dispatch nil)
  "Atelier's pprint dispatch table for code emission.
Copied from the initial (implementation-default) table at load time.
Never modified after load time. Bind dynamically inside code writers;
never set globally. The initial table already handles DEFUN, LET, FLET,
LABELS, LOOP, and standard special forms with correct indentation.")


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

;;;; End of file 'pretty-printer.lisp'
