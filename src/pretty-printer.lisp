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
             (pretty-print-form form 0)))
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
