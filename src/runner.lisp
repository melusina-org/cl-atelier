;;;; runner.lisp — Inspector runner for Atelier

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)


;;;;
;;;; Inspection Protocol
;;;;

(defgeneric inspect-file (inspector source)
  (:documentation "Run INSPECTOR on SOURCE and return a list of findings or NIL.
SOURCE is a PATHNAME for file-level inspectors, or a VECTOR of line
strings for line-level inspectors. Configuration is available via
*PROJECT-CONFIGURATION* and *LINTER-CONFIGURATION*.")
  (:method ((inspector inspector) source)
    "Default method returns NIL — no findings."
    (declare (ignore source))
    nil))

(defgeneric inspect-line (inspector line line-number)
  (:documentation "Run INSPECTOR on a single LINE at LINE-NUMBER.
Return a list of findings for this line, or NIL. LINE is a string.
*CURRENT-PATHNAME* is bound to the file being inspected.")
  (:method ((inspector inspector) (line string) (line-number integer))
    "Default method returns NIL — no findings."
    (declare (ignore line line-number))
    nil))

(defvar *current-pathname* nil
  "The pathname of the file currently being inspected.")

(defvar *project-configuration* nil
  "The project configuration for the system currently being linted.")

(defvar *linter-configuration* nil
  "The linter configuration for the system currently being linted.")

(defvar *current-cst-root* nil
  "The list of top-level CST forms of the file currently being inspected.
Bound during syntax inspection so INSPECT-SYNTAX methods can reference the
full parsed structure of the file.")

(defvar *current-line-vector* nil
  "The line vector of the file currently being inspected.
Bound during syntax inspection so SOURCE-POSITION-TO-LINE-COLUMN can convert
CST character offsets to line/column numbers without re-reading the file.")


;;;;
;;;; Syntax Inspection Protocol
;;;;

(defgeneric inspect-syntax (inspector form)
  (:documentation "Run INSPECTOR on a single top-level CST FORM.
Return a list of findings, or NIL. FORM is a CONCRETE-SYNTAX-TREE:CST node.
*CURRENT-PATHNAME*, *CURRENT-CST-ROOT*, and *CURRENT-LINE-VECTOR* are bound
by the caller.")
  (:method ((inspector inspector) form)
    "Default method returns NIL — no findings."
    (declare (ignore form))
    nil))


;;;;
;;;; CST Helpers
;;;;

(defgeneric parse-common-lisp (source)
  (:documentation "Parse SOURCE as Common Lisp and return a list of top-level CST forms.
Return NIL if the source cannot be parsed. SOURCE is a STRING or a PATHNAME.
Eclector source positions are character offsets (not byte offsets)."))

(defmethod parse-common-lisp ((content string))
  "Parse CONTENT string with Eclector. Return a list of top-level CST forms, or NIL."
  (declare (values list))
  (handler-case
      (let ((stream (make-string-input-stream content)))
        (loop :for form = (eclector.concrete-syntax-tree:read stream nil :eof)
              :until (eq form :eof)
              :collect form))
    (error () nil)))

(defmethod parse-common-lisp ((pathname pathname))
  "Read PATHNAME as UTF-8 and delegate to the string method.
Uses a string so Eclector's source positions are character offsets, not byte
offsets. This matters for files with multi-byte UTF-8 characters (©, –, ë)."
  (declare (values list))
  (handler-case
      (parse-common-lisp (uiop:read-file-string pathname :external-format :utf-8))
    (error () nil)))

(defun parse-lisp-file (pathname)
  "Parse PATHNAME with Eclector and return a list of top-level CST forms.
Compatibility wrapper for PARSE-COMMON-LISP on a pathname."
  (declare (type pathname pathname)
           (values list))
  (parse-common-lisp pathname))

(defun string-to-line-vector (content)
  "Split CONTENT string into a vector of line strings (without trailing newlines)."
  (declare (type string content)
           (values vector))
  (coerce
   (loop :with start = 0
         :for pos = (position #\Newline content :start start)
         :while pos
         :collect (subseq content start pos)
         :do (setf start (1+ pos))
         :finally (when (< start (length content))
                    (collect (subseq content start))))
   'vector))

(defun source-position-to-line-column (position line-vector)
  "Convert character POSITION to a cons (LINE . COLUMN) using LINE-VECTOR.
LINE is 1-based; COLUMN is 0-based. Assumes Unix line endings (newline per line).
Each entry in LINE-VECTOR is a string without its trailing newline character."
  (declare (type fixnum position)
           (type vector line-vector)
           (values cons))
  (let ((offset 0))
    (loop :for line-index :from 0 :below (length line-vector)
          :for line-string = (aref line-vector line-index)
          ;; +1 for the newline character that follows each line in the file
          :for line-end = (+ offset (length line-string) 1)
          :when (< position line-end)
            :return (cons (1+ line-index) (- position offset))
          :do (setf offset line-end)
          :finally (return (cons (length line-vector) 0)))))

(defun make-syntax-finding-from-form (form finding-class
                                      &key inspector severity observation rationale)
  "Create a FINDING-CLASS instance from CST FORM, deriving file, line, column
from the CST source positions and *CURRENT-LINE-VECTOR*.
*CURRENT-PATHNAME*, *CURRENT-LINE-VECTOR*, and *CURRENT-CST-ROOT* must be bound."
  (declare (type concrete-syntax-tree:cst form)
           (type symbol finding-class inspector)
           (type keyword severity)
           (type string observation rationale))
  (let* ((source (cst:source form))
         (start-pos (car source))
         (end-pos (cdr source))
         (line-vector *current-line-vector*)
         (start-lc (source-position-to-line-column start-pos line-vector))
         (end-lc (source-position-to-line-column end-pos line-vector))
         (source-text (write-to-string (cst:raw form))))
    (make-instance finding-class
      :inspector inspector
      :severity severity
      :observation observation
      :rationale rationale
      :file *current-pathname*
      :line (car start-lc)
      :column (cdr start-lc)
      :end-line (car end-lc)
      :end-column (cdr end-lc)
      :source-text source-text
      :cst-node form
      :cst-root *current-cst-root*)))


;;;;
;;;; INSPECT-FILE methods for SYNTAX-INSPECTOR
;;;;

(defmethod inspect-file ((inspector syntax-inspector) (pathname pathname))
  "Parse PATHNAME with Eclector and delegate to the list-of-forms method.
Binds *CURRENT-PATHNAME* and *CURRENT-LINE-VECTOR* for syntax inspection.
Returns NIL without error when the file cannot be parsed."
  (let ((forms (parse-lisp-file pathname)))
    (when forms
      (let ((*current-pathname* pathname)
            (*current-line-vector* (read-file-into-line-vector pathname)))
        (inspect-file inspector forms)))))

(defmethod inspect-file ((inspector syntax-inspector) (forms list))
  "Walk top-level CST FORMS and call INSPECT-SYNTAX on each.
Binds *CURRENT-CST-ROOT* to FORMS. *CURRENT-PATHNAME* and
*CURRENT-LINE-VECTOR* must already be bound by the caller."
  (let ((*current-cst-root* forms))
    (loop :for form :in forms
          :for form-findings = (inspect-syntax inspector form)
          :when form-findings
          :nconc form-findings)))


(defmethod inspect-file ((inspector line-inspector) (pathname pathname))
  "Read PATHNAME into lines and delegate to the VECTOR method.
Bind *CURRENT-PATHNAME* so inspectors can reference the file."
  (let ((*current-pathname* pathname))
    (inspect-file inspector (read-file-into-line-vector pathname))))

(defmethod inspect-file ((inspector line-inspector) (lines vector))
  "Iterate over LINES, calling INSPECT-LINE on each, and collect findings."
  (loop :for line :across lines
        :for line-number :from 1
        :for line-findings = (inspect-line inspector line line-number)
        :when line-findings
        :nconc line-findings))


;;;;
;;;; Helpers
;;;;

(defun inspector-disabled-p (inspector-name)
  "Return T if INSPECTOR-NAME is disabled in *LINTER-CONFIGURATION*."
  (declare (type symbol inspector-name))
  (when *linter-configuration*
    (member inspector-name
            (linter-configuration-disabled-inspectors *linter-configuration*)
            :test #'eq)))

(defun apply-severity-override (finding inspector-name)
  "Return FINDING with its severity overridden if *LINTER-CONFIGURATION* specifies one.
Return FINDING unchanged if no override applies."
  (declare (type finding finding)
           (type symbol inspector-name))
  (when *linter-configuration*
    (let ((override (assoc inspector-name
                          (linter-configuration-severity-overrides *linter-configuration*)
                          :test #'eq)))
      (when override
        (setf (slot-value finding 'severity) (cdr override)))))
  finding)

(defun collect-inspectors-by-level (level-class)
  "Return all registered inspectors of LEVEL-CLASS that are not disabled."
  (declare (type symbol level-class))
  (loop :for inspector-name :being :the :hash-key :of *inspectors*
        :for inspector-instance = (gethash inspector-name *inspectors*)
        :when (and (typep inspector-instance level-class)
                   (not (inspector-disabled-p inspector-name)))
        :collect inspector-instance))

(defun collect-and-override-findings (inspector-instance findings)
  "Apply severity overrides to FINDINGS from INSPECTOR-INSTANCE and return them."
  (declare (type inspector inspector-instance)
           (type list findings))
  (let ((inspector-name (inspector-name inspector-instance)))
    (flet ((apply-override (finding)
             (apply-severity-override finding inspector-name)))
      (mapcar #'apply-override findings))))

(defun read-file-into-line-vector (pathname)
  "Read PATHNAME into a vector of strings, one per line."
  (declare (type pathname pathname)
           (values vector))
  (with-open-file (stream pathname :direction :input :external-format :utf-8)
    (coerce
     (loop :for line = (read-line stream nil nil)
           :while line
           :collect line)
     'vector)))


;;;;
;;;; Inspection Pipeline
;;;;

(defun perform-file-inspection (pathname)
  "Run all registered file-level inspectors on PATHNAME.
Return a list of findings from file-inspector instances, applying any
severity overrides from *LINTER-CONFIGURATION*."
  (declare (type pathname pathname)
           (values list))
  (let ((file-inspectors (collect-inspectors-by-level 'file-inspector)))
    (loop :for inspector-instance :in file-inspectors
          :for inspector-findings = (inspect-file inspector-instance pathname)
          :when inspector-findings
          :nconc (collect-and-override-findings inspector-instance inspector-findings))))

(defun perform-line-inspection (pathname)
  "Run all registered line-level inspectors on PATHNAME.
Reads the file into a line vector once and runs each line-inspector via
INSPECT-FILE. Return a list of findings, applying any severity overrides
from *LINTER-CONFIGURATION*."
  (declare (type pathname pathname)
           (values list))
  (let ((line-inspectors (collect-inspectors-by-level 'line-inspector)))
    (when line-inspectors
      (let ((lines (read-file-into-line-vector pathname))
            (*current-pathname* pathname))
        (loop :for inspector-instance :in line-inspectors
              :for inspector-findings = (inspect-file inspector-instance lines)
              :when inspector-findings
              :nconc (collect-and-override-findings inspector-instance inspector-findings))))))

(defun perform-syntax-inspection (pathname)
  "Run all registered syntax-level inspectors on PATHNAME.
Parses the file with Eclector once and reads the line vector once; both
are shared across all syntax inspectors. Return a list of findings, or
NIL when the file cannot be parsed by Eclector."
  (declare (type pathname pathname)
           (values list))
  (let ((syntax-inspectors (collect-inspectors-by-level 'syntax-inspector)))
    (when syntax-inspectors
      (let ((forms (parse-lisp-file pathname)))
        (when forms
          (let ((*current-pathname* pathname)
                (*current-line-vector* (read-file-into-line-vector pathname))
                (*current-cst-root* forms))
            (loop :for inspector-instance :in syntax-inspectors
                  :for inspector-findings = (inspect-file inspector-instance forms)
                  :when inspector-findings
                  :nconc (collect-and-override-findings
                          inspector-instance inspector-findings))))))))

(defun perform-inspection (pathname)
  "Run the full inspection pipeline on PATHNAME and return a combined list of findings.
Executes three stages in order: file inspection, line inspection, syntax inspection.
Reads *PROJECT-CONFIGURATION* and *LINTER-CONFIGURATION* as dynamic variables;
bind them before calling if you need non-default configuration."
  (declare (type pathname pathname)
           (values list))
  (nconc
   (perform-file-inspection pathname)
   (perform-line-inspection pathname)
   (perform-syntax-inspection pathname)))

;;;; End of file `runner.lisp'
