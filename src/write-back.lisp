;;;; write-back.lisp — Write-back engine for Atelier

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)


;;;;
;;;; Write-back Engine
;;;;

(defun line-column-to-offset (line column line-vector)
  "Convert a 1-based LINE number and 0-based COLUMN to a character offset.
LINE-VECTOR is a vector of line strings without trailing newlines.
Each line in the file is followed by a newline character (+1).
Returns the character offset into the file string."
  (declare (type (integer 1) line)
           (type (integer 0) column)
           (type vector line-vector)
           (values (integer 0)))
  ;; Sum the lengths of all preceding lines, +1 newline each, then add column.
  (let ((offset 0))
    (loop :for i :from 0 :below (1- line)
          :do (incf offset (1+ (length (aref line-vector i)))))
    (+ offset column)))

(defgeneric resolution-text-span (resolution)
  (:documentation "Convert RESOLUTION to a text span (values start end replacement).
START and END are character offsets into the file string.
REPLACEMENT is the string to substitute for the characters in [START, END).
Dispatches on resolution subtype:
  TEXT-RESOLUTION: offsets derived from finding line/column via line-vector.
  SYNTAX-RESOLUTION: offsets from (cst:source (finding-cst-node finding));
    transform applied to (cst:raw (finding-cst-node finding));
    result pretty-printed at finding-column."))

(defmethod resolution-text-span ((resolution text-resolution))
  "Derive text span from the finding's line/column positions.
Reads the source file to build the line vector, then converts the
finding's (line, column) and (end-line, end-column) pairs to character
offsets. Returns (values start end replacement-string)."
  (declare (values (integer 0) (integer 0) string))
  (let* ((finding (resolution-finding resolution))
         (line-vector (read-file-into-line-vector (finding-file finding)))
         (start (line-column-to-offset (finding-line finding)
                                       (finding-column finding)
                                       line-vector))
         (end (line-column-to-offset (finding-end-line finding)
                                     (finding-end-column finding)
                                     line-vector)))
    (values start end (resolution-replacement resolution))))

(defmethod resolution-text-span ((resolution syntax-resolution))
  "Derive text span from CST source positions; apply transform; pretty-print.
The CST source cons (start . end) gives character offsets directly.
The transform is applied to the raw form; result is pretty-printed at
the finding's column. Returns (values start end replacement-string)."
  (declare (values (integer 0) (integer 0) string))
  (let* ((finding (resolution-finding resolution))
         (cst-node (finding-cst-node finding))
         (source (concrete-syntax-tree:source cst-node))
         (start (car source))
         (end (cdr source))
         (new-form (funcall (resolution-transform resolution)
                            (concrete-syntax-tree:raw cst-node)))
         (replacement (pretty-print-form new-form (finding-column finding))))
    (values start end replacement)))

(defun apply-resolutions-to-file (pathname resolutions)
  "Apply RESOLUTIONS to the source file at PATHNAME.
Reads the file into a string, converts each resolution to a text span
via RESOLUTION-TEXT-SPAN, sorts spans by start-offset descending
(end-to-start ordering per I17). When two consecutive spans overlap,
the later one (lower start offset) is silently skipped — the caller is
expected to re-inspect and retry rather than receive an error. Writes
the non-overlapping replacements to a temporary file then renames over
PATHNAME atomically. Returns PATHNAME."
  (declare (type pathname pathname)
           (type list resolutions)
           (values pathname))
  (labels ((collect-span (resolution)
              ;; Return a list (start end replacement) for one resolution.
              (multiple-value-list (resolution-text-span resolution)))
            (span-start (span) (first span))
            (span-end   (span) (second span))
            (span-replacement (span) (third span))
            (spans-overlap-p (a b)
              ;; Spans sorted descending by start: A has the higher start.
              ;; [B.start, B.end) and [A.start, A.end) overlap iff B.end > A.start.
              (> (second b) (first a)))
            (remove-overlapping-spans (sorted)
              ;; Walk the sorted list; skip any span that overlaps the previous
              ;; accepted span.  Returns the compatible subset, preserving order.
              (let ((accepted nil))
                (dolist (span sorted)
                  (when (or (null accepted)
                            (not (spans-overlap-p (first accepted) span)))
                    (push span accepted)))
                (nreverse accepted))))
    (let* ((spans   (mapcar #'collect-span resolutions))
           (sorted  (sort spans #'> :key #'span-start))
           (compatible (remove-overlapping-spans sorted)))
      ;; Read the file content, apply non-overlapping replacements end-to-start.
      (let* ((content (uiop:read-file-string pathname :external-format :utf-8))
             (result
               (loop :with text = content
                     :for span :in compatible
                     :do (setf text
                               (concatenate 'string
                                            (subseq text 0 (span-start span))
                                            (span-replacement span)
                                            (subseq text (span-end span))))
                     :finally (return text)))
             (tmp (uiop:tmpize-pathname pathname)))
        (with-open-file (stream tmp
                                :direction :output
                                :if-exists :supersede
                                :external-format :utf-8)
          (write-string result stream))
        (uiop:rename-file-overwriting-target tmp pathname)
        pathname))))

;;;; End of file 'write-back.lisp'
