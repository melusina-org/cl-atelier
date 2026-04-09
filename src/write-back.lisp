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

;;; *CURRENT-LINE-VECTOR* is defined in runner.lisp (the first file in the load
;;; order that uses it). Both runner and write-back share the same dynamic
;;; variable to convert line/column to character offsets without re-reading
;;; the source file.

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
         (line-vector (or *current-line-vector*
                         (read-file-into-line-vector (finding-file finding))))
         (start (line-column-to-offset (finding-line finding)
                                       (finding-column finding)
                                       line-vector))
         (end (line-column-to-offset (finding-end-line finding)
                                     (finding-end-column finding)
                                     line-vector)))
    (values start end (resolution-replacement resolution))))

(defmethod resolution-text-span ((resolution syntax-resolution))
  "Derive text span from the resolution's CST-NODE source positions.
The transform is applied to the node's raw form; result is pretty-printed
at the node's column. Returns (values start end replacement-string)."
  (declare (values (integer 0) (integer 0) string))
  (let* ((finding (resolution-finding resolution))
         (cst-node (resolution-cst-node resolution))
         (source (concrete-syntax-tree:source cst-node))
         (start (car source))
         (end (cdr source))
         (line-vector (or *current-line-vector*
                         (read-file-into-line-vector (finding-file finding))))
         (start-lc (source-position-to-line-column start line-vector))
         (column (cdr start-lc))
         (new-form (funcall (resolution-transform resolution)
                            (concrete-syntax-tree:raw cst-node)))
         (replacement (pretty-print-form new-form column)))
    (values start end replacement)))


;;;;
;;;; Apply Resolutions
;;;;

(defun compute-resolution-spans (resolutions)
  "Convert RESOLUTIONS to a sorted, non-overlapping list of text spans.
Each span is (start end replacement). Spans are sorted by start-offset
descending (end-to-start ordering). Overlapping spans are silently dropped."
  (declare (type list resolutions)
           (values list))
  (flet ((collect-span (resolution)
           (multiple-value-list (resolution-text-span resolution)))
         (spans-overlap-p (a b)
           (> (second b) (first a))))
    (flet ((remove-overlapping-spans (sorted)
             (let ((accepted nil))
               (dolist (span sorted)
                 (when (or (null accepted)
                           (not (spans-overlap-p (first accepted) span)))
                   (push span accepted)))
               (nreverse accepted))))
      (let* ((spans (mapcar #'collect-span resolutions))
             (sorted (sort spans #'> :key #'first)))
        (remove-overlapping-spans sorted)))))

(defun apply-spans-to-string (content spans)
  "Apply pre-computed SPANS to CONTENT string and return the result.
SPANS must be sorted by start-offset descending (end-to-start ordering)."
  (declare (type string content)
           (type list spans)
           (values string))
  (loop :with text = content
        :for span :in spans
        :do (setf text (concatenate 'string
                                    (subseq text 0 (first span))
                                    (third span)
                                    (subseq text (second span))))
        :finally (return text)))

(defgeneric apply-resolutions (source resolutions)
  (:documentation "Apply RESOLUTIONS to SOURCE and return the resulting content string.
SOURCE is a STRING (pure in-memory transform) or a PATHNAME (reads file,
writes back atomically, returns content). RESOLUTIONS are sorted end-to-start
and overlapping spans are silently skipped."))

(defmethod apply-resolutions ((content string) (resolutions list))
  "Apply RESOLUTIONS to CONTENT string and return the new content.
Binds *CURRENT-LINE-VECTOR* from CONTENT so that resolution-text-span
can convert line/column positions without reading from the filesystem."
  (declare (values string))
  (let ((*current-line-vector*
          (coerce
           (let ((lines nil) (start 0))
             (loop :for pos = (position #\Newline content :start start)
                   :while pos
                   :do (push (subseq content start pos) lines)
                       (setf start (1+ pos)))
             (when (< start (length content))
               (push (subseq content start) lines))
             (nreverse lines))
           'vector)))
    (apply-spans-to-string content (compute-resolution-spans resolutions))))

(defmethod apply-resolutions ((pathname pathname) (resolutions list))
  "Apply RESOLUTIONS to the file at PATHNAME. Write back atomically. Return new content."
  (declare (values string))
  (let* ((content (uiop:read-file-string pathname :external-format :utf-8))
         (result (apply-resolutions content resolutions))
         (tmp (uiop:tmpize-pathname pathname)))
    (with-open-file (stream tmp :direction :output :if-exists :supersede
                               :external-format :utf-8)
      (write-string result stream))
    (uiop:rename-file-overwriting-target tmp pathname)
    result))

(defun apply-resolutions-to-file (pathname resolutions)
  "Apply RESOLUTIONS to the source file at PATHNAME.
Compatibility wrapper for APPLY-RESOLUTIONS on a pathname.
Reads the file, applies resolutions, writes back atomically. Returns PATHNAME."
  (declare (type pathname pathname)
           (type list resolutions)
           (values pathname))
  (apply-resolutions pathname resolutions)
  pathname)

;;;; End of file `write-back.lisp'
