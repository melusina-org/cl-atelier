;;;; transcript-render.lisp — Sexp-to-JSON and sexp-to-Markdown walkers

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

;;; Pure functions on plist sequences. The transcript file is a
;;; sequence of plists, one per line, in the order events occurred.
;;; These walkers turn that sequence into the JSON and Markdown
;;; views exposed as resources. They never touch the filesystem.

(defun sexp-to-json-entries (entries)
  "Convert ENTRIES (a list of plists) to a JSON array string. Each
   plist becomes a JSON object whose keys are the lowercased keyword
   names. Nested plists, lists, and bare values are converted via
   %TRANSCRIPT-VALUE-TO-JSON-VALUE."
  (let ((array (map 'vector #'%transcript-plist-to-object entries)))
    (encode-to-string array)))

(defun %transcript-plist-to-object (plist)
  "Convert a single transcript entry plist to a hash-table suitable
   for jzon."
  (let ((object (make-hash-table :test 'equal)))
    (loop :for (k v) :on plist :by #'cddr :do
          (setf (gethash (string-downcase (symbol-name k)) object)
                (%transcript-value-to-json-value v)))
    object))

(defun %transcript-value-to-json-value (value)
  "Recursively convert VALUE for JSON encoding. Plists become objects;
   non-plist lists become arrays; keywords become strings; other
   atoms pass through unchanged."
  (cond
    ((null value) +json-null+)
    ((keywordp value) (string-downcase (symbol-name value)))
    ((symbolp value) (string-downcase (symbol-name value)))
    ((%plist-p value) (%transcript-plist-to-object value))
    ((listp value) (map 'vector #'%transcript-value-to-json-value value))
    (t value)))

(defun %plist-p (value)
  "Return T if VALUE is a non-empty list of even length whose first
   element is a keyword."
  (and (listp value)
       (not (null value))
       (evenp (length value))
       (keywordp (first value))))

(defun sexp-to-markdown-entries (entries &key (session-id "unknown"))
  "Render ENTRIES as a Markdown document. SESSION-ID becomes the
   document title."
  (with-output-to-string (out)
    (format out "# MCP session ~A~%~%" session-id)
    (loop :for entry :in entries
          :for index :from 1
          :do (%markdown-render-entry out entry index))))

(defun %markdown-render-entry (out entry index)
  "Render one transcript ENTRY as a Markdown section to OUT."
  (let ((kind      (or (getf entry :kind) :unknown))
        (timestamp (or (getf entry :timestamp) "—"))
        (seq       (or (getf entry :seq) index)))
    (format out "## Entry ~A — ~A — ~A~%~%" seq timestamp kind)
    (loop :for (k v) :on entry :by #'cddr
          :unless (member k '(:seq :timestamp :kind))
            :do (format out "- **~A**: ~A~%"
                        (string-downcase (symbol-name k))
                        (%markdown-format-value v)))
    (terpri out)))

(defun %markdown-format-value (value)
  "Format a single transcript value for Markdown display. Compound
   values are rendered as inline code spans."
  (cond
    ((stringp value) value)
    ((null value) "_(empty)_")
    ((atom value) (princ-to-string value))
    (t (format nil "`~A`" value))))

;;;; End of file `transcript-render.lisp'
