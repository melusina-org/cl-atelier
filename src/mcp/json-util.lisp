;;;; json-util.lisp — jzon wrappers and sexp-to-json walker

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

;;; Canonical JSON literal constants.
;;;
;;; See references/jzon-round-trip.md for why these values were chosen:
;;; JSON null parses to the symbol cl:null (not nil); JSON false parses
;;; to nil; JSON true parses to t. CL nil stringifies to "false", so
;;; "absence of value" and "JSON false" are not distinguishable in nil
;;; alone — be explicit when you mean one or the other.

(alexandria:define-constant +json-null+ 'cl:null
  :test #'eq
  :documentation "The Lisp value that round-trips with JSON null.")

(alexandria:define-constant +json-true+ t
  :test #'eq
  :documentation "The Lisp value that round-trips with JSON true.")

(alexandria:define-constant +json-false+ nil
  :test #'eq
  :documentation
  "The Lisp value that round-trips with JSON false. Also CL nil and the
   empty list — be explicit when the intent matters.")

;;; The unique sentinel for missing hash-table keys. Used with the
;;; three-argument form of gethash to distinguish "absent" from "null".

(defvar *missing-key-sentinel* (gensym "MISSING-"))

(defun missing-key ()
  "Return the unique sentinel used to mark absent JSON object keys.
   Use as: (gethash \"id\" obj (missing-key))"
  *missing-key-sentinel*)

;;; Core encode / decode wrappers.

(defun encode-to-string (object)
  "Encode OBJECT to a JSON string via jzon:stringify.
   OBJECT is expected to be a hash-table tree built by make-json-object
   or alist-to-json-object / plist-to-json-object helpers. Plain alists
   and plists do NOT encode as objects by default; they become arrays
   or errors. See references/jzon-round-trip.md."
  (com.inuoe.jzon:stringify object))

(defun decode-from-string (string)
  "Parse STRING as JSON via jzon:parse.
   Returns a hash-table for objects, a vector for arrays, the symbol
   cl:null for JSON null, t for true, nil for false, or the bare value
   for numbers and strings."
  (com.inuoe.jzon:parse string))

;;; Hash-table construction helpers. All three produce a hash-table with
;;; EQUAL test and string keys — the shape jzon:stringify emits as a
;;; JSON object. SBCL preserves insertion order, which is load-bearing
;;; for round-trip determinism in fixture tests.

(defun make-json-object (&rest pairs)
  "Build a hash-table from alternating string keys and values.
   (make-json-object \"a\" 1 \"b\" 2) ==> {\"a\":1,\"b\":2}"
  (let ((h (make-hash-table :test 'equal)))
    (loop :for (k v) :on pairs :by #'cddr :do
          (setf (gethash k h) v))
    h))

(defun alist-to-json-object (alist)
  "Convert an alist with string keys to a hash-table suitable for
   jzon:stringify. Values are not recursively converted — nested lists
   and alists pass through unchanged."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (entry alist h)
      (setf (gethash (car entry) h) (cdr entry)))))

(defun plist-to-json-object (plist)
  "Convert a plist with keyword keys to a hash-table suitable for
   jzon:stringify. Keys are lowercased."
  (let ((h (make-hash-table :test 'equal)))
    (loop :for (k v) :on plist :by #'cddr :do
          (setf (gethash (string-downcase (symbol-name k)) h) v))
    h))

(defun json-object-to-alist (object)
  "Inverse of alist-to-json-object. OBJECT is a hash-table parsed from
   JSON. Returns an alist with string keys, useful in tests."
  (loop :for k :being :the :hash-keys :of object :using (:hash-value v)
        :collect (cons k v)))

;;;; End of file `json-util.lisp'
