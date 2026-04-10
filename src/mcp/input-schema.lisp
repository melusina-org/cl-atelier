;;;; input-schema.lisp — Derive JSON Schema from a Lisp lambda list

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

;;; JSON Schema derivation rules live in references/define-tool-macro.md.
;;; Supported lambda lists: empty () and keyword-only (&key a b c).
;;; Type declarations map to JSON Schema types via %type-to-schema.
;;; Every &key parameter is required — no optional keyword arguments.

(defun %lambda-list-keyword-parameters (lambda-list)
  "Extract the list of keyword parameter symbol names from LAMBDA-LIST.
   LAMBDA-LIST is either () or a form starting with &KEY.
   Signals ERROR on any other shape."
  (cond
    ((null lambda-list) nil)
    ((eq (first lambda-list) '&key)
     (mapcar (lambda (arg) (if (consp arg) (first arg) arg))
             (rest lambda-list)))
    (t
     (error
      "Unsupported lambda list ~S for define-tool: expected () or (&key ...)."
      lambda-list))))

(defun %type-to-schema (type)
  "Translate a CL type specifier TYPE to a JSON Schema type hash-table.
   Returns an empty hash-table for unrecognised types — the schema will
   declare the key but leave its type unconstrained."
  (cond
    ((eq type 'string)        (make-json-object "type" "string"))
    ((eq type 'integer)       (make-json-object "type" "integer"))
    ((eq type 'boolean)       (make-json-object "type" "boolean"))
    ((member type '(real number float double-float single-float))
     (make-json-object "type" "number"))
    ((and (consp type) (eq (first type) 'integer))
     (%integer-range-schema type))
    ((and (consp type) (eq (first type) 'member))
     (%member-schema (rest type)))
    (t
     (make-json-object))))

(defun %integer-range-schema (type)
  "Build a schema for (integer LO HI) with optional range bounds."
  (let ((schema (make-json-object "type" "integer"))
        (lo (second type))
        (hi (third type)))
    (when (numberp lo) (setf (gethash "minimum" schema) lo))
    (when (numberp hi) (setf (gethash "maximum" schema) hi))
    schema))

(defun %member-schema (members)
  "Build a schema for (member ...). Represented as a string enum."
  (make-json-object
   "type" "string"
   "enum" (map 'vector (lambda (m) (string-downcase (string m))) members)))

(defun %extract-type-declarations (body)
  "Return an alist mapping parameter-name symbols to CL type specifiers
   from (declare (type ...)) forms at the start of BODY."
  (let ((result nil))
    (dolist (form body result)
      (when (and (consp form) (eq (first form) 'declare))
        (dolist (decl (rest form))
          (when (and (consp decl) (eq (first decl) 'type))
            (let ((type (second decl)))
              (dolist (var (cddr decl))
                (push (cons var type) result)))))))))

(defun derive-input-schema-from-lambda-list (lambda-list body)
  "Return a JSON Schema hash-table for LAMBDA-LIST. BODY is the
   handler body; its leading (declare (type ...)) forms refine the
   per-parameter schema. See references/define-tool-macro.md."
  (let ((params (%lambda-list-keyword-parameters lambda-list))
        (types  (%extract-type-declarations body))
        (properties (make-json-object)))
    (dolist (param params)
      (let* ((type (cdr (assoc param types)))
             (schema (if type (%type-to-schema type) (make-json-object))))
        (setf (gethash (string-downcase (symbol-name param)) properties)
              schema)))
    (let ((result (make-json-object "type" "object" "properties" properties)))
      (when params
        (setf (gethash "required" result)
              (map 'vector (lambda (p) (string-downcase (symbol-name p)))
                   params)))
      result)))

;;;; End of file `input-schema.lisp'
