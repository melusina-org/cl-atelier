;;;; define-tool.lisp — The DEFINE-TOOL macro

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp-kernel)

;;; DEFINE-TOOL is the single entry point for registering tools,
;;; resources, and dual-exposed definitions. Full design in
;;; references/define-tool-macro.md. The macro is a thin progn over
;;; helper functions, each kept under 25 lines for testability.

;;; The helper functions below all run at macro-expansion time.
;;; Signalled ERRORs propagate as COMPILE-FILE-ERRORs.

(defun %parse-define-tool-clauses (body)
  "Split BODY into (VALUES CLAUSES REST). CLAUSES is a plist whose
   keys are the clause indicators (:description, :resource, :tool)
   and whose values are the remaining forms of that clause. REST is
   everything after the last clause."
  (let ((clauses (make-hash-table))
        (cursor body))
    (loop :while (and cursor (%clause-form-p (first cursor)))
          :do (let ((form (first cursor)))
                (setf (gethash (first form) clauses) (rest form))
                (setf cursor (rest cursor))))
    (values
     (loop :for key :being :the :hash-keys :of clauses :using (:hash-value val)
           :collect key :collect val)
     cursor)))

(defun %clause-form-p (form)
  "Return T if FORM is a DEFINE-TOOL clause (a cons whose car is a
   recognised keyword indicator)."
  (and (consp form)
       (keywordp (first form))
       (member (first form) '(:description :resource :tool))))

(defun %clause-value (clauses key)
  "Return the value list for the clause KEY in CLAUSES, or NIL."
  (getf clauses key))

(defun %resource-option (resource-clause-value option)
  "Extract OPTION from the inner plist of a (:resource ...) clause.
   RESOURCE-CLAUSE-VALUE is the remainder of (:resource :uri ... :name ...)."
  (getf resource-clause-value option))

(defun %mime-type-string (value)
  "Normalise a :mime-type clause value to a string. Accepts a string
   or a keyword like :application/json."
  (cond
    ((stringp value) value)
    ((keywordp value) (string-downcase (symbol-name value)))
    (t (error "Unrecognised :mime-type value ~S (want string or keyword)."
              value))))

(defun %define-tool-class-name (name)
  "Derive the CLOS class name symbol for a tool NAME symbol.
   Interns NAME-TOOL in the same package as NAME."
  (intern (concatenate 'string (symbol-name name) "-TOOL")
          (symbol-package name)))

(defun %validate-define-tool-form (name lambda-list clauses)
  "Raise a compile-time ERROR if the form is invalid."
  (unless (symbolp name)
    (error "DEFINE-TOOL: name must be a symbol, got ~S." name))
  (%validate-lambda-list lambda-list)
  (unless (%clause-value clauses :description)
    (error "DEFINE-TOOL ~S: missing required (:description ...) clause." name))
  (%validate-resource-clause name lambda-list clauses)
  (%validate-tool-nil-clause name clauses))

(defun %validate-lambda-list (lambda-list)
  "Ensure LAMBDA-LIST is () or (&key ...)."
  (unless (or (null lambda-list)
              (eq (first lambda-list) '&key))
    (error "DEFINE-TOOL: lambda list must be () or (&key ...), got ~S."
           lambda-list)))

(defun %validate-resource-clause (name lambda-list clauses)
  "If a (:resource ...) clause is present, ensure its nested options
   are complete and that the URI template matches LAMBDA-LIST."
  (let ((resource (%clause-value clauses :resource)))
    (when resource
      (dolist (required '(:uri :name :mime-type))
        (unless (%resource-option resource required)
          (error "DEFINE-TOOL ~S: (:resource ...) is missing required ~S."
                 name required)))
      (validate-uri-template-against-lambda-list
       (%resource-option resource :uri) lambda-list))))

(defun %validate-tool-nil-clause (name clauses)
  "(:tool nil) is only valid when (:resource ...) is also present."
  (let ((tool-clause (%clause-value clauses :tool)))
    (when (and tool-clause
               (equal tool-clause '(nil))
               (not (%clause-value clauses :resource)))
      (error "DEFINE-TOOL ~S: (:tool nil) requires a (:resource ...) clause."
             name))))

(defun %define-tool-metadata-methods (class-name tool-name description clauses)
  "Return a list of defmethod forms for tool-name, tool-description,
   tool-input-schema plus, if there's a :resource clause, the resource
   reader methods. CLAUSES is the parsed clause plist."
  (let ((base (list
               `(defmethod tool-name        ((tool ,class-name)) ,tool-name)
               `(defmethod tool-description ((tool ,class-name)) ,description)
               `(defmethod tool-input-schema ((tool ,class-name))
                  (%build-input-schema-for-class ',class-name)))))
    (append base (%resource-metadata-methods class-name clauses))))

(defun %resource-metadata-methods (class-name clauses)
  "Return the resource metadata methods when (:resource ...) is
   present, empty list otherwise."
  (let ((resource (%clause-value clauses :resource)))
    (if resource
        (list
         `(defmethod resource-uri-template ((tool ,class-name))
            ,(%resource-option resource :uri))
         `(defmethod resource-name ((tool ,class-name))
            ,(%resource-option resource :name))
         `(defmethod resource-mime-type ((tool ,class-name))
            ,(%mime-type-string (%resource-option resource :mime-type))))
        '())))

(defvar *input-schema-cache* (make-hash-table :test 'eq)
  "Cache of class-name to input-schema hash-table. Populated lazily
   by %BUILD-INPUT-SCHEMA-FOR-CLASS on first read.")

(defvar *input-schema-source* (make-hash-table :test 'eq)
  "Cache of class-name to (lambda-list . body) used to derive the
   schema. Populated by DEFINE-TOOL at load time.")

(defun %build-input-schema-for-class (class-name)
  "Return the JSON Schema hash-table for CLASS-NAME, computing and
   caching it on first call."
  (or (gethash class-name *input-schema-cache*)
      (let ((source (gethash class-name *input-schema-source*)))
        (unless source
          (error "No input-schema source registered for ~S." class-name))
        (setf (gethash class-name *input-schema-cache*)
              (derive-input-schema-from-lambda-list (car source) (cdr source))))))

(defun %register-input-schema-source (class-name lambda-list body)
  "Remember the source used to derive CLASS-NAME's input schema so the
   accessor methods can compute it lazily on first read."
  (setf (gethash class-name *input-schema-source*) (cons lambda-list body))
  (remhash class-name *input-schema-cache*))

(defun %handle-tool-call-method (class-name lambda-list body)
  "Return the DEFMETHOD form for HANDLE-TOOL-CALL on CLASS-NAME.
   The method extracts the &key parameters from ARGUMENTS and binds
   them as local variables around BODY."
  (let ((params (%lambda-list-keyword-parameters lambda-list)))
    `(defmethod handle-tool-call ((tool ,class-name) arguments)
       (declare (ignorable arguments))
       (let ,(loop :for p :in params :collect (%bind-argument-form p))
         ,@body))))

(defun %bind-argument-form (param)
  "Return the LET binding form for extracting PARAM from ARGUMENTS."
  (let ((key (string-downcase (symbol-name param))))
    `(,param (cdr (assoc ,key arguments :test #'equal)))))

(defun %define-tool-expansion (name lambda-list clauses body)
  "Return the PROGN form that DEFINE-TOOL expands to."
  (let* ((class-name  (%define-tool-class-name name))
         (tool-name   (derive-tool-name-from-symbol name))
         (description (first (%clause-value clauses :description)))
         (register-as-tool
           (not (equal (%clause-value clauses :tool) '(nil))))
         (supers (if (%clause-value clauses :resource)
                     '(tool resource-tool)
                     '(tool))))
    `(progn
       (defclass ,class-name ,supers ())
       (%register-input-schema-source ',class-name ',lambda-list ',body)
       ,@(%define-tool-metadata-methods class-name tool-name description clauses)
       ,(%handle-tool-call-method class-name lambda-list body)
       (register-tool (make-instance ',class-name)
                      :register-as-tool ,register-as-tool)
       ',name)))

(defmacro define-tool (name lambda-list &body body)
  "Define an MCP tool, optionally also exposed as a resource.
   See references/define-tool-macro.md for the full syntax."
  (multiple-value-bind (clauses handler-body)
      (%parse-define-tool-clauses body)
    (%validate-define-tool-form name lambda-list clauses)
    (%define-tool-expansion name lambda-list clauses handler-body)))

;;;; End of file `define-tool.lisp'
