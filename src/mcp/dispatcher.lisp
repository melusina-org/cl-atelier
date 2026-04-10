;;;; dispatcher.lisp — handle-message methods for MCP requests

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

;;; The dispatcher implements one HANDLE-MESSAGE method per concrete
;;; request class declared in message.lisp, plus an :around for
;;; tools/call and resources/read that catches handler conditions
;;; and converts them to in-band errors. The :after method on
;;; mcp-message lives in server.lisp because it depends on the
;;; per-server transcript instance.

;;; ----- Helpers for building responses -----

(defun %make-success-response (request result)
  "Build an MCP-SUCCESS-RESPONSE for REQUEST with RESULT (a hash-table)."
  (make-instance 'mcp-success-response
                 :id (request-id request)
                 :request request
                 :result result))

(defun %make-error-response (request code message &optional data)
  "Build an MCP-ERROR-RESPONSE for REQUEST."
  (make-instance 'mcp-error-response
                 :id (request-id request)
                 :request request
                 :code code
                 :message message
                 :data data))

(defun %tool-call-content-text (text &key error-p)
  "Build the result hash-table for a tools/call response wrapping
   TEXT in a single text content item."
  (make-json-object
   "content" (vector (make-json-object "type" "text" "text" text))
   "isError" (if error-p +json-true+ +json-false+)))

(defun %resource-content-text (uri mime-type text)
  "Build the result hash-table for a resources/read response."
  (make-json-object
   "contents" (vector (make-json-object
                       "uri" uri
                       "mimeType" mime-type
                       "text" text))))

(defun %encode-handler-result (value mime-type)
  "Convert a handler return VALUE to a string suitable for the
   text envelope, choosing JSON encoding based on MIME-TYPE."
  (cond
    ((stringp value) value)
    ((or (null mime-type) (search "json" mime-type :test #'char-equal))
     (encode-to-string (%lispify-handler-result value)))
    (t (princ-to-string value))))

(defun %lispify-handler-result (value)
  "Recursively convert a Lisp data structure into the hash-table /
   vector form jzon expects.
   - NIL becomes an empty vector (JSON []).
   - A hash-table passes through unchanged.
   - An alist (list whose every entry is a dotted pair with an atomic
     car) becomes a JSON object via hash-table conversion.
   - Any other non-empty list becomes a JSON array.
   - The symbol CL:NULL is the explicit way to emit a JSON null from
     a handler. Plain NIL means \"no items\", not \"null\"."
  (cond
    ((eq value 'cl:null) +json-null+)
    ((null value) #())
    ((hash-table-p value) value)
    ((%alist-shape-p value) (%alist-to-hash-table value))
    ((listp value) (map 'vector #'%lispify-handler-result value))
    (t value)))

(defun %alist-shape-p (value)
  "Return T iff VALUE is a non-empty list and every element is a CONS
   whose CAR is atomic (string, symbol, or number) — the alist shape."
  (and (consp value)
       (every (lambda (entry)
                (and (consp entry)
                     (let ((k (car entry)))
                       (or (stringp k) (symbolp k) (numberp k)))))
              value)))

(defun %alist-to-hash-table (alist)
  "Convert ALIST into a hash-table, recursively lispifying its values."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (entry alist h)
      (let ((key (if (stringp (car entry)) (car entry)
                     (string-downcase (string (car entry))))))
        (setf (gethash key h) (%lispify-handler-result (cdr entry)))))))

;;; ----- Initialize -----

(defmethod handle-message ((request initialize-request) server)
  (declare (ignore server))
  (%make-success-response request (%initialize-result)))

(defun %initialize-result ()
  "Build the initialize response payload."
  (make-json-object
   "protocolVersion" +mcp-protocol-version+
   "capabilities" (make-json-object
                   "tools" (make-json-object "listChanged" +json-false+)
                   "resources" (make-json-object
                                "subscribe" +json-false+
                                "listChanged" +json-false+))
   "serverInfo" (make-json-object
                 "name" "org.melusina.atelier/mcp"
                 "version" "0.1.0")))

(defmethod handle-message ((notification initialized-notification) server)
  (declare (ignore notification server))
  nil)

;;; ----- Tools/list -----

(defmethod handle-message ((request tools-list-request) server)
  (declare (ignore server))
  (let ((tools (map 'vector #'%tool-to-json-object (list-tools))))
    (%make-success-response request (make-json-object "tools" tools))))

(defun %tool-to-json-object (tool)
  "Convert TOOL to a JSON object suitable for the tools/list array."
  (make-json-object
   "name" (tool-name tool)
   "description" (tool-description tool)
   "inputSchema" (tool-input-schema tool)))

;;; ----- Tools/call -----

(defmethod handle-message ((request tools-call-request) server)
  (declare (ignore server))
  (let* ((params (request-params request))
         (name   (and params (gethash "name" params)))
         (args-h (and params (gethash "arguments" params)))
         (args   (%hash-table-to-alist args-h))
         (tool   (find-tool-by-name name)))
    (if tool
        (%dispatch-tool-call request tool args)
        (%make-error-response request -32601
                              (format nil "Unknown tool: ~A" name)))))

(defun %dispatch-tool-call (request tool args)
  "Run TOOL's handler with ARGS and wrap the result, catching
   conditions to in-band tool-call errors."
  (handler-case
      (let ((value (handle-tool-call tool args)))
        (%make-success-response
         request
         (%tool-call-content-text
          (%encode-handler-result value (resource-mime-type tool)))))
    (error (c)
      (%make-success-response
       request
       (%tool-call-content-text (princ-to-string c) :error-p t)))))

(defun %hash-table-to-alist (h)
  "Return an alist of (string . value) pairs from H, or NIL if H is
   nil."
  (when (hash-table-p h)
    (loop :for k :being :the :hash-keys :of h :using (:hash-value v)
          :collect (cons k v))))

;;; ----- Resources/list -----

(defmethod handle-message ((request resources-list-request) server)
  (declare (ignore server))
  (let ((resources (map 'vector #'%concrete-resource-to-json-object
                        (list-concrete-resources))))
    (%make-success-response request (make-json-object "resources" resources))))

(defun %concrete-resource-to-json-object (tool)
  "Convert a concrete resource TOOL to a JSON object."
  (make-json-object
   "uri" (resource-uri-template tool)
   "name" (resource-name tool)
   "description" (tool-description tool)
   "mimeType" (resource-mime-type tool)))

;;; ----- Resources/templates/list -----

(defmethod handle-message ((request resources-templates-list-request) server)
  (declare (ignore server))
  (let ((templates (map 'vector #'%template-resource-to-json-object
                        (list-template-resources))))
    (%make-success-response request
                            (make-json-object "resourceTemplates" templates))))

(defun %template-resource-to-json-object (tool)
  "Convert a templated resource TOOL to a JSON object."
  (make-json-object
   "uriTemplate" (resource-uri-template tool)
   "name" (resource-name tool)
   "description" (tool-description tool)
   "mimeType" (resource-mime-type tool)))

;;; ----- Resources/read -----

(defmethod handle-message ((request resources-read-request) server)
  (declare (ignore server))
  (let* ((params (request-params request))
         (uri    (and params (gethash "uri" params))))
    (if uri
        (%dispatch-resource-read request uri)
        (%make-error-response request -32602 "resources/read missing uri parameter"))))

(defun %dispatch-resource-read (request uri)
  "Look up URI in the resource registries and dispatch the handler."
  (multiple-value-bind (tool bindings) (match-resource-uri uri)
    (if tool
        (%run-resource-handler request tool uri bindings)
        (%make-error-response request -32002
                              (format nil "Resource not found: ~A" uri)
                              (make-json-object "uri" uri)))))

(defun %run-resource-handler (request tool uri bindings)
  "Invoke TOOL's handler with BINDINGS and wrap the result for resources/read.
   RESOURCE-NOT-FOUND signalled by the handler is mapped to -32002 with
   the offending URI as data; any other error becomes -32603."
  (handler-case
      (let* ((value (handle-tool-call tool bindings))
             (mime  (resource-mime-type tool))
             (text  (%encode-handler-result value mime)))
        (%make-success-response request (%resource-content-text uri mime text)))
    (resource-not-found (c)
      (%make-error-response request -32002 (error-message c)
                            (make-json-object "uri" uri)))
    (error (c)
      (%make-error-response request -32603 (princ-to-string c)))))

;;; ----- Fallback for unknown methods -----

(defmethod handle-message ((request mcp-request) server)
  (declare (ignore server))
  (%make-error-response request -32601
                        (format nil "Method not found: ~A"
                                (request-method request))))

(defmethod handle-message ((notification mcp-notification) server)
  (declare (ignore notification server))
  nil)

;;;; End of file `dispatcher.lisp'
