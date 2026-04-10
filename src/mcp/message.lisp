;;;; message.lisp — MCP message hierarchy and parser

;;;; Atelier (https://github.com/melusina-org/atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

;;; The class hierarchy mirrors JSON-RPC 2.0 with MCP method classes
;;; as concrete request subclasses. See references/message-hierarchy.md.

(defclass mcp-message ()
  ((raw-json
    :initarg :raw-json
    :reader message-raw-json
    :type (or hash-table null)
    :documentation "Original parsed JSON object preserved for the transcript.")
   (timestamp
    :initarg :timestamp
    :reader message-timestamp
    :initform (get-universal-time)
    :type integer
    :documentation "Universal-time integer when the message was parsed."))
  (:documentation "Abstract base for any MCP wire message."))

(defclass mcp-request (mcp-message)
  ((id
    :initarg :id
    :reader request-id
    :documentation "JSON-RPC id (integer or string).")
   (method
    :initarg :method
    :reader request-method
    :type string
    :documentation "JSON-RPC method string.")
   (params
    :initarg :params
    :reader request-params
    :type (or hash-table null)
    :initform nil
    :documentation "JSON-RPC params object or NIL when absent."))
  (:documentation "Abstract base for any MCP request."))

(defclass mcp-notification (mcp-message)
  ((method
    :initarg :method
    :reader request-method
    :type string)
   (params
    :initarg :params
    :reader request-params
    :type (or hash-table null)
    :initform nil))
  (:documentation
   "Abstract base for any MCP notification. Notifications never carry
    an id field and never receive a response."))

(defclass mcp-response (mcp-message)
  ((id
    :initarg :id
    :reader request-id)
   (request
    :initarg :request
    :reader response-request
    :initform nil
    :type (or mcp-request null)
    :documentation "Back-link to the originating request."))
  (:documentation "Abstract base for any MCP response."))

(defclass mcp-success-response (mcp-response)
  ((result
    :initarg :result
    :reader response-result
    :type hash-table
    :documentation "Response payload as a hash-table.")))

(defclass mcp-error-response (mcp-response)
  ((code
    :initarg :code
    :reader error-code
    :type integer)
   (message
    :initarg :message
    :reader error-message
    :type string)
   (data
    :initarg :data
    :reader error-data
    :initform nil
    :documentation "Optional structured error data.")))

;;; Concrete request and notification classes — one per MCP method
;;; that Atelier slice 009 handles. Total: 6 request classes + 1
;;; notification class = 7 entries in the method-to-class table below.

(defclass initialize-request (mcp-request) ())
(defclass tools-list-request (mcp-request) ())
(defclass tools-call-request (mcp-request) ())
(defclass resources-list-request (mcp-request) ())
(defclass resources-templates-list-request (mcp-request) ())
(defclass resources-read-request (mcp-request) ())
(defclass initialized-notification (mcp-notification) ())

;;; Method-to-class table populated at load time.

(defvar *method-to-class-table* (make-hash-table :test 'equal)
  "Hash-table mapping MCP method strings to message classes.")

(defun %register-method-class (method-string class-name)
  "Install METHOD-STRING -> CLASS-NAME in the dispatch table."
  (setf (gethash method-string *method-to-class-table*) class-name))

(%register-method-class "initialize"               'initialize-request)
(%register-method-class "notifications/initialized" 'initialized-notification)
(%register-method-class "tools/list"               'tools-list-request)
(%register-method-class "tools/call"               'tools-call-request)
(%register-method-class "resources/list"           'resources-list-request)
(%register-method-class "resources/templates/list" 'resources-templates-list-request)
(%register-method-class "resources/read"           'resources-read-request)

;;; Parser. Converts a jzon-decoded hash-table into a typed message.

(defun parse-mcp-message (json)
  "Parse JSON (a hash-table from jzon:parse) into an MCP-MESSAGE
   instance. Dispatches on the method string and the presence of an
   id to choose between request and notification classes.
   Signals INVALID-REQUEST conditions for malformed envelopes."
  (let ((method (gethash "method" json))
        (id     (gethash "id" json (missing-key))))
    (unless (stringp method)
      (error 'mcp-error :message "method field missing or not a string"))
    (when (eq id 'cl:null)
      (error 'mcp-error :message "id field is JSON null (invalid request)"))
    (%instantiate-message json method id)))

(defun %instantiate-message (json method id)
  "Build the typed message instance. Notifications are constructed
   without the :id initarg because MCP-NOTIFICATION has no id slot."
  (let* ((class (%resolve-message-class method id))
         (notification-p (subtypep class 'mcp-notification))
         (params (gethash "params" json))
         (base   (list :raw-json json :method method :params params)))
    (apply #'make-instance class
           (if notification-p base (list* :id id base)))))

(defun %resolve-message-class (method id)
  "Pick the message class for METHOD given the absence (notification)
   or presence (request) of ID. Falls through to the abstract base
   class for unknown methods."
  (let ((registered (gethash method *method-to-class-table*)))
    (cond
      (registered registered)
      ((eq id (missing-key)) 'mcp-notification)
      (t 'mcp-request))))

(defgeneric handle-message (message server)
  (:documentation
   "Dispatch MESSAGE in the context of SERVER. Returns an
    MCP-RESPONSE for requests and NIL for notifications. Specialised
    by class in dispatcher.lisp."))

;;;; End of file `message.lisp'
