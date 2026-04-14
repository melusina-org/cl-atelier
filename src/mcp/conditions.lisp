;;;; conditions.lisp — Condition classes for Atelier MCP

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp-kernel)

(define-condition mcp-error (error)
  ((message
    :initarg :message
    :reader error-message
    :type string
    :documentation "Human-readable description of the failure."))
  (:default-initargs :message "Unspecified MCP error.")
  (:documentation
   "Base class for all Atelier MCP error conditions.
    Never signalled directly; subclasses carry the specifics."))

(defmethod print-object ((c mcp-error) stream)
  (print-unreadable-object (c stream :type t)
    (format stream "~A" (error-message c))))

(define-condition tool-not-found (mcp-error)
  ((tool-name
    :initarg :tool-name
    :reader tool-not-found-tool-name
    :type string
    :documentation "The name of the tool that was requested."))
  (:documentation
   "Signalled when a tools/call request names a tool that is not
    registered in *tool-registry*. The dispatcher converts this to a
    JSON-RPC -32601 Method not found error at the protocol boundary."))

(define-condition resource-not-found (mcp-error)
  ((uri
    :initarg :uri
    :reader resource-not-found-uri
    :type string
    :documentation "The URI that did not match any registered resource."))
  (:documentation
   "Signalled when a resources/read request names a URI that matches
    neither a concrete resource nor any URI template. The dispatcher
    converts this to a JSON-RPC -32002 Resource not found error (the
    MCP-specific code) at the protocol boundary."))

(define-condition not-implemented (mcp-error)
  ((operation
    :initarg :operation
    :reader not-implemented-operation
    :type symbol
    :documentation "The generic function that has no applicable method.")
   (class
    :initarg :class
    :reader not-implemented-class
    :type symbol
    :documentation "The class for which no method was defined."))
  (:documentation
   "Signalled by abstract protocol methods on image-connection when
    called on a class that does not override them. Slice 009 ships no
    concrete image-connection subclass; slice 010 will provide
    swank-connection and its concrete methods."))

(define-condition invalid-tool-arguments (mcp-error)
  ((tool-name
    :initarg :tool-name
    :reader invalid-tool-arguments-tool-name
    :type string
    :documentation "The tool whose arguments failed validation.")
   (errors
    :initarg :errors
    :reader invalid-tool-arguments-errors
    :type list
    :documentation "A list of per-argument error strings."))
  (:documentation
   "Signalled by tools/call when the arguments object does not match
    the tool's declared input-schema. The dispatcher converts this to a
    JSON-RPC -32602 Invalid params error at the protocol boundary."))

(define-condition invalid-uri-template (mcp-error)
  ((template
    :initarg :template
    :reader invalid-uri-template-template
    :type string
    :documentation "The URI template string.")
   (lambda-list
    :initarg :lambda-list
    :reader invalid-uri-template-lambda-list
    :type list
    :documentation "The &key lambda list that was supposed to match.")
   (mismatch
    :initarg :mismatch
    :reader invalid-uri-template-mismatch
    :type string
    :documentation "Description of the mismatch."))
  (:documentation
   "Signalled at macro-expansion time by define-tool when the URI
    template placeholders do not match the &key parameters. This is a
    compile-time error and prevents the defining system from loading."))

(define-condition child-image-spawn-failed (mcp-error)
  ((reason
    :initarg :reason
    :reader child-image-spawn-failed-reason
    :type string
    :documentation "Description of why the child image failed to start."))
  (:documentation
   "Signalled when make-child-connection fails to spawn a child SBCL,
    connect to its SWANK server, or read the assigned port. Common
    causes: SBCL not on PATH, startup timeout, port not received."))

(define-condition debugger-active (mcp-error)
  ()
  (:default-initargs :message "Debugger is active — invoke a restart or abort before evaluating.")
  (:documentation
   "Signalled when eval-form is called while the child image is in the
    debugger. The agent must invoke a restart or abort before issuing
    a new eval."))

;;;; End of file `conditions.lisp'
