;;;; server.lisp — serve-two-way-stream entry point (MCP kernel)

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp-kernel)

;;; The server reads JSON-RPC frames from a TWO-WAY-STREAM, parses
;;; them, dispatches via HANDLE-MESSAGE, and writes responses back.
;;; SERVE-TWO-WAY-STREAM is the only entry point shipped in the kernel.
;;; The connection-class/connection-initargs pattern (borrowed from
;;; Hunchentoot's acceptor) allows concrete transports to plug in
;;; without the kernel knowing about SWANK, TCP, or any specific
;;; transport mechanism.

(defclass mcp-server ()
  ((stream
    :initarg :stream
    :reader server-stream
    :type two-way-stream)
   (transcript
    :initarg :transcript
    :reader server-transcript
    :type t
    :initform nil
    :documentation "Transcript object (opaque to the kernel). Set by the
     transport layer's serve-two-way-stream wrapper.")
   (output-mutex
    :reader server-output-mutex
    :initform (bordeaux-threads:make-lock "atelier/mcp output"))
   (connection-class
    :initarg :connection-class
    :reader server-connection-class
    :initform 'image-connection
    :documentation "Class designator for make-instance when spawning a
     connection. Set by the transport layer.")
   (connection-initargs
    :initarg :connection-initargs
    :reader server-connection-initargs
    :initform nil
    :documentation "Plist of initargs passed to make-instance for the
     connection class.")
   (max-children
    :initarg :max-children
    :reader server-max-children
    :initform 3
    :type (integer 1)
    :documentation "Maximum concurrent child connections.")
   (children
    :accessor server-children
    :initform nil
    :type list
    :documentation "List of active IMAGE-CONNECTION instances.")
   (child-connection
    :accessor server-child-connection
    :initform nil
    :documentation "The session's primary child connection, created lazily
     on the first eval-form call. Shut down when the session ends."))
  (:documentation
   "An MCP server instance bound to a two-way stream. The
    connection-class and connection-initargs slots parametrise child
    connection creation (Hunchentoot acceptor pattern). The kernel never
    mentions any concrete connection subclass."))

(defvar *sbcl-home-at-build-time* nil
  "Captured at load time so the dumped image remembers where SBCL lived.")

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((home (sb-int:sbcl-homedir-pathname)))
    (when home
      (setf *sbcl-home-at-build-time* (namestring home)))))

(defun %ensure-sbcl-home ()
  "Set SBCL_HOME if not already set. When running from a dumped image,
   sb-int:sbcl-homedir-pathname may return NIL, so we fall back to the
   value captured at build time."
  #+sbcl
  (unless (uiop:getenv "SBCL_HOME")
    (let ((home (or (let ((p (sb-int:sbcl-homedir-pathname)))
                      (when p (namestring p)))
                    *sbcl-home-at-build-time*)))
      (when home
        (setf (uiop:getenv "SBCL_HOME") home))))
  (values))

(defun %make-connection (server)
  "Create a new child connection using the server's connection-class
   and connection-initargs. Registers the connection in SERVER-CHILDREN."
  (let ((conn (apply #'make-instance
                     (server-connection-class server)
                     (server-connection-initargs server))))
    (push conn (server-children server))
    conn))

(defun %enforce-child-cap (server)
  "If SERVER-CHILDREN exceeds SERVER-MAX-CHILDREN, shut down the oldest
   idle connection to make room."
  (loop :while (>= (length (server-children server))
                   (server-max-children server))
        :for oldest := (car (last (server-children server)))
        :do (ignore-errors (connection-shutdown oldest))
            (setf (server-children server)
                  (remove oldest (server-children server)))))

(defun ensure-child-connection (server)
  "Return the session's child-connection, creating one lazily on
   first call. If the existing child is dead, spawn a fresh one.
   Enforces the max-children cap."
  (let ((conn (server-child-connection server)))
    (cond
      ((and conn (connection-alive-p conn)) conn)
      (t
       (when conn
         (ignore-errors (connection-shutdown conn))
         (setf (server-children server)
               (remove conn (server-children server))))
       (%enforce-child-cap server)
       (let ((new-conn (%make-connection server)))
         (setf (server-child-connection server) new-conn)
         new-conn)))))

(defun %shutdown-child-if-present (server)
  "Shut down all child connections."
  (let ((conn (server-child-connection server)))
    (when conn
      (ignore-errors (connection-shutdown conn))
      (setf (server-child-connection server) nil)))
  (dolist (child (server-children server))
    (ignore-errors (connection-shutdown child)))
  (setf (server-children server) nil))

(defun %serve-loop (server)
  "The read-dispatch-write loop. Honours EOF as end-of-session.
   Shuts down any child connection when the session ends."
  (unwind-protect
       (let ((stream (server-stream server)))
         (loop :for line := (read-line stream nil nil)
               :while line
               :do (%serve-one-line server line)))
    (%shutdown-child-if-present server)))

(defun %serve-one-line (server line)
  "Handle a single line: parse, dispatch, write the response.
   Catches per-message conditions so the server keeps running."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
    (when (plusp (length trimmed))
      (handler-case (%dispatch-line server trimmed)
        (error (c) (%write-internal-error server c))))))

(defun %dispatch-line (server line)
  "Parse LINE as JSON-RPC, dispatch via HANDLE-MESSAGE, write any
   response. Logs the request and response to the transcript."
  (let* ((parsed   (handler-case (decode-from-string line)
                     (error () (%write-parse-error server) (return-from %dispatch-line))))
         (message  (parse-mcp-message parsed))
         (response (handle-message message server)))
    (%transcript-message server message)
    (when response
      (%transcript-message server response)
      (%write-response server response))))

(defgeneric write-transcript-entry (transcript entry)
  (:documentation
   "Write ENTRY to TRANSCRIPT. Defined in the kernel as a generic
    function so the transport layer can provide the concrete method.
    The kernel default is a no-op.")
  (:method (transcript entry)
    (declare (ignore transcript entry))
    nil))

(defun serve-two-way-stream (&optional
                               (stream (make-two-way-stream
                                        *standard-input*
                                        *standard-output*))
                               &rest server-initargs)
  "Serve the MCP protocol over STREAM. Reads JSON-RPC requests one
   line at a time, dispatches them, and writes responses to the same
   stream. Returns when the input side reaches EOF.

   SERVER-INITARGS are passed to MAKE-INSTANCE of MCP-SERVER and
   should include :CONNECTION-CLASS and :CONNECTION-INITARGS for the
   transport layer to configure child connections."
  (%ensure-sbcl-home)
  (let ((server (apply #'make-instance 'mcp-server
                       :stream stream
                       server-initargs)))
    (%serve-loop server)))

(defun %transcript-message (server message)
  "Append MESSAGE to the server's transcript, when one is configured."
  (let ((transcript (server-transcript server)))
    (when transcript
      (handler-case
          (write-transcript-entry transcript (%message-to-transcript-entry message))
        (error () nil)))))

(defun %message-to-transcript-entry (message)
  "Render a message as a transcript plist entry. Tags by class name
   so the consumer can typecase later."
  (let ((class (string-downcase (symbol-name (class-name (class-of message))))))
    (cond
      ((typep message 'mcp-request)
       (list :kind class :id (request-id message) :method (request-method message)))
      ((typep message 'mcp-notification)
       (list :kind class :method (request-method message)))
      ((typep message 'mcp-success-response)
       (list :kind class :id (request-id message)))
      ((typep message 'mcp-error-response)
       (list :kind class :id (request-id message)
             :code (error-code message) :message (error-message message))))))

(defun %write-response (server response)
  "Encode RESPONSE and write it as one line. Held under the output mutex."
  (let ((line (response-to-json-line response)))
    (bordeaux-threads:with-lock-held ((server-output-mutex server))
      (let ((stream (server-stream server)))
        (write-string line stream)
        (terpri stream)
        (finish-output stream)))))

(defun response-to-json-line (response)
  "Build the JSON-RPC envelope for RESPONSE as a single line of JSON."
  (encode-to-string
   (cond
     ((typep response 'mcp-success-response)
      (make-json-object
       "jsonrpc" "2.0"
       "id"      (request-id response)
       "result"  (response-result response)))
     ((typep response 'mcp-error-response)
      (make-json-object
       "jsonrpc" "2.0"
       "id"      (request-id response)
       "error"   (%error-to-json-object response))))))

(defun %error-to-json-object (response)
  "Build the JSON object for an MCP error response, with optional data."
  (let ((object (make-json-object
                 "code"    (error-code response)
                 "message" (error-message response))))
    (let ((data (error-data response)))
      (when data (setf (gethash "data" object) data)))
    object))

(defun %write-parse-error (server)
  "Write a JSON-RPC parse error response with id null."
  (let ((line (encode-to-string
               (make-json-object
                "jsonrpc" "2.0"
                "id"      +json-null+
                "error"   (make-json-object
                           "code"    -32700
                           "message" "Parse error")))))
    (bordeaux-threads:with-lock-held ((server-output-mutex server))
      (let ((stream (server-stream server)))
        (write-string line stream)
        (terpri stream)
        (finish-output stream)))))

(defun %write-internal-error (server condition)
  "Write a JSON-RPC -32603 internal error from CONDITION."
  (let ((line (encode-to-string
               (make-json-object
                "jsonrpc" "2.0"
                "id"      +json-null+
                "error"   (make-json-object
                           "code"    -32603
                           "message" (princ-to-string condition))))))
    (bordeaux-threads:with-lock-held ((server-output-mutex server))
      (let ((stream (server-stream server)))
        (write-string line stream)
        (terpri stream)
        (finish-output stream)))))

;;;; End of file `server.lisp'
