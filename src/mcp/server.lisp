;;;; server.lisp — serve-two-way-stream entry point

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

;;; The server reads JSON-RPC frames from a TWO-WAY-STREAM, parses
;;; them, dispatches via HANDLE-MESSAGE, and writes the response back.
;;; SERVE-TWO-WAY-STREAM is the only entry point shipped in slice 009.
;;; Slice 010 will add a thin shell wrapper that exec's SBCL with this
;;; called from --eval, but the dump-image work is its own slice.

(defclass mcp-server ()
  ((stream
    :initarg :stream
    :reader server-stream
    :type two-way-stream)
   (transcript
    :initarg :transcript
    :reader server-transcript
    :type (or transcript null)
    :initform nil)
   (output-mutex
    :reader server-output-mutex
    :initform (bordeaux-threads:make-lock "atelier/mcp output"))
   (child-connection
    :accessor server-child-connection
    :initform nil
    :documentation "The session's child SBCL connection, created lazily
     on the first eval-form call. Shut down when the session ends."))
  (:documentation
   "An MCP server instance bound to a two-way stream and an optional
    session transcript. Single output mutex serialises response
    writes; even though slice 009 is single-threaded, the discipline
    is set up here for slice 010's eventual concurrent dispatch."))

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

(defun serve-two-way-stream (&optional
                               (stream (make-two-way-stream
                                        *standard-input*
                                        *standard-output*)))
  "Serve the MCP protocol over STREAM. Reads JSON-RPC requests one
   line at a time, dispatches them, and writes responses to the same
   stream. Returns when the input side reaches EOF.

   STREAM defaults to a two-way-stream wrapping *standard-input* and
   *standard-output*, which is the production launch path.
   Test paths pass an explicit stream built from string-input-streams
   and string-output-streams to drive the server with recorded fixtures."
  (%ensure-sbcl-home)
  (let* ((transcript (handler-case (make-transcript)
                       (error () nil)))
         (server (make-instance 'mcp-server
                                :stream stream
                                :transcript transcript)))
    (%serve-loop server)))

(defun ensure-child-connection (server)
  "Return the session's child-connection, creating one lazily on
   first call. If the existing child is dead, spawn a fresh one."
  (let ((conn (server-child-connection server)))
    (cond
      ((and conn (connection-alive-p conn)) conn)
      (t
       (when conn
         (ignore-errors (connection-shutdown conn)))
       (let ((new-conn (make-child-connection)))
         (setf (server-child-connection server) new-conn)
         new-conn)))))

(defun %shutdown-child-if-present (server)
  "Shut down the session's child connection if one exists."
  (let ((conn (server-child-connection server)))
    (when conn
      (ignore-errors (connection-shutdown conn))
      (setf (server-child-connection server) nil))))

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
