;;;; swank-protocol.lisp — Minimal SWANK wire protocol client

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

;;; This file implements a minimal SWANK client — just enough to send
;;; :emacs-rex messages and receive :return results. The wire protocol
;;; is 6-char hex length prefix + UTF-8 s-expression payload.
;;;
;;; See references/swank-protocol.md for the full protocol description.

(defclass swank-connection ()
  ((usocket
    :initarg :usocket
    :reader swank-connection-usocket
    :documentation "The usocket stream-usocket connected to the SWANK server.")
   (host
    :initarg :host
    :reader swank-connection-host
    :type string)
   (port
    :initarg :port
    :reader swank-connection-port
    :type integer)
   (next-id
    :initform 1
    :accessor swank-connection-next-id
    :type integer
    :documentation "Monotonically increasing continuation ID for :emacs-rex."))
  (:documentation
   "A TCP connection to a SWANK server. Created by SWANK-CONNECT,
    closed by SWANK-DISCONNECT."))

(defun swank-connect (host port)
  "Open a TCP connection to a SWANK server at HOST:PORT.
   Returns a SWANK-CONNECTION. Signals an error on failure."
  (let ((socket (usocket:socket-connect host port :element-type '(unsigned-byte 8))))
    (make-instance 'swank-connection
                   :usocket socket
                   :host host
                   :port port)))

(defun swank-disconnect (connection)
  "Close the TCP connection to the SWANK server."
  (let ((socket (swank-connection-usocket connection)))
    (when socket
      (ignore-errors (usocket:socket-close socket)))))

(defun swank-send (connection sexp)
  "Send SEXP to the SWANK server using the SWANK wire protocol:
   6-char hex length prefix + UTF-8 payload."
  (swank-send-raw connection
                  (let ((*package* (find-package :cl))
                        (*print-case* :downcase)
                        (*print-readably* t))
                    (prin1-to-string sexp))))

(defun swank-send-raw (connection string)
  "Send STRING as a raw SWANK wire protocol message:
   6-char hex length prefix + UTF-8 payload."
  (let* ((octets (flexi-streams:string-to-octets string :external-format :utf-8))
         (length (length octets))
         (header (flexi-streams:string-to-octets (format nil "~6,'0x" length)
                                          :external-format :utf-8))
         (stream (usocket:socket-stream (swank-connection-usocket connection))))
    (write-sequence header stream)
    (write-sequence octets stream)
    (finish-output stream)))

(defun swank-receive (connection)
  "Read one message from the SWANK server. Returns the s-expression.
   Blocks until a complete message is available."
  (let* ((stream (usocket:socket-stream (swank-connection-usocket connection)))
         (header-buf (make-array 6 :element-type '(unsigned-byte 8))))
    ;; Read 6-byte header
    (let ((n (read-sequence header-buf stream)))
      (when (< n 6)
        (error "SWANK connection closed: incomplete header (~D bytes)." n)))
    (let* ((header-string (flexi-streams:octets-to-string header-buf :external-format :utf-8))
           (length (parse-integer header-string :radix 16))
           (payload-buf (make-array length :element-type '(unsigned-byte 8))))
      ;; Read payload
      (let ((n (read-sequence payload-buf stream)))
        (when (< n length)
          (error "SWANK connection closed: incomplete payload (~D of ~D bytes)."
                 n length)))
      (let ((payload-string (flexi-streams:octets-to-string payload-buf :external-format :utf-8)))
        ;; SWANK messages use keywords for message types (:return, :write-string, etc.)
        ;; and SWANK-internal symbols in the payload. Read with CL-USER so unknown
        ;; SWANK symbols become CL-USER interned (harmless — we only inspect keywords).
        (let ((*package* (find-package :cl-user)))
          (read-from-string payload-string))))))

(defun %next-continuation-id (connection)
  "Return the next continuation ID and increment the counter."
  (prog1 (swank-connection-next-id connection)
    (incf (swank-connection-next-id connection))))

(defun swank-eval (connection form-string
                   &key (package "CL-USER") (timeout 30))
  "Evaluate FORM-STRING in the SWANK server. Returns
   (VALUES result-string output-string). Handles :write-string
   messages for output capture, :ping for keepalive, and :debug
   for debugger events (auto-abort). Signals error on timeout or
   evaluation abort."
  (declare (ignore timeout))
  (let ((id (%next-continuation-id connection))
        (output (make-string-output-stream)))
    ;; Send :emacs-rex with eval-and-grab-output (SWANK package not in parent)
    (swank-send-raw connection
                    (format nil "(:emacs-rex (swank:eval-and-grab-output ~S) ~S t ~D)"
                            form-string package id))
    ;; Read messages until we get :return with our ID
    (loop
      (let ((message (swank-receive connection)))
        (unless (consp message)
          (error "Unexpected SWANK message: ~S" message))
        (case (first message)
          (:return
           ;; (:return (:ok (OUTPUT RESULT)) ID)  or  (:return (:abort TEXT) ID)
           (let ((value (second message))
                 (ret-id (third message)))
             (when (eql ret-id id)
               (let ((extra-output (get-output-stream-string output)))
                 (cond
                   ((and (consp value) (eq (first value) :ok))
                    ;; value is (:ok ("captured-output" "result"))
                    (let ((payload (second value)))
                      (let ((captured (if (consp payload) (first payload) ""))
                            (result (if (consp payload) (second payload) (princ-to-string payload))))
                        (return (values result
                                        (concatenate 'string extra-output captured))))))
                   ((and (consp value) (eq (first value) :abort))
                    (error "Evaluation aborted: ~A" (second value)))
                   (t
                    (error "Unexpected :return value: ~S" value)))))))
          (:write-string
           ;; (:write-string STRING TARGET)
           (write-string (second message) output))
          (:ping
           ;; (:ping THREAD TAG) → respond with :emacs-pong
           (swank-send connection
                       `(:emacs-pong ,(second message) ,(third message))))
          (:debug
           ;; Auto-abort: invoke restart 0 (ABORT)
           ;; (:debug THREAD LEVEL CONDITION RESTARTS FRAMES CONTS)
           (let ((level (third message))
                 (condition-info (fourth message)))
             ;; Send abort restart
             (let ((abort-id (%next-continuation-id connection)))
               (swank-send-raw connection
                               (format nil "(:emacs-rex (swank:invoke-nth-restart-for-emacs ~D 0) ~S t ~D)"
                                       level package abort-id)))
             ;; Record condition for error reporting
             (let ((condition-text (if (consp condition-info)
                                       (format nil "~{~A~^ ~}" condition-info)
                                       (princ-to-string condition-info))))
               (write-string condition-text output))))
          (:debug-activate nil)
          (:debug-return nil)
          (:new-features nil)
          (:indentation-update nil)
          (otherwise nil))))))

;;;; End of file `swank-protocol.lisp'
