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


;;; ---- Debug state ----

(defclass debug-state ()
  ((condition-text
    :initarg :condition
    :reader debug-state-condition
    :type string
    :documentation "The condition text from the SWANK :debug message.")
   (restarts
    :initarg :restarts
    :reader debug-state-restarts
    :type list
    :documentation "List of alists, each with index, name, description.")
   (backtrace
    :initarg :backtrace
    :reader debug-state-backtrace
    :type list
    :documentation "List of alists, each with index and description.")
   (level
    :initarg :level
    :reader debug-state-level
    :type integer
    :documentation "The SWANK debug level.")
   (thread
    :initarg :thread
    :reader debug-state-thread
    :documentation "The SWANK thread ID."))
  (:documentation
   "Captures the debugger state when a SWANK eval enters the debugger.
    Used by the MCP tools to expose restarts and backtrace to the agent."))

(defun %parse-debug-message (message)
  "Parse a SWANK :debug message into a debug-state.
   Message format: (:debug THREAD LEVEL CONDITION RESTARTS FRAMES CONTS)"
  (let ((thread (second message))
        (level (third message))
        (condition-info (fourth message))
        (restarts-raw (fifth message))
        (frames-raw (sixth message)))
    (make-instance 'debug-state
                   :condition (if (consp condition-info)
                                  (format nil "~{~A~^ ~}" condition-info)
                                  (princ-to-string condition-info))
                   :restarts (loop :for (name desc) :in restarts-raw
                                   :for i :from 0
                                   :collect (list (cons "index" i)
                                                  (cons "name" (princ-to-string name))
                                                  (cons "description" (princ-to-string desc))))
                   :backtrace (loop :for frame :in frames-raw
                                    :collect (list (cons "index" (first frame))
                                                   (cons "description" (second frame))))
                   :level level
                   :thread thread)))


;;; ---- Core message loop ----

(defun %swank-receive-loop (connection id output)
  "Read SWANK messages until a conclusive result for ID.
   Returns (VALUES :ok result captured-output) on success,
   (VALUES :debug debug-state captured-output) on debugger entry,
   or signals an error.
   OUTPUT is a string-output-stream for :write-string accumulation."
  (loop
    (let ((message (swank-receive connection)))
      (unless (consp message)
        (error "Unexpected SWANK message: ~S" message))
      (case (first message)
        (:return
         (let ((value (second message))
               (ret-id (third message)))
           (when (eql ret-id id)
             (let ((extra-output (get-output-stream-string output)))
               (cond
                 ((and (consp value) (eq (first value) :ok))
                  (let ((payload (second value)))
                    (let ((captured (if (consp payload) (first payload) ""))
                          (result (if (consp payload) (second payload)
                                      (princ-to-string payload))))
                      (return (values :ok result
                                      (concatenate 'string extra-output captured))))))
                 ((and (consp value) (eq (first value) :abort))
                  (error "Evaluation aborted: ~A" (second value)))
                 (t
                  (error "Unexpected :return value: ~S" value)))))))
        (:write-string
         (write-string (second message) output))
        (:ping
         (swank-send connection
                     `(:emacs-pong ,(second message) ,(third message))))
        (:debug
         ;; Debugger entered — parse the debug state and return it
         (let ((debug-state (%parse-debug-message message)))
           ;; Wait for :debug-activate before returning
           (loop :for activate-msg := (swank-receive connection)
                 :do (when (and (consp activate-msg)
                                (eq (first activate-msg) :debug-activate))
                       (return))
                 :do (when (and (consp activate-msg)
                                (eq (first activate-msg) :write-string))
                       (write-string (second activate-msg) output))
                 :do (when (and (consp activate-msg)
                                (eq (first activate-msg) :ping))
                       (swank-send connection
                                   `(:emacs-pong ,(second activate-msg)
                                                 ,(third activate-msg)))))
           (return (values :debug debug-state
                           (get-output-stream-string output)))))
        (:debug-return nil)
        (:new-features nil)
        (:indentation-update nil)
        (otherwise nil)))))


;;; ---- Eval ----

(defun swank-eval (connection form-string
                   &key (package "CL-USER") (timeout nil))
  "Evaluate FORM-STRING in the SWANK server.
   Returns (VALUES result-string output-string) on success.
   Returns (VALUES :debug debug-state output-string) when the debugger
   is entered — the caller decides whether to auto-abort or expose
   the debug state.
   When TIMEOUT is non-nil (seconds), a watchdog thread sends
   :emacs-interrupt after TIMEOUT seconds."
  (let ((id (%next-continuation-id connection))
        (output (make-string-output-stream))
        (watchdog nil))
    ;; Send :emacs-rex with eval-and-grab-output
    (swank-send-raw connection
                    (format nil "(:emacs-rex (swank:eval-and-grab-output ~S) ~S t ~D)"
                            form-string package id))
    ;; Start watchdog thread for timeout
    (when (and timeout (plusp timeout))
      (setf watchdog
            (bordeaux-threads:make-thread
             (lambda ()
               (sleep timeout)
               (ignore-errors (swank-interrupt connection)))
             :name "atelier/mcp eval-form watchdog")))
    (unwind-protect
         (%swank-receive-loop connection id output)
      ;; Cancel watchdog if eval completed before timeout
      (when (and watchdog (bordeaux-threads:thread-alive-p watchdog))
        (ignore-errors (bordeaux-threads:destroy-thread watchdog))))))


;;; ---- Invoke restart ----

(defun swank-invoke-restart (connection level index
                             &key (package "CL-USER") (thread t))
  "Send invoke-nth-restart-for-emacs to the SWANK server and process
   the result. THREAD should be the debug thread from the debug-state.
   Returns (VALUES :ok result output) if the restart resolves the eval,
   (VALUES :debug debug-state output) if a new debugger level is entered,
   or (VALUES :aborted condition-text \"\") if the restart aborts."
  (let ((id (%next-continuation-id connection))
        (output (make-string-output-stream)))
    (swank-send-raw connection
                    (format nil "(:emacs-rex (swank:invoke-nth-restart-for-emacs ~D ~D) ~S ~S ~D)"
                            level index package thread id))
    ;; Read messages until conclusive.
    (loop
      (let ((message (swank-receive connection)))
        (unless (consp message)
          (error "Unexpected SWANK message: ~S" message))
        (case (first message)
          (:return
           (let ((ret-id (third message)))
             (when (eql ret-id id)
               ;; After abort, SWANK may re-enter the debugger.
               ;; Drain :debug-return, :debug, :debug-activate messages,
               ;; and auto-abort any re-entered debugger level.
               (%drain-post-abort-messages connection thread package)
               (return (values :aborted
                               (get-output-stream-string output)
                               "")))))
          (:write-string
           (write-string (second message) output))
          (:ping
           (swank-send connection
                       `(:emacs-pong ,(second message) ,(third message))))
          (:debug
           ;; New debugger level entered (higher than current)
           (let ((debug-state (%parse-debug-message message)))
             (loop :for activate-msg := (swank-receive connection)
                   :do (when (and (consp activate-msg)
                                  (eq (first activate-msg) :debug-activate))
                         (return))
                   :do (when (and (consp activate-msg)
                                  (eq (first activate-msg) :write-string))
                         (write-string (second activate-msg) output))
                   :do (when (and (consp activate-msg)
                                  (eq (first activate-msg) :ping))
                         (swank-send connection
                                     `(:emacs-pong ,(second activate-msg)
                                                   ,(third activate-msg)))))
             (return (values :debug debug-state
                             (get-output-stream-string output)))))
          (:debug-return nil)
          (:new-features nil)
          (:indentation-update nil)
          (otherwise nil))))))

(defun %drain-post-abort-messages (connection thread package)
  "After an abort, SWANK may send :debug-return then re-enter the debugger.
   Drain those messages, and auto-abort any re-entered debugger level
   using throw-to-toplevel."
  (let ((socket (swank-connection-usocket connection)))
    ;; Check if there are pending messages with a short timeout
    (loop :repeat 5
          :do (handler-case
                  (sb-ext:with-timeout 0.5
                    (let ((msg (swank-receive connection)))
                      (when (consp msg)
                        (case (first msg)
                          (:debug-return nil)   ; expected, consume
                          (:debug-activate
                           ;; Re-entered debugger — send throw-to-toplevel
                           (let ((ttl-id (%next-continuation-id connection)))
                             (swank-send-raw connection
                               (format nil "(:emacs-rex (swank:throw-to-toplevel) ~S ~S ~D)"
                                       package thread ttl-id))
                             ;; Drain the response
                             (loop :repeat 10
                                   :do (handler-case
                                           (sb-ext:with-timeout 1
                                             (let ((rmsg (swank-receive connection)))
                                               (when (and (consp rmsg)
                                                          (eq (first rmsg) :return)
                                                          (eql (third rmsg) ttl-id))
                                                 (return))))
                                         (sb-ext:timeout () (return))))))
                          (:debug nil)          ; consume, wait for :debug-activate
                          (:write-string nil)
                          (:ping
                           (swank-send connection
                                       `(:emacs-pong ,(second msg) ,(third msg))))
                          (otherwise nil)))))
                (sb-ext:timeout () (return))))))


;;; ---- Backtrace ----

(defun swank-backtrace-frames (connection start end
                               &key (package "CL-USER") (thread t))
  "Request backtrace frames START..END from the SWANK server.
   THREAD should be the debug thread from the debug-state.
   Returns a list of alists with index and description."
  (let ((id (%next-continuation-id connection)))
    (swank-send-raw connection
                    (format nil "(:emacs-rex (swank:backtrace ~D ~D) ~S ~S ~D)"
                            start end package thread id))
    (loop
      (let ((message (swank-receive connection)))
        (unless (consp message)
          (error "Unexpected SWANK message: ~S" message))
        (case (first message)
          (:return
           (let ((value (second message))
                 (ret-id (third message)))
             (when (eql ret-id id)
               (when (and (consp value) (eq (first value) :ok))
                 (return (loop :for frame :in (second value)
                               :collect (list (cons "index" (first frame))
                                              (cons "description"
                                                    (if (consp (cdr frame))
                                                        (second frame)
                                                        (princ-to-string (cdr frame)))))))))))
          (:ping
           (swank-send connection
                       `(:emacs-pong ,(second message) ,(third message))))
          (:write-string nil)
          (otherwise nil))))))


;;; ---- Eval in frame ----

(defun swank-eval-in-frame (connection expression frame-index
                            &key (package "CL-USER") (thread t))
  "Evaluate EXPRESSION in the context of FRAME-INDEX in the debugger.
   THREAD should be the debug thread from the debug-state.
   Returns the result as a string."
  (let ((id (%next-continuation-id connection)))
    (swank-send-raw connection
                    (format nil "(:emacs-rex (swank:eval-string-in-frame ~S ~D ~S) ~S ~S ~D)"
                            expression frame-index package package thread id))
    (loop
      (let ((message (swank-receive connection)))
        (unless (consp message)
          (error "Unexpected SWANK message: ~S" message))
        (case (first message)
          (:return
           (let ((value (second message))
                 (ret-id (third message)))
             (when (eql ret-id id)
               (cond
                 ((and (consp value) (eq (first value) :ok))
                  (return (second value)))
                 ((and (consp value) (eq (first value) :abort))
                  (error "eval-in-frame aborted: ~A" (second value)))
                 (t (error "Unexpected eval-in-frame result: ~S" value))))))
          (:ping
           (swank-send connection
                       `(:emacs-pong ,(second message) ,(third message))))
          (:debug
           ;; eval-in-frame itself entered a new debugger level. Auto-abort
           ;; to get back to the original debug level.
           (let ((inner-condition (if (consp (fourth message))
                                      (format nil "~{~A~^ ~}" (fourth message))
                                      (princ-to-string (fourth message)))))
             ;; Wait for :debug-activate
             (loop :for msg := (swank-receive connection)
                   :do (when (and (consp msg) (eq (first msg) :debug-activate))
                         (let ((level (third msg))
                               (abort-id (%next-continuation-id connection)))
                           (swank-send-raw connection
                                           (format nil "(:emacs-rex (swank:invoke-nth-restart-for-emacs ~D 0) ~S ~S ~D)"
                                                   level package thread abort-id))
                           ;; Wait for the abort's :return — the original eval-in-frame
                           ;; (our id) never gets a :return (INV-24)
                           (loop :for rmsg := (swank-receive connection)
                                 :do (cond
                                       ((and (consp rmsg)
                                             (eq (first rmsg) :return)
                                             (eql (third rmsg) abort-id))
                                        (return))
                                       ((and (consp rmsg)
                                             (eq (first rmsg) :debug-return))
                                        nil)
                                       (t nil))))
                         (return))
                   :do (when (and (consp msg) (eq (first msg) :write-string))
                         nil))
             (error "eval-in-frame error: ~A" inner-condition)))
          (:write-string nil)
          (otherwise nil))))))


;;; ---- Interrupt ----

(defun swank-interrupt (connection)
  "Send :emacs-interrupt to the SWANK server to interrupt a running eval.
   This causes the child to enter the debugger with a keyboard-interrupt."
  (swank-send-raw connection "(:emacs-interrupt :repl-thread)"))

;;;; End of file `swank-protocol.lisp'
