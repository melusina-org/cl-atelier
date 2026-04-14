;;;; child-connection.lisp — Concrete SWANK-based child connection

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

;;; child-connection is the concrete IMAGE-CONNECTION subclass that
;;; spawns a child SBCL, connects via SWANK over TCP, and provides
;;; eval, shutdown, and health-check methods. This file is part of
;;; org.melusina.atelier/mcp (not the kernel) because it depends on
;;; the SWANK wire protocol, usocket, flexi-streams, and
;;; bordeaux-threads.


;;;;
;;;; Condition
;;;;

(define-condition child-image-spawn-failed (error)
  ((reason
    :initarg :reason
    :reader child-image-spawn-failed-reason)
   (message
    :initarg :message
    :reader child-image-spawn-failed-message))
  (:report (lambda (condition stream)
             (format stream "Child image spawn failed: ~A"
                     (child-image-spawn-failed-message condition))))
  (:documentation "Signalled when make-instance of CHILD-CONNECTION fails."))


;;;;
;;;; Debug state
;;;;

(defclass debug-state ()
  ((condition-text
    :initarg :condition
    :reader debug-state-condition
    :documentation "The condition that was raised.")
   (restarts
    :initarg :restarts
    :reader debug-state-restarts
    :documentation "List of (index name description) alists.")
   (backtrace
    :initarg :backtrace
    :reader debug-state-backtrace
    :documentation "List of (index description) alists.")
   (level
    :initarg :level
    :reader debug-state-level
    :documentation "SWANK debug level (integer).")
   (thread
    :initarg :thread
    :reader debug-state-thread
    :documentation "SWANK thread ID."))
  (:documentation "Captures the debugger state when a child eval enters the debugger."))

(define-condition debugger-active (error)
  ()
  (:report "Cannot evaluate: debugger is active. Use abort-debug or select-restart first.")
  (:documentation "Signalled when eval-form is called while the debugger is active."))


;;;;
;;;; Child connection class
;;;;

(defclass child-connection (image-connection)
  ((port
    :initarg :port
    :reader child-connection-port
    :type integer
    :documentation "TCP port the child SWANK server listens on.")
   (swank-conn
    :initarg :swank-conn
    :accessor child-connection-swank-conn
    :type (or swank-connection null)
    :initform nil
    :documentation "The SWANK protocol connection to the child.")
   (stdout-drain-thread
    :initarg :stdout-drain-thread
    :initform nil
    :documentation "Background thread draining child stdout to prevent
     pipe deadlock. Started after the port is read.")
   (debug-state
    :accessor connection-debug-state
    :initform nil
    :type (or debug-state null)
    :documentation "The current debug state if the child is in the
     debugger, or NIL. Set by eval-form when the debugger is entered,
     cleared by invoke-restart or abort."))
  (:documentation
   "A connection to a child SBCL image via SWANK over TCP.
    Created by (MAKE-INSTANCE 'CHILD-CONNECTION), which spawns SBCL,
    loads the child-worker system, starts SWANK on a random port, and
    connects."))


;;;;
;;;; Spawn helpers
;;;;

(defvar *child-worker-system-path* nil
  "Pathname to the directory containing org.melusina.atelier.asd.
   Set lazily from this image's source registry.")

(defun %find-atelier-asd-directory ()
  "Find the directory containing org.melusina.atelier.asd."
  (or *child-worker-system-path*
      (let ((system (asdf:find-system "org.melusina.atelier" nil)))
        (when system
          (setf *child-worker-system-path*
                (asdf:system-source-directory system))))))

(defun %quicklisp-setup-path ()
  "Return the path to quicklisp/setup.lisp, or NIL."
  (let ((ql-home (symbol-value
                  (find-symbol "*QUICKLISP-HOME*"
                               (find-package :ql)))))
    (when ql-home
      (merge-pathnames "setup.lisp" ql-home))))

(defun %start-stdout-drain (stream)
  "Start a background thread that reads and discards lines from STREAM.
   Prevents pipe deadlock when the child writes to stdout after the
   parent has read the port number."
  (bordeaux-threads:make-thread
   (lambda ()
     (ignore-errors
       (loop :for line := (read-line stream nil nil)
             :while line)))
   :name "atelier/mcp stdout drain"))

(defun %connect-with-retry (host port &key (retries 10) (delay 0.5))
  "Try to connect to HOST:PORT, retrying on failure."
  (loop :for attempt :from 1 :to retries
        :do (handler-case
                (return (swank-connect host port))
              (error ()
                (when (= attempt retries)
                  (error "Failed to connect to ~A:~D after ~D attempts."
                         host port retries))
                (sleep delay)))))


;;;;
;;;; Initialization — spawn child, read port, connect SWANK
;;;;

(defmethod initialize-instance :after ((conn child-connection) &key (timeout 10))
  "Spawn a child SBCL, load the child-worker system, start SWANK,
   and connect via TCP."
  (let* ((asd-dir (%find-atelier-asd-directory))
         (ql-setup (%quicklisp-setup-path))
         (process-info
           (handler-case
               (uiop:launch-program
                (list "sbcl" "--non-interactive"
                      "--eval" (format nil "(load ~S)"
                                       (namestring ql-setup))
                      "--eval" (format nil "(push ~S asdf:*central-registry*)"
                                       (namestring asd-dir))
                      "--eval" "(asdf:load-system \"org.melusina.atelier/child-worker\")"
                      "--eval" "(atelier/child-worker:start-worker)")
                :input :stream
                :output :stream
                :error-output :output)
             (error (c)
               (error 'child-image-spawn-failed
                      :reason (format nil "Failed to spawn SBCL: ~A" c)
                      :message (format nil "Failed to spawn SBCL: ~A" c))))))
    (setf (slot-value conn 'process-info) process-info)
    ;; Read the port from child's stdout
    (let ((port-line nil)
          (child-stdout (uiop:process-info-output process-info)))
      (handler-case
          (sb-ext:with-timeout timeout
            (loop
              (let ((line (read-line child-stdout nil nil)))
                (unless line
                  (error 'child-image-spawn-failed
                         :reason "Child stdout closed before port was received."
                         :message "Child stdout closed before port was received."))
                (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline) line)))
                  (when (and (plusp (length trimmed))
                             (every #'digit-char-p trimmed))
                    (setf port-line trimmed)
                    (return))))))
        (sb-ext:timeout ()
          (uiop:terminate-process process-info)
          (uiop:wait-process process-info)
          (error 'child-image-spawn-failed
                 :reason "Timeout waiting for child SWANK port."
                 :message "Timeout waiting for child SWANK port.")))
      (let ((port (parse-integer (string-trim '(#\Space #\Tab #\Return #\Newline)
                                              port-line)
                                 :junk-allowed nil)))
        (unless port
          (uiop:terminate-process process-info)
          (uiop:wait-process process-info)
          (error 'child-image-spawn-failed
                 :reason (format nil "Invalid port from child: ~S" port-line)
                 :message (format nil "Invalid port from child: ~S" port-line)))
        (setf (slot-value conn 'port) port)
        ;; Drain child stdout in background
        (setf (slot-value conn 'stdout-drain-thread)
              (%start-stdout-drain child-stdout))
        ;; Connect to SWANK
        (let ((swank-conn (handler-case
                              (%connect-with-retry "127.0.0.1" port :retries 10 :delay 0.5)
                            (error (c)
                              (uiop:terminate-process process-info)
                              (uiop:wait-process process-info)
                              (error 'child-image-spawn-failed
                                     :reason (format nil "Failed to connect to child SWANK: ~A" c)
                                     :message (format nil "Failed to connect to child SWANK: ~A" c))))))
          (setf (child-connection-swank-conn conn) swank-conn))))))


;;;;
;;;; Generic function methods
;;;;

(defmethod connection-alive-p ((conn child-connection))
  "Return T if the child process is running and the SWANK socket is
   responsive. Probes SWANK with a lightweight eval of T (INV-42)."
  (and (child-connection-swank-conn conn)
       (call-next-method)
       (handler-case
           (progn
             (swank-eval (child-connection-swank-conn conn) "T" :timeout 1)
             t)
         (error () nil))))

(defmethod connection-eval ((conn child-connection) form)
  "Evaluate FORM (a string) in the child via the SWANK connection.
   Returns (VALUES result-string output-string). If the eval enters
   the debugger, auto-aborts and signals an error (preserving slice 010
   behavior for callers that use connection-eval directly).
   If the SWANK socket is broken (pipe error, connection reset), marks
   the connection as dead so ensure-child-connection will respawn."
  (handler-case
      (multiple-value-bind (status result output)
          (swank-eval (child-connection-swank-conn conn) form)
        (case status
          (:ok (values result output))
          (:debug
           ;; Auto-abort for connection-eval callers (backward compat)
           (let ((condition-text (debug-state-condition result)))
             (ignore-errors
               (swank-invoke-restart (child-connection-swank-conn conn)
                                     (debug-state-level result) 0
                                     :thread (debug-state-thread result)))
             (error "Evaluation aborted: ~A" condition-text)))
          (otherwise (error "Unexpected eval status: ~S" status))))
    (stream-error (c)
      ;; SWANK socket is dead (broken pipe, connection reset, etc.)
      ;; Mark connection as dead so ensure-child-connection respawns.
      (ignore-errors (swank-disconnect (child-connection-swank-conn conn)))
      (setf (child-connection-swank-conn conn) nil)
      (error "SWANK connection lost (child will respawn): ~A" c))))

(defmethod connection-shutdown ((conn child-connection))
  "Shut down the child: disconnect SWANK, terminate process, wait."
  (let ((swank-conn (child-connection-swank-conn conn)))
    (when swank-conn
      (ignore-errors
        (swank-send-raw swank-conn "(:emacs-rex (swank:quit-lisp) \"CL-USER\" t 0)"))
      (ignore-errors (swank-disconnect swank-conn))
      (setf (child-connection-swank-conn conn) nil)))
  ;; Fall back to process termination
  (call-next-method))

(defmethod connection-pid ((conn child-connection))
  "Return the OS PID of the child SBCL process."
  (call-next-method))


;;;;
;;;; Legacy factory function (for test compatibility)
;;;;

(defun make-child-connection (&key (timeout 10))
  "Spawn a child SBCL and return a CHILD-CONNECTION.
   Convenience wrapper for (make-instance 'child-connection :timeout TIMEOUT)."
  (make-instance 'child-connection :timeout timeout))

;;;; End of file `child-connection.lisp'
