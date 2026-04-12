;;;; image-connection.lisp — Abstract image-connection class

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

;;; The image-connection abstraction is the slice-010 extension point
;;; for spawning and talking to a child Lisp image. Slice 009 ships
;;; only the abstract class plus three generics with sensible default
;;; methods. Slice 010 will define a SWANK-CONNECTION subclass that
;;; populates the PROCESS-INFO slot from (uiop:launch-program ...).

(defclass image-connection ()
  ((id
    :initarg :id
    :reader connection-id
    :type (or string null)
    :initform nil
    :documentation "An identifier the MCP server uses to address this connection.")
   (process-info
    :initarg :process-info
    :reader connection-process-info
    :initform nil
    :documentation
    "A UIOP process-info struct for the spawned child Lisp image, or
     NIL for an unspawned/abstract connection. Slice 009 ships no
     concrete subclass; slice 010's SWANK-CONNECTION will populate
     this slot from (uiop:launch-program ...) over a socketpair."))
  (:documentation
   "Abstract base class for a connection to a Lisp image.
    Generic function signatures on this class are stable across
    slices: methods may be added; the existing CONNECTION-EVAL,
    CONNECTION-SHUTDOWN, and CONNECTION-ALIVE-P signatures must not
    be changed without a deliberate slice and risk review.
    See references/message-hierarchy.md and product/knowledge/invariants.md."))

(defgeneric connection-alive-p (connection)
  (:documentation
   "Return T if CONNECTION's image is currently running.")
  (:method ((connection image-connection))
    (let ((info (connection-process-info connection)))
      (and info (uiop:process-alive-p info)))))

(defgeneric connection-shutdown (connection)
  (:documentation
   "Gracefully shut down CONNECTION's image. Returns no useful value.")
  (:method ((connection image-connection))
    (let ((info (connection-process-info connection)))
      (when info
        (uiop:terminate-process info)
        (uiop:wait-process info)))))

(defgeneric connection-eval (connection form)
  (:documentation
   "Evaluate FORM in CONNECTION's image. Return the result as a string.
    Slice 009 ships no concrete subclass; this primary method signals
    NOT-IMPLEMENTED. Slice 010's SWANK-CONNECTION will provide the
    real implementation.")
  (:method ((connection image-connection) form)
    (declare (ignore form))
    (error 'not-implemented
           :operation 'connection-eval
           :class (class-name (class-of connection))
           :message "connection-eval has no concrete subclass in slice 009.")))


;;; ---- child-connection: concrete subclass using SWANK over TCP ----

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
     pipe deadlock. Started after the port is read."))
  (:documentation
   "A connection to a child SBCL image via SWANK over TCP.
    Created by MAKE-CHILD-CONNECTION, which spawns SBCL, loads the
    child-worker system, starts SWANK on a random port, and connects."))

(defvar *child-worker-system-path* nil
  "Pathname to the directory containing org.melusina.atelier.asd.
   Set lazily by MAKE-CHILD-CONNECTION from this image's source registry.")

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

(defun make-child-connection (&key (timeout 10))
  "Spawn a child SBCL, load the child-worker system, start SWANK,
   and connect via TCP. Returns a CHILD-CONNECTION.
   TIMEOUT is seconds to wait for the child to print its port.
   Signals CHILD-IMAGE-SPAWN-FAILED on failure."
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
    ;; Read the port from child's stdout. The child emits SBCL banner,
      ;; compilation messages, etc. before printing the port. We read
      ;; lines until we find one that's a pure decimal integer.
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
          ;; Drain child stdout in background to prevent pipe deadlock.
          ;; After reading the port, the child may continue writing
          ;; compilation messages to stdout (merged with stderr).
          (bordeaux-threads:make-thread
           (lambda ()
             (ignore-errors
               (loop :for line := (read-line child-stdout nil nil)
                     :while line)))
           :name "atelier/mcp child stdout drain")
          ;; Connect to SWANK
          (let ((swank-conn (handler-case
                                (%connect-with-retry "127.0.0.1" port :retries 10 :delay 0.5)
                              (error (c)
                                (uiop:terminate-process process-info)
                                (uiop:wait-process process-info)
                                (error 'child-image-spawn-failed
                                       :reason (format nil "Failed to connect to child SWANK: ~A" c)
                                       :message (format nil "Failed to connect to child SWANK: ~A" c))))))
            (make-instance 'child-connection
                           :process-info process-info
                           :port port
                           :swank-conn swank-conn))))))

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

(defmethod connection-eval ((conn child-connection) form)
  "Evaluate FORM (a string) in the child via the SWANK connection.
   Returns (VALUES result-string output-string)."
  (swank-eval (child-connection-swank-conn conn) form))

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

(defmethod connection-alive-p ((conn child-connection))
  "Return T if the child process is running."
  (and (child-connection-swank-conn conn)
       (call-next-method)))

;;;; End of file `image-connection.lisp'
