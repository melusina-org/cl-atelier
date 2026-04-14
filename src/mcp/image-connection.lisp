;;;; image-connection.lisp — Abstract image-connection class (MCP kernel)

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp-kernel)

;;; The image-connection abstraction is the kernel extension point for
;;; spawning and talking to a child Lisp image. The kernel provides the
;;; abstract class and four generics. Concrete subclasses (e.g.
;;; child-connection in org.melusina.atelier/mcp) implement the
;;; transport-specific logic.

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
     NIL for an unspawned/abstract connection."))
  (:documentation
   "Abstract base class for a connection to a Lisp image.
    Generic function signatures on this class are stable: methods may
    be added; the existing CONNECTION-EVAL, CONNECTION-SHUTDOWN,
    CONNECTION-ALIVE-P, and CONNECTION-PID signatures must not be
    changed without a deliberate slice and risk review."))

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
    Concrete subclasses must implement this method.")
  (:method ((connection image-connection) form)
    (declare (ignore form))
    (error 'not-implemented
           :operation 'connection-eval
           :class (class-name (class-of connection))
           :message "connection-eval has no concrete implementation.")))

(defgeneric connection-pid (connection)
  (:documentation
   "Return the OS process ID of CONNECTION's image, or NIL.
    The kernel default returns the PID from UIOP process-info if
    available.")
  (:method ((connection image-connection))
    (let ((info (connection-process-info connection)))
      (when info
        #+sbcl (sb-ext:process-pid
                (slot-value info 'uiop/launch-program::process))
        #-sbcl nil))))

;;;; End of file `image-connection.lisp'
