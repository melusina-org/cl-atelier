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

;;;; End of file `image-connection.lisp'
