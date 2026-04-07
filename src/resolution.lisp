;;;; resolution.lisp — Resolution class hierarchy for Atelier

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)


;;;;
;;;; Resolution
;;;;

(defclass resolution ()
  ((maintainer
    :initarg :maintainer
    :reader resolution-maintainer
    :type symbol
    :documentation "The maintainer symbol that produced this resolution.")
   (finding
    :initarg :finding
    :reader resolution-finding
    :type finding
    :documentation "The finding this resolution addresses.")
   (kind
    :initarg :kind
    :reader resolution-kind
    :type (member :automatic :agent)
    :documentation "Whether this resolution is automatic or agent-assisted.")
   (description
    :initarg :description
    :reader resolution-description
    :type string
    :documentation "A human-readable description of the resolution."))
  (:documentation "Base class for all diagnostic resolutions."))


;;;;
;;;; Text Resolution
;;;;

(defclass text-resolution (resolution)
  ((replacement
    :initarg :replacement
    :reader resolution-replacement
    :type string
    :documentation "The replacement text. Location is derived from the linked finding."))
  (:documentation
   "A resolution that replaces text at the finding's location."))

(defun make-text-resolution (&rest initargs
                             &key maintainer finding kind description replacement)
  "Create and return a TEXT-RESOLUTION."
  (declare (ignore maintainer finding kind description replacement))
  (apply #'make-instance 'text-resolution initargs))

(defmethod print-object ((instance text-resolution) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~S ~S"
            (resolution-kind instance)
            (resolution-maintainer instance))))

(defmethod describe-object ((instance text-resolution) stream)
  (format stream "~&~A is a TEXT-RESOLUTION.~%" instance)
  (format stream "~&  Maintainer:   ~S~%" (resolution-maintainer instance))
  (format stream "~&  Kind:         ~S~%" (resolution-kind instance))
  (format stream "~&  Description:  ~A~%" (resolution-description instance))
  (format stream "~&  Replacement:  ~S~%" (resolution-replacement instance))
  (terpri stream))


;;;;
;;;; Syntax Resolution
;;;;

(defclass syntax-resolution (resolution)
  ((transform
    :initarg :transform
    :reader resolution-transform
    :type function
    :documentation "A function from CST node to new form. NIL means delete."))
  (:documentation "A resolution that transforms a CST node."))

(defun make-syntax-resolution (&rest initargs
                               &key maintainer finding kind description transform)
  "Create and return a SYNTAX-RESOLUTION."
  (declare (ignore maintainer finding kind description transform))
  (apply #'make-instance 'syntax-resolution initargs))

(defmethod print-object ((instance syntax-resolution) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~S ~S"
            (resolution-kind instance)
            (resolution-maintainer instance))))

(defmethod describe-object ((instance syntax-resolution) stream)
  (format stream "~&~A is a SYNTAX-RESOLUTION.~%" instance)
  (format stream "~&  Maintainer:   ~S~%" (resolution-maintainer instance))
  (format stream "~&  Kind:         ~S~%" (resolution-kind instance))
  (format stream "~&  Description:  ~A~%" (resolution-description instance))
  (format stream "~&  Transform:    ~S~%" (resolution-transform instance))
  (terpri stream))


;;;;
;;;; Agent Resolution
;;;;

(defclass agent-resolution (resolution)
  ((prompt
    :initarg :prompt
    :reader resolution-prompt
    :type string
    :documentation
    "A structured prompt for an LLM or coding agent to apply the fix."))
  (:documentation
   "A resolution carrying a prompt for an LLM-driven maintainer."))

(defun make-agent-resolution (&rest initargs
                              &key maintainer finding kind description prompt)
  "Create and return an AGENT-RESOLUTION."
  (declare (ignore maintainer finding kind description prompt))
  (apply #'make-instance 'agent-resolution initargs))

(defmethod print-object ((instance agent-resolution) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~S ~S"
            (resolution-kind instance)
            (resolution-maintainer instance))))

(defmethod describe-object ((instance agent-resolution) stream)
  (format stream "~&~A is an AGENT-RESOLUTION.~%" instance)
  (format stream "~&  Maintainer:   ~S~%" (resolution-maintainer instance))
  (format stream "~&  Kind:         ~S~%" (resolution-kind instance))
  (format stream "~&  Description:  ~A~%" (resolution-description instance))
  (format stream "~&  Prompt:       ~S~%" (resolution-prompt instance))
  (terpri stream))


;;;;
;;;; Composite Resolution
;;;;

(defclass composite-resolution (resolution)
  ((transforms
    :initarg :transforms
    :reader resolution-transforms
    :type list
    :documentation
    "An ordered list of SYNTAX-RESOLUTION instances.
Contract: innermost CST nodes first. The defining maintainer is
responsible for correct ordering."))
  (:documentation
   "A resolution composed of multiple syntax transforms applied in order."))

(defun make-composite-resolution (&rest initargs
                                  &key maintainer finding kind description transforms)
  "Create and return a COMPOSITE-RESOLUTION."
  (declare (ignore maintainer finding kind description transforms))
  (apply #'make-instance 'composite-resolution initargs))

(defmethod print-object ((instance composite-resolution) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~S ~S (~D transforms)"
            (resolution-kind instance)
            (resolution-maintainer instance)
            (length (resolution-transforms instance)))))

(defmethod describe-object ((instance composite-resolution) stream)
  (format stream "~&~A is a COMPOSITE-RESOLUTION.~%" instance)
  (format stream "~&  Maintainer:   ~S~%" (resolution-maintainer instance))
  (format stream "~&  Kind:         ~S~%" (resolution-kind instance))
  (format stream "~&  Description:  ~A~%" (resolution-description instance))
  (format stream "~&  Transforms:   ~D entries~%" (length (resolution-transforms instance)))
  (terpri stream))

;;;; End of file `resolution.lisp'
