;;;; entry-point.lisp — MCP server binary entry point

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

;;; This is the entry point for the MCP server binary (atelier_mcp).
;;; It calls the kernel's serve-two-way-stream with the Atelier-specific
;;; connection class and transcript.

(defun serve ()
  "Entry point for the MCP server binary. Configures the SWANK child
   connection transport, creates a session transcript, and starts the
   kernel server loop."
  (let ((transcript (handler-case (make-transcript)
                      (error () nil))))
    (serve-two-way-stream (make-two-way-stream *standard-input* *standard-output*)
                          :connection-class 'child-connection
                          :connection-initargs '(:timeout 10)
                          :transcript transcript)))

;;;; End of file `entry-point.lisp'
