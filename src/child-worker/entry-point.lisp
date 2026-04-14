;;;; entry-point.lisp — Child worker SWANK startup

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/child-worker)

;;; The child worker is a SBCL subprocess spawned by the Atelier MCP
;;; server. It loads SWANK at runtime, starts a SWANK server on an
;;; OS-assigned TCP port, prints the port to stdout, and blocks forever.
;;; The parent connects via TCP and sends :emacs-rex messages.

(defun start-worker ()
  "Start the child worker: load SWANK, start server, print port, block.
   This function does not return. The parent terminates the child via
   uiop:terminate-process."
  ;; 1. Load SWANK
  (require :swank)
  ;; 2. Configure SWANK — suppress Emacs-specific features
  (setf (symbol-value (find-symbol "*CONFIGURE-EMACS-INDENTATION*" :swank)) nil)
  ;; 3. Start SWANK on OS-assigned port
  (let ((port (funcall (find-symbol "CREATE-SERVER" :swank)
                       :port 0
                       :dont-close t)))
    ;; 5. Print port for the parent to read
    (format t "~D~%" port)
    (finish-output)
    ;; 6. Block forever — SWANK runs in background threads
    (loop (sleep 3600))))

;;;; End of file `entry-point.lisp'
