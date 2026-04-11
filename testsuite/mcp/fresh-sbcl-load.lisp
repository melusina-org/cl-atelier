;;;; fresh-sbcl-load.lisp — Fresh-SBCL subprocess load test (slow)

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

;;; Automates the INV-4 fresh-SBCL discipline into the test suite.
;;; Spawns a cold sbcl --non-interactive subprocess, asks it to
;;; (asdf:load-system "org.melusina.atelier/mcp"), and asserts
;;; that stderr contains no warnings from the MCP files themselves.
;;; Pre-existing warnings from the base org.melusina.atelier system
;;; are filtered out so this test is about the slice-009 code.

(defun %sbcl-runtime-pathname ()
  "Return the pathname of this process's SBCL runtime, or NIL."
  #+sbcl (probe-file sb-ext:*runtime-pathname*)
  #-sbcl nil)

(defun %subprocess-eval-forms ()
  "Return the list of --eval forms fed to the subprocess. Split
   across multiple forms because the reader cannot resolve
   asdf:*central-registry* until ASDF is loaded."
  (list "(require :asdf)"
        "(load #P\"/Users/michael/share/quicklisp/setup.lisp\")"
        "(pushnew #P\"/Users/michael/Melusina/atelier/\" (symbol-value (find-symbol \"*CENTRAL-REGISTRY*\" :asdf)))"
        "(funcall (find-symbol \"QUICKLOAD\" :ql) :com.inuoe.jzon :silent t)"
        "(funcall (find-symbol \"LOAD-SYSTEM\" :asdf) :org.melusina.atelier/mcp)"
        "(funcall (find-symbol \"QUIT\" :uiop) 0)"))

(defun %run-fresh-sbcl-load ()
  "Run a subprocess SBCL loading org.melusina.atelier/mcp. Returns
   (values stdout stderr exit-code) or NIL if SBCL is unavailable."
  (let ((sbcl (%sbcl-runtime-pathname)))
    (unless sbcl (return-from %run-fresh-sbcl-load nil))
    (let ((args (list (namestring sbcl)
                      "--non-interactive"
                      "--no-sysinit" "--no-userinit")))
      (dolist (form (%subprocess-eval-forms))
        (setf args (append args (list "--eval" form))))
      (multiple-value-bind (stdout stderr exit)
          (uiop:run-program args
                            :output :string
                            :error-output :string
                            :ignore-error-status t)
        (values stdout stderr exit)))))

(defun %mcp-warning-count (stderr)
  "Count lines in STDERR that announce caught warnings originating in
   src/mcp/ files. Pre-existing warnings from base atelier files are
   ignored to keep this test scoped to slice 009."
  (let ((lines (uiop:split-string stderr :separator '(#\Newline)))
        (count 0)
        (prev nil))
    (dolist (line lines count)
      (when (and prev
                 (or (search "caught WARNING" prev)
                     (search "caught STYLE-WARNING" prev))
                 (search "src/mcp/" line))
        (incf count))
      (setf prev line))))

(define-testcase validate-mcp-system-loads-cleanly ()
  "A fresh sbcl --non-interactive cold-load of org.melusina.atelier/mcp
   reports zero warnings originating in src/mcp/."
  (unless (%sbcl-runtime-pathname)
    (return-from validate-mcp-system-loads-cleanly))
  (multiple-value-bind (stdout stderr exit) (%run-fresh-sbcl-load)
    (declare (ignore stdout))
    (assert-eql 0 exit)
    (assert-eql 0 (%mcp-warning-count stderr))))

;;;; End of file `fresh-sbcl-load.lisp'
