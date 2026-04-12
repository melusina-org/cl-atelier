;;;; run-tests-fresh.lisp — atelier:run-tests-fresh MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

;;; run-tests-fresh spawns a separate SBCL subprocess (NOT the session
;;; child), loads a system, runs its test suite, and captures the output.
;;; This is the cold-start, stateless equivalent of the pre-commit pattern.

(define-tool run-tests-fresh (&key system-name)
  (:description
   "Run a system's test suite in a fresh SBCL subprocess. Spawns a
    new SBCL (not the session's child), loads the system, runs
    asdf:test-system, and returns the output. The subprocess is
    shut down after the call.")
  (declare (type string system-name))
  (let* ((asd-dir (%find-atelier-asd-directory))
         (ql-setup (%quicklisp-setup-path))
         (start (get-internal-real-time)))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program
         (list "sbcl" "--non-interactive"
               "--eval" (format nil "(load ~S)" (namestring ql-setup))
               "--eval" (format nil "(push ~S asdf:*central-registry*)"
                                (namestring asd-dir))
               "--eval" (format nil "(asdf:load-system ~S)" system-name)
               "--eval" (format nil "(asdf:test-system ~S)" system-name))
         :output :string
         :error-output :string
         :ignore-error-status t)
      (let ((duration-ms (round (* 1000 (- (get-internal-real-time) start))
                                internal-time-units-per-second)))
        (list (cons "system" system-name)
              (cons "exit-code" exit-code)
              (cons "success" (zerop exit-code))
              (cons "duration-ms" duration-ms)
              (cons "output" stdout)
              (cons "error-output" stderr))))))

;;;; End of file `run-tests-fresh.lisp'
