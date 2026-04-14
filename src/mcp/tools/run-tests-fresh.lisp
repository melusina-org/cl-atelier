;;;; run-tests-fresh.lisp — atelier:run-tests-fresh MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

;;; run-tests-fresh spawns a separate SBCL subprocess (NOT the session
;;; child), loads a testsuite system, runs a testcase, and captures
;;; the output.
;;;
;;; Slice 015: Both testsuite-system and testcase-designator are
;;; mandatory. The caller decides what to load and what to run.

(define-tool run-tests-fresh (&key testsuite-system testcase-designator)
  (:description
   "Run a testcase in a fresh SBCL subprocess. Spawns a new SBCL
    (not the session's child), loads TESTSUITE-SYSTEM, resolves
    TESTCASE-DESIGNATOR as a fully-qualified symbol, calls it, and
    returns the output. The subprocess is shut down after the call.

    TESTSUITE-SYSTEM is the ASDF system to load
    (e.g. \"org.melusina.atelier/testsuite\").
    TESTCASE-DESIGNATOR is a fully-qualified symbol name
    (e.g. \"atelier/testsuite:run-all-tests\" or
    \"atelier/testsuite:validate-template-idempotent-write\").")
  (declare (type string testsuite-system testcase-designator))
  (let* ((asd-dir (%find-atelier-asd-directory))
         (ql-setup (%quicklisp-setup-path))
         (start (get-internal-real-time))
         (colon-pos (position #\: testcase-designator))
         (pkg-name (string-upcase
                    (if colon-pos
                        (subseq testcase-designator 0 colon-pos)
                        "CL-USER")))
         (sym-name (string-upcase
                    (if colon-pos
                        (string-left-trim ":" (subseq testcase-designator colon-pos))
                        testcase-designator)))
         (run-form (format nil "(uiop:symbol-call ~S ~S)" pkg-name sym-name)))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program
         (list "sbcl" "--non-interactive"
               "--eval" (format nil "(load ~S)" (namestring ql-setup))
               "--eval" (format nil "(push ~S asdf:*central-registry*)"
                                (namestring asd-dir))
               "--eval" (format nil "(asdf:load-system ~S)" testsuite-system)
               "--eval" run-form)
         :output :string
         :error-output :string
         :ignore-error-status t)
      (let ((duration-ms (round (* 1000 (- (get-internal-real-time) start))
                                internal-time-units-per-second)))
        (list (cons "testsuite-system" testsuite-system)
              (cons "testcase" testcase-designator)
              (cons "exit-code" exit-code)
              (cons "success" (zerop exit-code))
              (cons "duration-ms" duration-ms)
              (cons "output" stdout)
              (cons "error-output" stderr))))))

;;;; End of file `run-tests-fresh.lisp'
