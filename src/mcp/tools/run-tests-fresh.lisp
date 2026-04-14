;;;; run-tests-fresh.lisp — atelier:run-tests-fresh MCP tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

;;; run-tests-fresh spawns a separate SBCL subprocess (NOT the session
;;; child), loads a system and its testsuite, runs tests, and captures
;;; the output. Supports targeted execution of a single testcase.
;;;
;;; Slice 015: Fixed to call the actual test entry point instead of
;;; asdf:test-system (which had no perform method). Accepts optional
;;; testsuite-system and testcase-designator for targeted execution.

(define-tool run-tests-fresh (&key system-name testsuite-system testcase-designator)
  (:description
   "Run a system's test suite in a fresh SBCL subprocess. Spawns a
    new SBCL (not the session's child), loads the system and its
    testsuite, runs the tests, and returns the output. The subprocess
    is shut down after the call.

    SYSTEM-NAME is the primary system to load (e.g. \"org.melusina.atelier\").
    TESTSUITE-SYSTEM defaults to SYSTEM-NAME/testsuite if not provided.
    TESTCASE-DESIGNATOR is an optional fully-qualified symbol name
    (e.g. \"atelier/testsuite:validate-template-idempotent-write\")
    for targeted execution. When omitted, calls RUN-ALL-TESTS in the
    testsuite package.")
  (declare (type string system-name))
  (let* ((asd-dir (%find-atelier-asd-directory))
         (ql-setup (%quicklisp-setup-path))
         (ts-system (or testsuite-system
                        (concatenate 'string system-name "/testsuite")))
         (start (get-internal-real-time))
         (run-form
           (if (and testcase-designator (stringp testcase-designator)
                    (plusp (length testcase-designator)))
               ;; Targeted: resolve the symbol and call it
               (let* ((colon-pos (position #\: testcase-designator))
                      (pkg-name (if colon-pos
                                    (subseq testcase-designator 0 colon-pos)
                                    "CL-USER"))
                      (sym-name (if colon-pos
                                    (string-left-trim ":" (subseq testcase-designator colon-pos))
                                    testcase-designator)))
                 (format nil "(uiop:symbol-call ~S ~S)" pkg-name sym-name))
               ;; Default: call run-all-tests in the testsuite package
               (format nil "(let* ((ts-sys (asdf:find-system ~S nil))
                                   (pkg-name (when ts-sys
                                               (string-upcase
                                                (asdf:component-name ts-sys)))))
                              (if (and pkg-name (find-package pkg-name))
                                  (uiop:symbol-call pkg-name \"RUN-ALL-TESTS\")
                                  (asdf:test-system ~S)))"
                       ts-system system-name))))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program
         (list "sbcl" "--non-interactive"
               "--eval" (format nil "(load ~S)" (namestring ql-setup))
               "--eval" (format nil "(push ~S asdf:*central-registry*)"
                                (namestring asd-dir))
               "--eval" (format nil "(asdf:load-system ~S)" ts-system)
               "--eval" run-form)
         :output :string
         :error-output :string
         :ignore-error-status t)
      (let ((duration-ms (round (* 1000 (- (get-internal-real-time) start))
                                internal-time-units-per-second)))
        (list (cons "system" system-name)
              (cons "testsuite-system" ts-system)
              (cons "testcase" (or testcase-designator "run-all-tests"))
              (cons "exit-code" exit-code)
              (cons "success" (zerop exit-code))
              (cons "duration-ms" duration-ms)
              (cons "output" stdout)
              (cons "error-output" stderr))))))

;;;; End of file `run-tests-fresh.lisp'
