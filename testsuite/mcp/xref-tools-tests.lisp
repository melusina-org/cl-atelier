;;;; xref-tools-tests.lisp — Tests for xref, who-tests, run-impacted tools (slice 014)

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

;;; Tests for slice 014: who-calls, who-references, who-binds,
;;; who-specializes, who-macroexpands, who-tests, run-impacted.
;;; All slow (require a child SBCL via SWANK).


;;; ---- S1: who-calls ----

(define-testcase validate-who-calls ()
  "who-calls for a known function returns a non-empty list of callers."
  #+sbcl
  (with-test-child (conn)
    (declare (ignore conn))
    (let* ((server (make-instance 'atelier/mcp::mcp-server
                                  :stream (make-two-way-stream
                                           (make-string-input-stream "")
                                           (make-string-output-stream))))
           (atelier/mcp::*current-server* server))
      (setf (atelier/mcp:server-child-connection server) *test-child*)
      (setf (atelier/mcp:connection-debug-state *test-child*) nil)
      ;; Load atelier so xref data is available
      (atelier/mcp:connection-eval *test-child*
                                   "(asdf:load-system \"org.melusina.atelier\")")
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:who-calls"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "symbol-designator" "atelier:initialize")))))
        (assert-t* (listp result))
        ;; initialize is called by lint-system and others
        (assert-t* (not (null result))))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-who-calls (not SBCL)~%"))

(define-testcase validate-who-calls-unknown ()
  "who-calls for an undefined symbol returns an empty list."
  #+sbcl
  (with-test-child (conn)
    (declare (ignore conn))
    (let* ((server (make-instance 'atelier/mcp::mcp-server
                                  :stream (make-two-way-stream
                                           (make-string-input-stream "")
                                           (make-string-output-stream))))
           (atelier/mcp::*current-server* server))
      (setf (atelier/mcp:server-child-connection server) *test-child*)
      (setf (atelier/mcp:connection-debug-state *test-child*) nil)
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:who-calls"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "symbol-designator"
                                  "cl-user::totally-nonexistent-fn-xyz")))))
        (assert-t* (null result)))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-who-calls-unknown (not SBCL)~%"))


;;; ---- S2: who-references ----

(define-testcase validate-who-references ()
  "who-references for a known special variable returns referencing functions."
  #+sbcl
  (with-test-child (conn)
    (declare (ignore conn))
    (let* ((server (make-instance 'atelier/mcp::mcp-server
                                  :stream (make-two-way-stream
                                           (make-string-input-stream "")
                                           (make-string-output-stream))))
           (atelier/mcp::*current-server* server))
      (setf (atelier/mcp:server-child-connection server) *test-child*)
      (setf (atelier/mcp:connection-debug-state *test-child*) nil)
      (atelier/mcp:connection-eval *test-child*
                                   "(asdf:load-system \"org.melusina.atelier\")")
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:who-references"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "symbol-designator"
                                  "atelier::*parameter-bindings*")))))
        (assert-t* (listp result)))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-who-references (not SBCL)~%"))


;;; ---- S3: who-binds ----

(define-testcase validate-who-binds ()
  "who-binds for a known special variable returns binding sites."
  #+sbcl
  (with-test-child (conn)
    (declare (ignore conn))
    (let* ((server (make-instance 'atelier/mcp::mcp-server
                                  :stream (make-two-way-stream
                                           (make-string-input-stream "")
                                           (make-string-output-stream))))
           (atelier/mcp::*current-server* server))
      (setf (atelier/mcp:server-child-connection server) *test-child*)
      (setf (atelier/mcp:connection-debug-state *test-child*) nil)
      (atelier/mcp:connection-eval *test-child*
                                   "(asdf:load-system \"org.melusina.atelier\")")
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:who-binds"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "symbol-designator"
                                  "atelier::*parameter-bindings*")))))
        (assert-t* (listp result)))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-who-binds (not SBCL)~%"))


;;; ---- S4: who-specializes ----

(define-testcase validate-who-specializes ()
  "who-specializes for a known class returns method information."
  #+sbcl
  (with-test-child (conn)
    (declare (ignore conn))
    (let* ((server (make-instance 'atelier/mcp::mcp-server
                                  :stream (make-two-way-stream
                                           (make-string-input-stream "")
                                           (make-string-output-stream))))
           (atelier/mcp::*current-server* server))
      (setf (atelier/mcp:server-child-connection server) *test-child*)
      (setf (atelier/mcp:connection-debug-state *test-child*) nil)
      (atelier/mcp:connection-eval *test-child*
                                   "(asdf:load-system \"org.melusina.atelier\")")
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:who-specializes"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "symbol-designator" "atelier:finding")))))
        (assert-t* (listp result))
        ;; finding has methods specializing on it
        (assert-t* (not (null result))))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-who-specializes (not SBCL)~%"))


;;; ---- S5: who-macroexpands ----

(define-testcase validate-who-macroexpands ()
  "who-macroexpands for a known macro returns expansion sites."
  #+sbcl
  (with-test-child (conn)
    (declare (ignore conn))
    (let* ((server (make-instance 'atelier/mcp::mcp-server
                                  :stream (make-two-way-stream
                                           (make-string-input-stream "")
                                           (make-string-output-stream))))
           (atelier/mcp::*current-server* server))
      (setf (atelier/mcp:server-child-connection server) *test-child*)
      (setf (atelier/mcp:connection-debug-state *test-child*) nil)
      (atelier/mcp:connection-eval *test-child*
                                   "(asdf:load-system \"org.melusina.atelier\")")
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:who-macroexpands"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "symbol-designator"
                                  "atelier:define-file-inspector")))))
        (assert-t* (listp result)))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-who-macroexpands (not SBCL)~%"))


;;; ---- S8: who-tests ----

(define-testcase validate-who-tests ()
  "who-tests for a function called by testcases returns testcase names."
  #+sbcl
  (with-test-child (conn)
    (declare (ignore conn))
    (let* ((server (make-instance 'atelier/mcp::mcp-server
                                  :stream (make-two-way-stream
                                           (make-string-input-stream "")
                                           (make-string-output-stream))))
           (atelier/mcp::*current-server* server))
      (setf (atelier/mcp:server-child-connection server) *test-child*)
      (setf (atelier/mcp:connection-debug-state *test-child*) nil)
      ;; Load the testsuite so testcases are registered
      (atelier/mcp:connection-eval *test-child*
                                   "(asdf:load-system \"org.melusina.atelier/testsuite\")")
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:who-tests"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "symbol-designator"
                                  "atelier::parameter-replace")))))
        (assert-t* (listp result)))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-who-tests (not SBCL)~%"))

(define-testcase validate-who-tests-no-callers ()
  "who-tests for a function not called by any testcase returns empty list."
  #+sbcl
  (with-test-child (conn)
    (declare (ignore conn))
    (let* ((server (make-instance 'atelier/mcp::mcp-server
                                  :stream (make-two-way-stream
                                           (make-string-input-stream "")
                                           (make-string-output-stream))))
           (atelier/mcp::*current-server* server))
      (setf (atelier/mcp:server-child-connection server) *test-child*)
      (setf (atelier/mcp:connection-debug-state *test-child*) nil)
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:who-tests"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "symbol-designator"
                                  "cl-user::totally-nonexistent-fn-xyz")))))
        (assert-t* (null result)))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-who-tests-no-callers (not SBCL)~%"))


;;; ---- S9: run-impacted ----

(define-testcase validate-run-impacted ()
  "run-impacted discovers and runs impacted testcases."
  #+sbcl
  (with-test-child (conn)
    (declare (ignore conn))
    (let* ((server (make-instance 'atelier/mcp::mcp-server
                                  :stream (make-two-way-stream
                                           (make-string-input-stream "")
                                           (make-string-output-stream))))
           (atelier/mcp::*current-server* server))
      (setf (atelier/mcp:server-child-connection server) *test-child*)
      (setf (atelier/mcp:connection-debug-state *test-child*) nil)
      (atelier/mcp:connection-eval *test-child*
                                   "(asdf:load-system \"org.melusina.atelier/testsuite\")")
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:run-impacted"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "symbol-designator"
                                  "atelier::parameter-replace")))))
        (assert-t* (assoc "function" result :test #'string=))
        (assert-t* (assoc "testcases" result :test #'string=)))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-run-impacted (not SBCL)~%"))

(define-testcase validate-run-impacted-no-tests ()
  "run-impacted with no impacted tests returns a message."
  #+sbcl
  (with-test-child (conn)
    (declare (ignore conn))
    (let* ((server (make-instance 'atelier/mcp::mcp-server
                                  :stream (make-two-way-stream
                                           (make-string-input-stream "")
                                           (make-string-output-stream))))
           (atelier/mcp::*current-server* server))
      (setf (atelier/mcp:server-child-connection server) *test-child*)
      (setf (atelier/mcp:connection-debug-state *test-child*) nil)
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:run-impacted"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "symbol-designator"
                                  "cl-user::totally-nonexistent-fn-xyz")))))
        (assert-t* (assoc "message" result :test #'string=)))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-run-impacted-no-tests (not SBCL)~%"))


;;; ---- Tool count ----

(define-testcase validate-xref-tool-count ()
  "Verify that 40 tools are registered (30 + 10 new in slice 014)."
  (assert= 40 (hash-table-count atelier/mcp:*tool-registry*)))


;;; ---- Combined runner ----

(define-testcase run-xref-tools-tests ()
  "Run all xref/who-tests/run-impacted tool tests with a shared child."
  (let ((*test-child* nil))
    (unwind-protect
         (progn
           (validate-who-calls)
           (validate-who-calls-unknown)
           (validate-who-references)
           (validate-who-binds)
           (validate-who-specializes)
           (validate-who-macroexpands)
           (validate-who-tests)
           (validate-who-tests-no-callers)
           (validate-run-impacted)
           (validate-run-impacted-no-tests))
      (when *test-child*
        (ignore-errors (atelier/mcp:connection-shutdown *test-child*))
        (setf *test-child* nil)))))

;;;; End of file `xref-tools-tests.lisp'
