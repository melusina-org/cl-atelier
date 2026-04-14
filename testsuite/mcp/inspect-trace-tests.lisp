;;;; inspect-trace-tests.lisp — Tests for CLOS inspector and trace tools (slice 014)

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

;;; Tests for slice 014: inspect-class, trace-function, untrace-function.
;;; All slow (require a child SBCL via SWANK).


;;; ---- S6: inspect-class ----

(define-testcase validate-inspect-class ()
  "inspect-class for a known class returns detailed information."
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
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:inspect-class"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "symbol-designator" "atelier:finding")))))
        ;; Should have all expected fields
        (assert-t* (assoc "name" result :test #'string=))
        (assert-t* (assoc "superclasses" result :test #'string=))
        (assert-t* (assoc "subclasses" result :test #'string=))
        (assert-t* (assoc "slots" result :test #'string=))
        (assert-t* (assoc "methods" result :test #'string=))
        ;; finding should have subclasses
        (assert-t* (not (null (cdr (assoc "subclasses" result
                                          :test #'string=))))))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-inspect-class (not SBCL)~%"))

(define-testcase validate-inspect-class-non-class ()
  "inspect-class for a non-class symbol returns an error."
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
      (let ((tool (atelier/mcp:find-tool-by-name "atelier:inspect-class")))
        (assert-condition
         (atelier/mcp:handle-tool-call
          tool
          (list (cons "symbol-designator" "cl:car")))
         error))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-inspect-class-non-class (not SBCL)~%"))


;;; ---- S7: trace-function / untrace-function ----

(define-testcase validate-trace-function ()
  "trace-function enables tracing and returns confirmation."
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
      ;; Define a simple function to trace
      (atelier/mcp:connection-eval *test-child*
                                   "(defun cl-user::test-trace-fn (x) (* x 2))")
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:trace-function"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "symbol-designator"
                                  "cl-user::test-trace-fn")))))
        (assert-t (cdr (assoc "traced" result :test #'string=))))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-trace-function (not SBCL)~%"))

(define-testcase validate-untrace-function ()
  "untrace-function removes tracing."
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
      ;; Ensure function exists and is traced
      (atelier/mcp:connection-eval *test-child*
                                   "(defun cl-user::test-trace-fn (x) (* x 2))")
      (atelier/mcp:connection-eval *test-child*
                                   "(atelier/child-worker:trace-function-data \"cl-user::test-trace-fn\")")
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:untrace-function"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "symbol-designator"
                                  "cl-user::test-trace-fn")))))
        (assert-nil (cdr (assoc "traced" result :test #'string=))))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-untrace-function (not SBCL)~%"))

(define-testcase validate-trace-output-in-eval ()
  "Traced function produces trace output visible in eval-form stdout."
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
      ;; Define, trace, and call
      (atelier/mcp:connection-eval *test-child*
                                   "(defun cl-user::test-trace-fn2 (x) (* x 3))")
      (atelier/mcp:connection-eval *test-child*
                                   "(atelier/child-worker:trace-function-data \"cl-user::test-trace-fn2\")")
      ;; Now eval the function via the eval-form tool.
      ;; Bind *trace-output* to *standard-output* at eval time so
      ;; SWANK's eval-and-grab-output captures the trace output.
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:eval-form"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "form"
                                  "(cl-user::test-trace-fn2 7)")))))
        ;; The value should be 21
        (assert-string= "21" (cdr (assoc "value" result :test #'string=)))
        ;; Stdout should contain trace output mentioning the function
        (let ((stdout (cdr (assoc "stdout" result :test #'string=))))
          (assert-t* (search "TEST-TRACE-FN2" stdout))))
      ;; Clean up
      (atelier/mcp:connection-eval *test-child*
                                   "(atelier/child-worker:untrace-function-data \"cl-user::test-trace-fn2\")")))
  #-sbcl
  (format t "~&;; SKIPPED: validate-trace-output-in-eval (not SBCL)~%"))


;;; ---- Combined runner ----

(define-testcase run-inspect-trace-tests ()
  "Run all inspect-class and trace tool tests.
   Expects *test-child* to be managed by the caller (run-mcp-tests).
   Cleans up trace state to avoid polluting subsequent groups."
  (unwind-protect
       (progn
         (validate-inspect-class)
         (validate-inspect-class-non-class)
         (validate-trace-function)
         (validate-untrace-function)
         (validate-trace-output-in-eval))
    ;; Clean up: untrace everything in case a test left traces active
    (when *test-child*
      (ignore-errors
        (atelier/mcp:connection-eval *test-child* "(untrace)")))))

;;;; End of file `inspect-trace-tests.lisp'
