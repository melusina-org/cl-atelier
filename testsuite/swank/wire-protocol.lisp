;;;; wire-protocol.lisp — Learn the SWANK wire protocol by testing it

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/swank)

;;; These tests run against a real SWANK server (in-process) to
;;; understand and document the exact message formats, the connection
;;; lifecycle, and the eval protocol. They use the atelier/mcp SWANK
;;; client to verify our implementation against the real server.

;;; ---- Helpers ----

(defvar *test-swank-port* nil
  "Port of the in-process SWANK server started for tests.")

(defun ensure-test-swank-server ()
  "Start an in-process SWANK server if one isn't running. Return port."
  (or *test-swank-port*
      (progn
        (require :swank)
        (setf (symbol-value (find-symbol "*CONFIGURE-EMACS-INDENTATION*" :swank)) nil)
        (setf *test-swank-port*
              (funcall (find-symbol "CREATE-SERVER" :swank)
                       :port 0
                       :dont-close t)))))

(defun with-test-swank-connection (fn)
  "Call FN with a SWANK-CONNECTION connected to the test server."
  (let* ((port (ensure-test-swank-server))
         (conn (atelier/mcp:swank-connect "127.0.0.1" port)))
    (unwind-protect (funcall fn conn)
      (atelier/mcp:swank-disconnect conn))))


;;; ---- Message framing ----

(define-testcase validate-swank-framing ()
  "Verify that our send/receive round-trips through a real SWANK server."
  (with-test-swank-connection
    (lambda (conn)
      ;; Send an eval and verify we get a response
      (atelier/mcp:swank-send-raw conn
        "(:emacs-rex (swank:eval-and-grab-output \"42\") \"CL-USER\" t 1)")
      ;; Read messages until :return
      (let ((return-msg nil))
        (loop :for msg := (atelier/mcp:swank-receive conn)
              :do (when (and (consp msg) (eq (first msg) :return))
                    (setf return-msg msg)
                    (return))
              :do (when (and (consp msg) (eq (first msg) :ping))
                    ;; Respond to pings
                    (atelier/mcp:swank-send conn
                      `(:emacs-pong ,(second msg) ,(third msg)))))
        ;; return-msg should be (:RETURN (:OK ("" "42")) 1)
        (assert-t* (consp return-msg))
        (assert-equal :return (first return-msg))
        ;; Second element is the value
        (let ((value (second return-msg)))
          (assert-t* (consp value))
          (assert-equal :ok (first value)))
        ;; Third element is the continuation ID
        (assert= 1 (third return-msg))))))


;;; ---- eval-and-grab-output ----

(define-testcase validate-swank-eval-simple ()
  "eval-and-grab-output on a simple form returns the result."
  (with-test-swank-connection
    (lambda (conn)
      (multiple-value-bind (status result output)
          (atelier/mcp:swank-eval conn "(+ 1 2)")
        (assert-eq :ok status)
        (assert-string= "3" result)
        ;; No stdout from (+ 1 2)
        (assert-string= "" output)))))

(define-testcase validate-swank-eval-with-output ()
  "eval-and-grab-output captures stdout from the evaluated form."
  (with-test-swank-connection
    (lambda (conn)
      (multiple-value-bind (status result output)
          (atelier/mcp:swank-eval conn "(progn (format t \"hello\") 42)")
        (assert-eq :ok status)
        (assert-string= "42" result)
        (assert-t* (search "hello" output))))))

(define-testcase validate-swank-eval-sequential-state ()
  "Sequential evals share the package state in the server."
  (with-test-swank-connection
    (lambda (conn)
      ;; Define a variable
      (atelier/mcp:swank-eval conn "(defvar *swank-test-var* 99)")
      ;; Read it back
      (multiple-value-bind (status result output)
          (atelier/mcp:swank-eval conn "*swank-test-var*")
        (declare (ignore output))
        (assert-eq :ok status)
        (assert-string= "99" result)))))

(define-testcase validate-swank-eval-error ()
  "eval-and-grab-output on an error form returns :debug status (slice 011)."
  (with-test-swank-connection
    (lambda (conn)
      ;; Should enter the debugger and return :debug status
      (multiple-value-bind (status debug-state output)
          (atelier/mcp:swank-eval conn "(error \"test error\")")
        (declare (ignore output))
        (assert-eq :debug status)
        (assert-t* (typep debug-state 'atelier/mcp:debug-state))
        (assert-t* (search "test error" (atelier/mcp:debug-state-condition debug-state)))
        ;; Clean up: abort
        (atelier/mcp:swank-invoke-restart conn
          (atelier/mcp:debug-state-level debug-state) 0
          :thread (atelier/mcp:debug-state-thread debug-state))))))

(define-testcase validate-swank-eval-multiple-values ()
  "eval-and-grab-output returns the printed representation of values."
  (with-test-swank-connection
    (lambda (conn)
      ;; SWANK's eval-and-grab-output returns the printed result
      (multiple-value-bind (status result output)
          (atelier/mcp:swank-eval conn "(values 1 2 3)")
        (declare (ignore output))
        (assert-eq :ok status)
        ;; Result format depends on SWANK — may be "1" or "(1 2 3)"
        ;; Just verify it's non-empty
        (assert-t* (plusp (length result)))))))


;;; ---- Connection lifecycle ----

(define-testcase validate-swank-connect-disconnect ()
  "Connect to SWANK, disconnect, verify clean state."
  (let* ((port (ensure-test-swank-server))
         (conn (atelier/mcp:swank-connect "127.0.0.1" port)))
    (assert-t* (typep conn 'atelier/mcp:swank-connection))
    (atelier/mcp:swank-disconnect conn)
    ;; After disconnect, can connect again
    (let ((conn2 (atelier/mcp:swank-connect "127.0.0.1" port)))
      (assert-t* (typep conn2 'atelier/mcp:swank-connection))
      (atelier/mcp:swank-disconnect conn2))))


;;; ---- Debug lifecycle (slice 011) ----

(define-testcase validate-swank-debug-abort-then-eval ()
  "After entering the debugger and aborting, a subsequent eval works."
  (with-test-swank-connection
    (lambda (conn)
      ;; Enter debugger
      (multiple-value-bind (status debug-state output)
          (atelier/mcp:swank-eval conn "(error \"test\")")
        (declare (ignore output))
        (assert-eq :debug status)
        (assert-t* (typep debug-state 'atelier/mcp:debug-state))
        (assert-t* (search "test" (atelier/mcp:debug-state-condition debug-state)))
        ;; Abort
        (multiple-value-bind (s2 r2 o2)
            (atelier/mcp:swank-invoke-restart conn
              (atelier/mcp:debug-state-level debug-state) 0
              :thread (atelier/mcp:debug-state-thread debug-state))
          (declare (ignore r2 o2))
          (assert-eq :aborted s2)))
      ;; Subsequent eval must work
      (multiple-value-bind (status result output)
          (atelier/mcp:swank-eval conn "(+ 1 2)")
        (declare (ignore output))
        (assert-eq :ok status)
        (assert-string= "3" result)))))

(define-testcase validate-swank-debug-repeated-abort ()
  "Multiple enter-debugger-abort cycles work on the same connection."
  (with-test-swank-connection
    (lambda (conn)
      ;; Cycle 1
      (multiple-value-bind (s r o) (atelier/mcp:swank-eval conn "(error \"c1\")")
        (declare (ignore o))
        (atelier/mcp:swank-invoke-restart conn
          (atelier/mcp:debug-state-level r) 0
          :thread (atelier/mcp:debug-state-thread r)))
      ;; Cycle 2
      (multiple-value-bind (s r o) (atelier/mcp:swank-eval conn "(error \"c2\")")
        (declare (ignore o))
        (atelier/mcp:swank-invoke-restart conn
          (atelier/mcp:debug-state-level r) 0
          :thread (atelier/mcp:debug-state-thread r)))
      ;; Final eval must work
      (multiple-value-bind (status result output)
          (atelier/mcp:swank-eval conn "(+ 2 3)")
        (declare (ignore output))
        (assert-eq :ok status)
        (assert-string= "5" result)))))

(define-testcase validate-swank-debug-backtrace ()
  "swank-backtrace-frames returns frame data during active debug session."
  (with-test-swank-connection
    (lambda (conn)
      (multiple-value-bind (s r o) (atelier/mcp:swank-eval conn "(error \"bt\")")
        (declare (ignore o))
        (let ((frames (atelier/mcp:swank-backtrace-frames
                       conn 0 10
                       :thread (atelier/mcp:debug-state-thread r))))
          (assert-t* (listp frames))
          (assert-t* (plusp (length frames)))
          (let ((frame (first frames)))
            (assert-t* (assoc "index" frame :test #'string=))
            (assert-t* (assoc "description" frame :test #'string=))))
        ;; Clean up
        (atelier/mcp:swank-invoke-restart conn
          (atelier/mcp:debug-state-level r) 0
          :thread (atelier/mcp:debug-state-thread r))))))

(define-testcase validate-swank-debug-interrupt ()
  "swank-interrupt causes a running eval to enter the debugger.
   Skipped in exploratory tests because interrupt requires a child
   process (in-process SWANK uses the same thread for eval and
   interrupt delivery). Tested via the MCP test suite instead."
  ;; This test is a placeholder — interrupt is tested via the
  ;; MCP-level validate-eval-form-timeout test which uses a real
  ;; child connection.
  t)


;;; ---- Entry point ----

(define-testcase run-swank-tests ()
  "Run all SWANK protocol behavior tests."
  (validate-swank-framing)
  (validate-swank-eval-simple)
  (validate-swank-eval-with-output)
  (validate-swank-eval-sequential-state)
  (validate-swank-eval-error)
  (validate-swank-eval-multiple-values)
  (validate-swank-connect-disconnect)
  ;; Slice 011: debug lifecycle
  (validate-swank-debug-abort-then-eval)
  (validate-swank-debug-repeated-abort)
  (validate-swank-debug-backtrace)
  (validate-swank-debug-interrupt))

;;;; End of file `wire-protocol.lisp'
