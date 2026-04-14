;;;; journey-tests.lisp — End-to-end journey tests for MCP tool workflows

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

;;; Journey tests exercise the MCP tools in realistic sequences,
;;; replicating the workflows an AI agent would perform. Each journey
;;; is a slow test that uses a shared child connection.
;;;
;;; These tests were discovered during slice 014 development when
;;; the MCP server was used as the primary development tool.


;;; ---- Journey 1: Load → xref → who-tests → run-impacted ----

(define-testcase validate-journey-xref-to-impacted ()
  "Full workflow: load system, find callers, discover testcases, run them."
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
      ;; Step 1: Load the system under test
      (let* ((ql-tool (atelier/mcp:find-tool-by-name "atelier:quickload"))
             (ql-result (atelier/mcp:handle-tool-call
                         ql-tool
                         (list (cons "system-name" "org.melusina.atelier/testsuite")))))
        (assert-t (cdr (assoc "success" ql-result :test #'string=))))
      ;; Step 2: Find callers of a function
      (let* ((wc-tool (atelier/mcp:find-tool-by-name "atelier:who-calls"))
             (wc-result (atelier/mcp:handle-tool-call
                         wc-tool
                         (list (cons "symbol-designator" "atelier:initialize")))))
        (assert-t* (listp wc-result))
        (assert-t* (not (null wc-result)))
        ;; Every result should have a name field
        (dolist (entry wc-result)
          (assert-t* (assoc "name" entry :test #'string=))))
      ;; Step 3: Find testcases that call parameter-replace
      (let* ((wt-tool (atelier/mcp:find-tool-by-name "atelier:who-tests"))
             (wt-result (atelier/mcp:handle-tool-call
                         wt-tool
                         (list (cons "symbol-designator"
                                     "atelier::parameter-replace")))))
        (assert-t* (listp wt-result))
        (assert-t* (not (null wt-result))))
      ;; Step 4: Run impacted tests
      (let* ((ri-tool (atelier/mcp:find-tool-by-name "atelier:run-impacted"))
             (ri-result (atelier/mcp:handle-tool-call
                         ri-tool
                         (list (cons "symbol-designator"
                                     "atelier::parameter-replace")))))
        (assert-t* (assoc "testcases" ri-result :test #'string=))
        (let ((total (cdr (assoc "total" ri-result :test #'string=))))
          (assert-t* (plusp total))))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-journey-xref-to-impacted (not SBCL)~%"))


;;; ---- Journey 2: Inspect class → who-specializes ----

(define-testcase validate-journey-class-introspection ()
  "Full workflow: inspect a class, then find methods specializing on it."
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
      ;; Step 1: Inspect the class
      (let* ((ic-tool (atelier/mcp:find-tool-by-name "atelier:inspect-class"))
             (ic-result (atelier/mcp:handle-tool-call
                         ic-tool
                         (list (cons "symbol-designator" "atelier:finding")))))
        ;; Has all structural fields
        (assert-t* (assoc "superclasses" ic-result :test #'string=))
        (assert-t* (assoc "subclasses" ic-result :test #'string=))
        (assert-t* (assoc "slots" ic-result :test #'string=))
        (assert-t* (assoc "methods" ic-result :test #'string=))
        ;; Slots have detailed info
        (let ((slots (cdr (assoc "slots" ic-result :test #'string=))))
          (assert-t* (not (null slots)))
          (let ((first-slot (first slots)))
            (assert-t* (assoc "readers" first-slot :test #'string=))
            (assert-t* (assoc "initargs" first-slot :test #'string=)))))
      ;; Step 2: who-specializes on same class
      (let* ((ws-tool (atelier/mcp:find-tool-by-name "atelier:who-specializes"))
             (ws-result (atelier/mcp:handle-tool-call
                         ws-tool
                         (list (cons "symbol-designator" "atelier:finding")))))
        (assert-t* (listp ws-result))
        (assert-t* (not (null ws-result)))
        ;; Methods have generic-function name
        (dolist (entry ws-result)
          (assert-t* (assoc "generic-function" entry :test #'string=))))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-journey-class-introspection (not SBCL)~%"))


;;; ---- Journey 3: Trace → eval → observe → untrace ----

(define-testcase validate-journey-trace-observe-untrace ()
  "Full workflow: trace a function, call it, observe trace output, untrace."
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
      ;; Step 1: Define a function
      (atelier/mcp:connection-eval *test-child*
                                   "(defun cl-user::journey-fn (x) (* x 5))")
      ;; Step 2: Trace it
      (let* ((tf-tool (atelier/mcp:find-tool-by-name "atelier:trace-function"))
             (tf-result (atelier/mcp:handle-tool-call
                         tf-tool
                         (list (cons "symbol-designator"
                                     "cl-user::journey-fn")))))
        (assert-t (cdr (assoc "traced" tf-result :test #'string=))))
      ;; Step 3: Eval and observe trace output.
      ;; Bind *trace-output* to *standard-output* at eval time so
      ;; SWANK captures it.
      (let* ((ev-tool (atelier/mcp:find-tool-by-name "atelier:eval-form"))
             (ev-result (atelier/mcp:handle-tool-call
                         ev-tool
                         (list (cons "form"
                                     "(cl-user::journey-fn 4)")))))
        (assert-string= "20" (cdr (assoc "value" ev-result :test #'string=)))
        ;; Trace output should mention the function
        (let ((stdout (cdr (assoc "stdout" ev-result :test #'string=))))
          (assert-t* (search "JOURNEY-FN" stdout))))
      ;; Step 4: Untrace
      (let* ((ut-tool (atelier/mcp:find-tool-by-name "atelier:untrace-function"))
             (ut-result (atelier/mcp:handle-tool-call
                         ut-tool
                         (list (cons "symbol-designator"
                                     "cl-user::journey-fn")))))
        (assert-nil (cdr (assoc "traced" ut-result :test #'string=))))
      ;; Step 5: Eval again — no trace output
      (let* ((ev-tool (atelier/mcp:find-tool-by-name "atelier:eval-form"))
             (ev-result (atelier/mcp:handle-tool-call
                         ev-tool
                         (list (cons "form"
                                     "(cl-user::journey-fn 4)")))))
        (assert-string= "20" (cdr (assoc "value" ev-result :test #'string=)))
        (let ((stdout (cdr (assoc "stdout" ev-result :test #'string=))))
          (assert-nil (search "JOURNEY-FN" stdout))))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-journey-trace-observe-untrace (not SBCL)~%"))


;;; ---- Combined runner ----

(define-testcase run-journey-tests ()
  "Run all journey tests with a shared child."
  (let ((*test-child* nil))
    (unwind-protect
         (progn
           (validate-journey-xref-to-impacted)
           (validate-journey-class-introspection)
           (validate-journey-trace-observe-untrace))
      (when *test-child*
        (ignore-errors (atelier/mcp:connection-shutdown *test-child*))
        (setf *test-child* nil)))))

;;;; End of file `journey-tests.lisp'
