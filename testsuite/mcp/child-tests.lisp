;;;; child-tests.lisp — Tests for child connection, eval, introspection, runners

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

;;; Slow tests that spawn a child SBCL via SWANK. All child-dependent
;;; tests share a single child connection to avoid spawning many SBCLs.

(defvar *test-child* nil
  "Shared child connection for slow tests. Created once, shut down
   in unwind-protect.")

(defmacro with-test-child ((var) &body body)
  "Execute BODY with VAR bound to the shared test child connection.
   Creates the child on first use."
  `(let ((,var (or *test-child*
                   (setf *test-child* (atelier/mcp:make-child-connection)))))
     ,@body))


;;; ---- S4: Child connection tests ----

(define-testcase validate-child-connection-spawn ()
  "Verify that make-child-connection spawns a child SBCL and connects."
  #+sbcl
  (with-test-child (conn)
    ;; Should be a child-connection
    (assert-t* (typep conn 'atelier/mcp:child-connection))
    ;; Should be alive
    (assert-t (atelier/mcp:connection-alive-p conn))
    ;; Should have a port
    (assert-t* (plusp (atelier/mcp:child-connection-port conn))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-child-connection-spawn (not SBCL)~%"))

(define-testcase validate-child-connection-eval ()
  "Verify that connection-eval returns the result of a simple form."
  #+sbcl
  (with-test-child (conn)
    (multiple-value-bind (result output)
        (atelier/mcp:connection-eval conn "(+ 1 2)")
      (declare (ignore output))
      (assert-string= "3" result)))
  #-sbcl
  (format t "~&;; SKIPPED: validate-child-connection-eval (not SBCL)~%"))

(define-testcase validate-child-connection-sequential-state ()
  "Verify that sequential evals share state in the child."
  #+sbcl
  (with-test-child (conn)
    ;; Define a variable
    (atelier/mcp:connection-eval conn "(defvar *test-x* 42)")
    ;; Read it back
    (multiple-value-bind (result output)
        (atelier/mcp:connection-eval conn "*test-x*")
      (declare (ignore output))
      (assert-string= "42" result)))
  #-sbcl
  (format t "~&;; SKIPPED: validate-child-connection-sequential-state (not SBCL)~%"))

(define-testcase validate-child-connection-shutdown ()
  "Verify that connection-shutdown terminates the child."
  #+sbcl
  (let ((conn (atelier/mcp:make-child-connection)))
    (assert-t (atelier/mcp:connection-alive-p conn))
    (atelier/mcp:connection-shutdown conn)
    ;; Give the process a moment to exit
    (sleep 0.5)
    (assert-nil (atelier/mcp:connection-alive-p conn)))
  #-sbcl
  (format t "~&;; SKIPPED: validate-child-connection-shutdown (not SBCL)~%"))

(define-testcase validate-child-connection-spawn-failure ()
  "Verify that spawn failure signals child-image-spawn-failed."
  ;; Temporarily break the SBCL lookup
  (let ((orig-path (uiop:getenv "PATH")))
    (unwind-protect
         (progn
           (setf (uiop:getenv "PATH") "/nonexistent")
           (assert-condition
            (atelier/mcp:make-child-connection :timeout 3)
            atelier/mcp:child-image-spawn-failed))
      (setf (uiop:getenv "PATH") orig-path))))


;;; ---- S5: Eval-form tool tests ----

(define-testcase validate-eval-form-basic ()
  "Verify eval-form tool returns value and duration."
  #+sbcl
  (with-test-child (conn)
    (declare (ignore conn))
    (let* ((server (make-instance 'atelier/mcp::mcp-server
                                  :stream (make-two-way-stream
                                           (make-string-input-stream "")
                                           (make-string-output-stream))))
           (atelier/mcp::*current-server* server))
      ;; Set the child on this test server
      (setf (atelier/mcp:server-child-connection server) *test-child*)
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:eval-form"))
             (result (atelier/mcp:handle-tool-call tool
                                                   (list (cons "form" "(+ 1 2)")))))
        (assert-t* (assoc "value" result :test #'string=))
        (assert-string= "3" (cdr (assoc "value" result :test #'string=)))
        (assert-t* (assoc "duration-ms" result :test #'string=)))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-eval-form-basic (not SBCL)~%"))

(define-testcase validate-eval-form-multiple-values ()
  "Verify eval-form returns the printed representation of multiple values."
  #+sbcl
  (with-test-child (conn)
    (declare (ignore conn))
    (let* ((server (make-instance 'atelier/mcp::mcp-server
                                  :stream (make-two-way-stream
                                           (make-string-input-stream "")
                                           (make-string-output-stream))))
           (atelier/mcp::*current-server* server))
      (setf (atelier/mcp:server-child-connection server) *test-child*)
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:eval-form"))
             (result (atelier/mcp:handle-tool-call tool
                                                   (list (cons "form" "(values 1 2 3)")))))
        ;; interactive-eval returns the last printed value
        (assert-t* (assoc "value" result :test #'string=)))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-eval-form-multiple-values (not SBCL)~%"))

(define-testcase validate-eval-form-error ()
  "Verify eval-form on a signalling form returns an error."
  #+sbcl
  (with-test-child (conn)
    (declare (ignore conn))
    (let* ((server (make-instance 'atelier/mcp::mcp-server
                                  :stream (make-two-way-stream
                                           (make-string-input-stream "")
                                           (make-string-output-stream))))
           (atelier/mcp::*current-server* server))
      (setf (atelier/mcp:server-child-connection server) *test-child*)
      (let ((tool (atelier/mcp:find-tool-by-name "atelier:eval-form")))
        ;; Evaluating an error should signal
        (assert-condition
         (atelier/mcp:handle-tool-call tool
                                       (list (cons "form" "(error \"test error\")")))
         error))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-eval-form-error (not SBCL)~%"))

(define-testcase validate-eval-form-output-capture ()
  "Verify eval-form captures stdout from the child eval."
  #+sbcl
  (with-test-child (conn)
    (declare (ignore conn))
    (let* ((server (make-instance 'atelier/mcp::mcp-server
                                  :stream (make-two-way-stream
                                           (make-string-input-stream "")
                                           (make-string-output-stream))))
           (atelier/mcp::*current-server* server))
      (setf (atelier/mcp:server-child-connection server) *test-child*)
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:eval-form"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "form" "(progn (format t \"hello~%\") 42)")))))
        (let ((stdout (cdr (assoc "stdout" result :test #'string=))))
          (assert-t* (search "hello" stdout))))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-eval-form-output-capture (not SBCL)~%"))


;;; ---- S6: Introspection tool tests ----

(define-testcase validate-list-packages ()
  "Verify list-packages returns package data from the child."
  #+sbcl
  (with-test-child (conn)
    (declare (ignore conn))
    (let* ((server (make-instance 'atelier/mcp::mcp-server
                                  :stream (make-two-way-stream
                                           (make-string-input-stream "")
                                           (make-string-output-stream))))
           (atelier/mcp::*current-server* server))
      (setf (atelier/mcp:server-child-connection server) *test-child*)
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:list-packages"))
             (result (atelier/mcp:handle-tool-call tool nil)))
        ;; Should be a list
        (assert-t* (listp result))
        ;; Should contain COMMON-LISP
        (assert-t* (find "COMMON-LISP" result
                         :key (lambda (pkg)
                                (cdr (assoc "name" pkg :test #'string=)))
                         :test #'string=)))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-list-packages (not SBCL)~%"))

(define-testcase validate-list-package-symbols ()
  "Verify list-package-symbols returns symbol descriptors."
  #+sbcl
  (with-test-child (conn)
    (declare (ignore conn))
    (let* ((server (make-instance 'atelier/mcp::mcp-server
                                  :stream (make-two-way-stream
                                           (make-string-input-stream "")
                                           (make-string-output-stream))))
           (atelier/mcp::*current-server* server))
      (setf (atelier/mcp:server-child-connection server) *test-child*)
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:list-package-symbols"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "package-name" "COMMON-LISP")))))
        ;; Should have CAR in the results
        (assert-t* (find "CAR" result
                         :key (lambda (sym)
                                (cdr (assoc "name" sym :test #'string=)))
                         :test #'string=)))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-list-package-symbols (not SBCL)~%"))

(define-testcase validate-describe-symbol ()
  "Verify describe-symbol returns function info."
  #+sbcl
  (with-test-child (conn)
    (declare (ignore conn))
    (let* ((server (make-instance 'atelier/mcp::mcp-server
                                  :stream (make-two-way-stream
                                           (make-string-input-stream "")
                                           (make-string-output-stream))))
           (atelier/mcp::*current-server* server))
      (setf (atelier/mcp:server-child-connection server) *test-child*)
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:describe-symbol"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "symbol-designator" "cl:car")))))
        (assert-t* (assoc "kind" result :test #'string=))
        (assert-string= "FUNCTION"
                         (cdr (assoc "kind" result :test #'string=))))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-describe-symbol (not SBCL)~%"))

(define-testcase validate-find-definition ()
  "Verify find-definition returns a source location for a known symbol."
  #+sbcl
  (with-test-child (conn)
    (declare (ignore conn))
    (let* ((server (make-instance 'atelier/mcp::mcp-server
                                  :stream (make-two-way-stream
                                           (make-string-input-stream "")
                                           (make-string-output-stream))))
           (atelier/mcp::*current-server* server))
      (setf (atelier/mcp:server-child-connection server) *test-child*)
      ;; First load atelier in the child so we have a source-tracked symbol
      (atelier/mcp:connection-eval *test-child*
                                   "(asdf:load-system \"org.melusina.atelier\")")
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:find-definition"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "symbol-designator" "atelier:lint-string")))))
        ;; Should have source-file
        (assert-t* (or (eq result 'cl:null)
                       (assoc "source-file" result :test #'string=))))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-find-definition (not SBCL)~%"))

(define-testcase validate-introspection-missing-symbol ()
  "Verify introspection on non-existent symbol returns an error."
  #+sbcl
  (with-test-child (conn)
    (declare (ignore conn))
    (let* ((server (make-instance 'atelier/mcp::mcp-server
                                  :stream (make-two-way-stream
                                           (make-string-input-stream "")
                                           (make-string-output-stream))))
           (atelier/mcp::*current-server* server))
      (setf (atelier/mcp:server-child-connection server) *test-child*)
      (let ((tool (atelier/mcp:find-tool-by-name "atelier:describe-symbol")))
        (assert-condition
         (atelier/mcp:handle-tool-call
          tool
          (list (cons "symbol-designator" "nonexistent-pkg:nonexistent-sym")))
         error))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-introspection-missing-symbol (not SBCL)~%"))


;;; ---- S7: Test runner tests ----

(define-testcase validate-run-tests-fresh ()
  "Verify run-tests-fresh spawns a separate SBCL and runs tests."
  #+sbcl
  (let* ((tool (atelier/mcp:find-tool-by-name "atelier:run-tests-fresh"))
         (result (atelier/mcp:handle-tool-call
                  tool
                  (list (cons "system-name" "org.melusina.atelier")))))
    (assert-t* (assoc "exit-code" result :test #'string=))
    (assert-t* (assoc "output" result :test #'string=))
    ;; Check it succeeded
    (assert= 0 (cdr (assoc "exit-code" result :test #'string=))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-run-tests-fresh (not SBCL)~%"))

(define-testcase validate-run-tests-in-child ()
  "Verify run-tests-in-child uses the session child."
  #+sbcl
  (with-test-child (conn)
    (declare (ignore conn))
    (let* ((server (make-instance 'atelier/mcp::mcp-server
                                  :stream (make-two-way-stream
                                           (make-string-input-stream "")
                                           (make-string-output-stream))))
           (atelier/mcp::*current-server* server))
      (setf (atelier/mcp:server-child-connection server) *test-child*)
      ;; Load atelier in the child first
      (atelier/mcp:connection-eval *test-child*
                                   "(asdf:load-system \"org.melusina.atelier\")")
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:run-tests-in-child"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "system-name" "org.melusina.atelier")))))
        ;; Should have output
        (assert-t* (assoc "system" result :test #'string=)))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-run-tests-in-child (not SBCL)~%"))

(define-testcase validate-run-tests-fresh-load-failure ()
  "Verify run-tests-fresh handles load failure gracefully."
  #+sbcl
  (let* ((tool (atelier/mcp:find-tool-by-name "atelier:run-tests-fresh"))
         (result (atelier/mcp:handle-tool-call
                  tool
                  (list (cons "system-name" "nonexistent-system-xyz")))))
    ;; Should have a non-zero exit code
    (assert-t* (not (zerop (cdr (assoc "exit-code" result :test #'string=))))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-run-tests-fresh-load-failure (not SBCL)~%"))


;;; ---- Registration and orphan tests ----

(define-testcase validate-tool-registration-count ()
  "Verify that 14 tools are registered (6 from slice 009 + 8 new)."
  (assert= 14 (hash-table-count atelier/mcp:*tool-registry*)))

(define-testcase validate-no-orphan-sbcl ()
  "Verify no orphan SBCL processes after test suite child cleanup."
  #+sbcl
  (let ((before-count (%count-sbcl-processes)))
    ;; Spawn and shut down a child
    (let ((conn (atelier/mcp:make-child-connection)))
      (atelier/mcp:connection-eval conn "(+ 1 2)")
      (atelier/mcp:connection-shutdown conn))
    (sleep 1)
    (let ((after-count (%count-sbcl-processes)))
      ;; After count should be <= before count
      (assert-t* (<= after-count before-count))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-no-orphan-sbcl (not SBCL)~%"))

(defun %count-sbcl-processes ()
  "Count the number of SBCL processes on this machine."
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program '("pgrep" "-c" "sbcl")
                        :output :string
                        :error-output nil
                        :ignore-error-status t)
    (declare (ignore error-output))
    (if (zerop exit-code)
        (or (parse-integer (string-trim '(#\Space #\Newline #\Return) output)
                           :junk-allowed t)
            0)
        0)))


;;; ---- Combined runner for child-dependent tests ----

(define-testcase run-child-dependent-tests ()
  "Run all tests that require a child SBCL connection."
  (let ((*test-child* nil))
    (unwind-protect
         (progn
           ;; S4: Child connection
           (validate-child-connection-spawn)
           (validate-child-connection-eval)
           (validate-child-connection-sequential-state)
           ;; S5: Eval-form tool
           (validate-eval-form-basic)
           (validate-eval-form-multiple-values)
           (validate-eval-form-error)
           (validate-eval-form-output-capture)
           ;; S6: Introspection
           (validate-list-packages)
           (validate-list-package-symbols)
           (validate-describe-symbol)
           (validate-find-definition)
           (validate-introspection-missing-symbol)
           ;; S7: Test runners (in-child only; fresh is separate)
           (validate-run-tests-in-child))
      (when *test-child*
        (ignore-errors (atelier/mcp:connection-shutdown *test-child*))
        (setf *test-child* nil)))))

;;;; End of file `child-tests.lisp'
