;;;; asdf-tools-tests.lisp — Tests for ASDF/Quicklisp/Confidence tools (slice 012)

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

;;; Tests for slice 012: quickload, system-info, system-apropos,
;;; list-testcases, run-testcase.
;;; All slow (require a child SBCL via SWANK).


;;; ---- S1: quickload ----

(define-testcase validate-quickload-known-system ()
  "quickload loads a known system successfully."
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
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:quickload"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "system-name" "org.melusina.atelier")))))
        (assert-t (cdr (assoc "success" result :test #'string=)))
        (assert-t* (assoc "duration-ms" result :test #'string=)))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-quickload-known-system (not SBCL)~%"))


;;; ---- S2: system-info ----

(define-testcase validate-system-info ()
  "system-info returns details for a loaded system."
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
      ;; Load atelier first
      (atelier/mcp:connection-eval *test-child*
                                   "(asdf:load-system \"org.melusina.atelier\")")
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:system-info"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "system-name" "org.melusina.atelier")))))
        (assert-string= "org.melusina.atelier"
                         (cdr (assoc "name" result :test #'string=)))
        (assert-t* (assoc "depends-on" result :test #'string=))
        (assert-t* (assoc "source-directory" result :test #'string=)))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-system-info (not SBCL)~%"))


;;; ---- S3: system-apropos ----

(define-testcase validate-system-apropos ()
  "system-apropos finds systems matching a search string (runs in parent)."
  (let* ((tool (atelier/mcp:find-tool-by-name "atelier:system-apropos"))
         (result (atelier/mcp:handle-tool-call
                  tool
                  (list (cons "search-string" "atelier")))))
    (assert-t* (listp result))
    (assert-t* (find "org.melusina.atelier" result :test #'string=))))


;;; ---- S4: list-testcases ----

(define-testcase validate-list-testcases ()
  "list-testcases discovers Confidence testcases in a package."
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
      ;; Load the testsuite first
      (atelier/mcp:connection-eval *test-child*
                                   "(asdf:load-system \"org.melusina.atelier/testsuite\")")
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:list-testcases"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "package-name" "ATELIER/TESTSUITE")))))
        (assert-t* (listp result))
        ;; Should find run-all-tests
        (assert-t* (find "ATELIER/TESTSUITE:RUN-ALL-TESTS" result
                         :key (lambda (entry)
                                (cdr (assoc "name" entry :test #'string=)))
                         :test #'string=)))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-list-testcases (not SBCL)~%"))


;;; ---- S5: run-testcase ----

(define-testcase validate-run-testcase ()
  "run-testcase runs a specific Confidence testcase."
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
      ;; Load the testsuite first
      (atelier/mcp:connection-eval *test-child*
                                   "(asdf:load-system \"org.melusina.atelier/testsuite\")")
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:run-testcase"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "testcase-designator"
                                  "ATELIER/TESTSUITE:TESTSUITE-PARAMETER")))))
        (assert-t* (assoc "name" result :test #'string=))
        (assert-t* (assoc "output" result :test #'string=)))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-run-testcase (not SBCL)~%"))


;;; ---- S6: Tool count ----

(define-testcase validate-asdf-tool-count ()
  "Verify that 23 tools are registered (18 + 5 new in slice 012)."
  (assert= 23 (hash-table-count atelier/mcp:*tool-registry*)))


;;; ---- Combined runner ----

(define-testcase run-asdf-tools-tests ()
  "Run all ASDF/Quicklisp/Confidence tool tests with a shared child."
  (let ((*test-child* nil))
    (unwind-protect
         (progn
           (validate-quickload-known-system)
           (validate-system-info)
           (validate-system-apropos)
           (validate-list-testcases)
           (validate-run-testcase))
      (when *test-child*
        (ignore-errors (atelier/mcp:connection-shutdown *test-child*))
        (setf *test-child* nil)))))

;;;; End of file `asdf-tools-tests.lisp'
