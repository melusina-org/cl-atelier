;;;; documentation-tools-tests.lisp — Tests for documentation tools (slice 013)

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

;;; Tests for slice 013: apropos, hyperspec-lookup, hyperspec-issue,
;;; hyperspec-issues, macroexpand-form, disassemble-symbol, compile-form.


;;; ---- S1: apropos ----

(define-testcase validate-apropos-no-filter ()
  "apropos with no package filter returns symbols from multiple packages."
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
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:apropos-search"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "search-string" "MAPCAR")))))
        (assert-t* (listp result))
        ;; Should find CL:MAPCAR at minimum
        (assert-t* (find "MAPCAR" result
                         :key (lambda (entry)
                                (cdr (assoc "name" entry :test #'string=)))
                         :test #'string=)))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-apropos-no-filter (not SBCL)~%"))

(define-testcase validate-apropos-package-filter ()
  "apropos with package filter returns only symbols from that package."
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
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:apropos-search"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "search-string" "MAP")
                            (cons "package-name" "COMMON-LISP")))))
        (assert-t* (listp result))
        ;; Every result should be from CL package
        (assert-t (not (null result)))
        (dolist (entry result)
          (assert-string= "COMMON-LISP"
                          (cdr (assoc "package" entry :test #'string=)))))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-apropos-package-filter (not SBCL)~%"))


;;; ---- S2: hyperspec-lookup ----

(define-testcase validate-hyperspec-lookup-known ()
  "hyperspec-lookup for MAPCAR returns non-empty HTML content."
  (unless (atelier/mcp::hyperspec-available-p)
    (format t "~&;; SKIPPED: validate-hyperspec-lookup-known (no HyperSpec)~%")
    (return-from validate-hyperspec-lookup-known))
  (let* ((tool (atelier/mcp:find-tool-by-name "atelier:hyperspec-lookup"))
         (result (atelier/mcp:handle-tool-call
                  tool
                  (list (cons "symbol-name" "MAPCAR")))))
    (assert-string= "MAPCAR" (cdr (assoc "symbol" result :test #'string=)))
    (assert-t* (cdr (assoc "content" result :test #'string=)))
    ;; Content should contain HTML
    (assert-t* (search "<HTML>" (string-upcase
                                 (cdr (assoc "content" result :test #'string=)))))))

(define-testcase validate-hyperspec-lookup-unknown ()
  "hyperspec-lookup for unknown symbol returns resource-not-found."
  (unless (atelier/mcp::hyperspec-available-p)
    (format t "~&;; SKIPPED: validate-hyperspec-lookup-unknown (no HyperSpec)~%")
    (return-from validate-hyperspec-lookup-unknown))
  (let ((tool (atelier/mcp:find-tool-by-name "atelier:hyperspec-lookup")))
    (assert-condition
     (atelier/mcp:handle-tool-call
      tool
      (list (cons "symbol-name" "MY-TOTALLY-FAKE-SYMBOL-12345")))
     atelier/mcp::resource-not-found)))


;;; ---- S3: hyperspec symbol resource ----

(define-testcase validate-hyperspec-symbol-resource ()
  "lisp://hyperspec/symbol/{name} resource returns HTML content."
  (unless (atelier/mcp::hyperspec-available-p)
    (format t "~&;; SKIPPED: validate-hyperspec-symbol-resource (no HyperSpec)~%")
    (return-from validate-hyperspec-symbol-resource))
  (let ((resource (atelier/mcp:match-resource-uri "lisp://hyperspec/symbol/DEFUN")))
    (assert-t* resource)))


;;; ---- S4: hyperspec-issue ----

(define-testcase validate-hyperspec-issue-known ()
  "hyperspec-issue for ADJUST-ARRAY-DISPLACEMENT returns issue HTML."
  (unless (atelier/mcp::hyperspec-available-p)
    (format t "~&;; SKIPPED: validate-hyperspec-issue-known (no HyperSpec)~%")
    (return-from validate-hyperspec-issue-known))
  (let* ((tool (atelier/mcp:find-tool-by-name "atelier:hyperspec-issue"))
         (result (atelier/mcp:handle-tool-call
                  tool
                  (list (cons "issue-name" "ADJUST-ARRAY-DISPLACEMENT")))))
    (assert-string= "ADJUST-ARRAY-DISPLACEMENT"
                     (cdr (assoc "issue" result :test #'string=)))
    (assert-t* (cdr (assoc "content" result :test #'string=)))))

(define-testcase validate-hyperspec-issue-unknown ()
  "hyperspec-issue for unknown issue returns resource-not-found."
  (unless (atelier/mcp::hyperspec-available-p)
    (format t "~&;; SKIPPED: validate-hyperspec-issue-unknown (no HyperSpec)~%")
    (return-from validate-hyperspec-issue-unknown))
  (let ((tool (atelier/mcp:find-tool-by-name "atelier:hyperspec-issue")))
    (assert-condition
     (atelier/mcp:handle-tool-call
      tool
      (list (cons "issue-name" "TOTALLY-FAKE-ISSUE-12345")))
     atelier/mcp::resource-not-found)))


;;; ---- S5: hyperspec-issues list ----

(define-testcase validate-hyperspec-issues-list ()
  "lisp://hyperspec/issues resource returns list of 365 issue names."
  (unless (atelier/mcp::hyperspec-available-p)
    (format t "~&;; SKIPPED: validate-hyperspec-issues-list (no HyperSpec)~%")
    (return-from validate-hyperspec-issues-list))
  (let* ((tool (atelier/mcp:find-tool-by-name "atelier:hyperspec-issues"))
         (result (atelier/mcp:handle-tool-call tool nil)))
    (assert-t* (listp result))
    (assert= 365 (length result))
    ;; Should contain known issue names
    (assert-t* (find "ADJUST-ARRAY-DISPLACEMENT" result :test #'string=))))

(define-testcase validate-hyperspec-issue-resource ()
  "lisp://hyperspec/issues/{name} resource is registered."
  (unless (atelier/mcp::hyperspec-available-p)
    (format t "~&;; SKIPPED: validate-hyperspec-issue-resource (no HyperSpec)~%")
    (return-from validate-hyperspec-issue-resource))
  (let ((resource (atelier/mcp:match-resource-uri
                   "lisp://hyperspec/issues/ADJUST-ARRAY-DISPLACEMENT")))
    (assert-t* resource)))


;;; ---- S6: macroexpand-form ----

(define-testcase validate-macroexpand-form ()
  "macroexpand-form with default (single step) returns expansion."
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
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:macroexpand-form"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "form" "(defun foo (x) x)")))))
        (assert-t* (cdr (assoc "expanded" result :test #'string=)))
        ;; defun expands to something non-trivial
        (assert-t (not (equal "(defun foo (x) x)"
                              (cdr (assoc "expanded" result :test #'string=))))))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-macroexpand-form (not SBCL)~%"))

(define-testcase validate-macroexpand-form-fully ()
  "macroexpand-form with fully=true returns full expansion."
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
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:macroexpand-form"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "form" "(defun bar (x) x)")
                            (cons "fully" t)))))
        (assert-t* (cdr (assoc "expanded" result :test #'string=)))
        (assert-t (cdr (assoc "fully" result :test #'string=))))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-macroexpand-form-fully (not SBCL)~%"))


;;; ---- S7: disassemble-symbol ----

(define-testcase validate-disassemble-symbol ()
  "disassemble-symbol for CL:CAR returns assembly output."
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
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:disassemble-symbol"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "symbol-designator" "cl:car")))))
        (assert-string= "cl:car"
                         (cdr (assoc "symbol" result :test #'string=)))
        (assert-t* (cdr (assoc "disassembly" result :test #'string=)))
        ;; Should contain some assembly-like content
        (assert-t* (> (length (cdr (assoc "disassembly" result :test #'string=))) 10)))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-disassemble-symbol (not SBCL)~%"))

(define-testcase validate-disassemble-symbol-unknown ()
  "disassemble-symbol for nonexistent function returns error."
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
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:disassemble-symbol"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "symbol-designator"
                                  "cl-user::totally-fake-fn-12345")))))
        ;; Should have an error field
        (assert-t* (assoc "error" result :test #'string=)))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-disassemble-symbol-unknown (not SBCL)~%"))


;;; ---- S8: compile-form ----

(define-testcase validate-compile-form-clean ()
  "compile-form for a clean form returns no diagnostics."
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
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:compile-form"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "form" "(+ 1 2)")))))
        (assert= 0 (cdr (assoc "diagnostic-count" result :test #'string=))))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-compile-form-clean (not SBCL)~%"))

(define-testcase validate-compile-form-with-warning ()
  "compile-form for a form with type issue returns diagnostics."
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
      (let* ((tool (atelier/mcp:find-tool-by-name "atelier:compile-form"))
             (result (atelier/mcp:handle-tool-call
                      tool
                      (list (cons "form" "(+ 1 \"x\")")))))
        ;; Should have at least one diagnostic
        (assert-t* (> (cdr (assoc "diagnostic-count" result :test #'string=)) 0)))))
  #-sbcl
  (format t "~&;; SKIPPED: validate-compile-form-with-warning (not SBCL)~%"))


;;; ---- Tool count ----

(define-testcase validate-documentation-tool-count ()
  "Verify that 40 tools are registered (30 + 10 new in slice 014)."
  (assert= 40 (hash-table-count atelier/mcp:*tool-registry*)))


;;; ---- Combined runner ----

(define-testcase run-documentation-tools-tests ()
  "Run all documentation tool tests with a shared child."
  (let ((*test-child* nil))
    (unwind-protect
         (progn
           ;; Child-dependent tests
           (validate-apropos-no-filter)
           (validate-apropos-package-filter)
           (validate-macroexpand-form)
           (validate-macroexpand-form-fully)
           (validate-disassemble-symbol)
           (validate-disassemble-symbol-unknown)
           (validate-compile-form-clean)
           (validate-compile-form-with-warning))
      (when *test-child*
        (ignore-errors (atelier/mcp:connection-shutdown *test-child*))
        (setf *test-child* nil)))))

;;;; End of file `documentation-tools-tests.lisp'
