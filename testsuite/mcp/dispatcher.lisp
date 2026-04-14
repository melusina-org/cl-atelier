;;;; dispatcher.lisp — Tests for the HANDLE-MESSAGE dispatch

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

(defun %dispatch-json (json-string)
  "Parse JSON-STRING, dispatch via HANDLE-MESSAGE, return the response."
  (handle-message (parse-mcp-message (decode-from-string json-string)) nil))

(define-testcase validate-dispatch-initialize ()
  "The initialize method returns a success response with protocolVersion."
  (let ((resp (%dispatch-json "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}")))
    (assert-type resp 'mcp-success-response)
    (assert-string= "2024-11-05"
                    (gethash "protocolVersion" (response-result resp)))))

(define-testcase validate-dispatch-initialized-notification-returns-nil ()
  "Notifications do not produce a response."
  (assert-nil
   (%dispatch-json "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}")))

(define-testcase validate-dispatch-unknown-method ()
  "Unknown request methods produce -32601."
  (let ((resp (%dispatch-json "{\"jsonrpc\":\"2.0\",\"id\":9,\"method\":\"foo/bar\"}")))
    (assert-type resp 'mcp-error-response)
    (assert-eql -32601 (error-code resp))))

(define-testcase validate-dispatch-tool-handler-signal-in-band ()
  "A signalling tool handler produces an in-band isError:true result."
  (with-isolated-registries
    (let ((*package* (find-package :atelier/mcp)))
      (eval (read-from-string
             "(define-tool boom () (:description \"boom\") (error \"kaboom\"))")))
    (let* ((resp (%dispatch-json
                  "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"tools/call\",\"params\":{\"name\":\"atelier:boom\",\"arguments\":{}}}"))
           (result (response-result resp))
           (is-error (gethash "isError" result))
           (content (gethash "content" result)))
      (assert-type resp 'mcp-success-response)
      (assert-eq t is-error)
      (assert-t* (search "kaboom"
                        (gethash "text" (aref content 0)))))))

(define-testcase validate-dispatch-resources-read-not-found ()
  "An unknown URI produces -32002."
  (with-isolated-registries
    (let ((resp (%dispatch-json
                 "{\"jsonrpc\":\"2.0\",\"id\":8,\"method\":\"resources/read\",\"params\":{\"uri\":\"unknown://foo\"}}")))
      (assert-type resp 'mcp-error-response)
      (assert-eql -32002 (error-code resp)))))

(define-testcase validate-dispatch-tools-list-method-present ()
  "A HANDLE-MESSAGE method is defined for TOOLS-LIST-REQUEST."
  (assert-t (not (null (find-method #'handle-message '()
                                    (list (find-class 'tools-list-request)
                                          (find-class t))
                                    nil)))))

(define-testcase validate-dispatch-tools-call-binds-current-server ()
  "The tools/call dispatcher dynamically binds *CURRENT-SERVER* so
   tool handlers can access the server instance. Regression test for
   the bug where *CURRENT-SERVER* was defined after dispatcher.lisp
   in load order, causing the LET in the dispatcher to create a
   lexical binding instead of a dynamic one."
  (with-isolated-registries
    (let ((*package* (find-package :atelier/mcp)))
      (eval (read-from-string
             "(define-tool check-server-bound ()
                (:description \"Return whether *current-server* is bound.\")
                (list (cons \"bound\" (not (null *current-server*)))))")))
    (let* ((server (make-instance 'atelier/mcp::mcp-server
                                  :stream (make-two-way-stream
                                           (make-string-input-stream "")
                                           (make-string-output-stream))))
           (resp (handle-message
                  (parse-mcp-message
                   (decode-from-string
                    "{\"jsonrpc\":\"2.0\",\"id\":99,\"method\":\"tools/call\",\"params\":{\"name\":\"atelier:check-server-bound\",\"arguments\":{}}}"))
                  server)))
      ;; The response should be a success with bound=true
      (assert-type resp 'mcp-success-response)
      ;; Extract the text content and verify it contains "bound" as truthy
      (let* ((result (response-result resp))
             (content (gethash "content" result))
             (text (gethash "text" (aref content 0))))
        (assert-t* (search "true" text))))))

(define-testcase validate-dispatcher-tests ()
  (validate-dispatch-initialize)
  (validate-dispatch-initialized-notification-returns-nil)
  (validate-dispatch-unknown-method)
  (validate-dispatch-tool-handler-signal-in-band)
  (validate-dispatch-resources-read-not-found)
  (validate-dispatch-tools-list-method-present)
  (validate-dispatch-tools-call-binds-current-server))

;;;; End of file `dispatcher.lisp'
