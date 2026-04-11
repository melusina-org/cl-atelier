;;;; protocol-handshake.lisp — End-to-end handshake tests via serve-two-way-stream

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

(defun %run-server-on-string (input)
  "Feed INPUT to SERVE-TWO-WAY-STREAM and return the output string."
  (with-output-to-string (out)
    (with-input-from-string (in input)
      (serve-two-way-stream (make-two-way-stream in out)))))

(defun %fixture-initialize-request ()
  "Return the initialize request frame as a single line."
  "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2024-11-05\",\"capabilities\":{},\"clientInfo\":{\"name\":\"test\",\"version\":\"1.0\"}}}")

(define-testcase validate-handshake-initialize ()
  "Initialize request produces a success response naming the server."
  (let* ((output (%run-server-on-string
                  (format nil "~A~%" (%fixture-initialize-request))))
         (lines (split-non-blank-lines output))
         (parsed (decode-from-string (first lines)))
         (result (gethash "result" parsed))
         (server-info (gethash "serverInfo" result)))
    (assert-eql 1 (length lines))
    (assert-string= "2024-11-05" (gethash "protocolVersion" result))
    (assert-string= "org.melusina.atelier/mcp" (gethash "name" server-info))))

(define-testcase validate-handshake-notification-suppressed ()
  "The initialized notification produces no response line."
  (let* ((input (format nil "~A~%{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}~%"
                        (%fixture-initialize-request)))
         (output (%run-server-on-string input))
         (lines  (split-non-blank-lines output)))
    (assert-eql 1 (length lines))))

(define-testcase validate-handshake-tools-list-returns-six ()
  "tools/list returns exactly the six tools declared in slice 009."
  (let* ((input (format nil "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\"}~%"))
         (output (%run-server-on-string input))
         (parsed (decode-from-string (first (split-non-blank-lines output))))
         (tools  (gethash "tools" (gethash "result" parsed))))
    (assert-eql 6 (length tools))))

(define-testcase validate-handshake-resources-list-returns-three ()
  "resources/list returns exactly the three concrete resources."
  (let* ((input (format nil "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"resources/list\"}~%"))
         (output (%run-server-on-string input))
         (parsed (decode-from-string (first (split-non-blank-lines output))))
         (resources (gethash "resources" (gethash "result" parsed))))
    (assert-eql 3 (length resources))))

(define-testcase validate-handshake-resources-templates-list-returns-five ()
  "resources/templates/list returns exactly the five URI templates."
  (let* ((input (format nil "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"resources/templates/list\"}~%"))
         (output (%run-server-on-string input))
         (parsed (decode-from-string (first (split-non-blank-lines output))))
         (templates (gethash "resourceTemplates" (gethash "result" parsed))))
    (assert-eql 5 (length templates))))

(define-testcase validate-handshake-malformed-json-parse-error ()
  "Malformed JSON produces a -32700 parse error."
  (let* ((output (%run-server-on-string
                  (format nil "this is not json~%")))
         (parsed (decode-from-string (first (split-non-blank-lines output))))
         (err (gethash "error" parsed)))
    (assert-eql -32700 (gethash "code" err))))

(define-testcase validate-handshake-tests ()
  (validate-handshake-initialize)
  (validate-handshake-notification-suppressed)
  (validate-handshake-tools-list-returns-six)
  (validate-handshake-resources-list-returns-three)
  (validate-handshake-resources-templates-list-returns-five)
  (validate-handshake-malformed-json-parse-error))

;;;; End of file `protocol-handshake.lisp'
