;;;; message-parsing.lisp — Tests for parse-mcp-message

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

(defun %parse-json (string)
  "Parse STRING as a JSON object and return the MCP message instance."
  (parse-mcp-message (decode-from-string string)))

(define-testcase validate-parse-initialize-request ()
  (let ((msg (%parse-json "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}")))
    (assert-type msg 'initialize-request)
    (assert-string= "initialize" (request-method msg))
    (assert-eql 1 (request-id msg))))

(define-testcase validate-parse-tools-list-request ()
  (let ((msg (%parse-json "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\"}")))
    (assert-type msg 'tools-list-request)))

(define-testcase validate-parse-tools-call-request ()
  (let ((msg (%parse-json "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\"name\":\"x\"}}")))
    (assert-type msg 'tools-call-request)
    (assert-type (request-params msg) 'hash-table)))

(define-testcase validate-parse-resources-list-request ()
  (assert-type (%parse-json "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"resources/list\"}")
               'resources-list-request))

(define-testcase validate-parse-resources-templates-list-request ()
  (assert-type (%parse-json "{\"jsonrpc\":\"2.0\",\"id\":5,\"method\":\"resources/templates/list\"}")
               'resources-templates-list-request))

(define-testcase validate-parse-resources-read-request ()
  (assert-type (%parse-json "{\"jsonrpc\":\"2.0\",\"id\":6,\"method\":\"resources/read\",\"params\":{\"uri\":\"x\"}}")
               'resources-read-request))

(define-testcase validate-parse-initialized-notification ()
  (let ((msg (%parse-json "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}")))
    (assert-type msg 'initialized-notification)
    (assert-string= "notifications/initialized" (request-method msg))))

(define-testcase validate-parse-unknown-method-falls-through-to-base ()
  "Unknown request method falls through to MCP-REQUEST base class."
  (let ((msg (%parse-json "{\"jsonrpc\":\"2.0\",\"id\":9,\"method\":\"unknown/method\"}")))
    (assert-type msg 'mcp-request)))

(define-testcase validate-parse-unknown-notification-falls-through ()
  "Unknown notification falls through to MCP-NOTIFICATION base class."
  (let ((msg (%parse-json "{\"jsonrpc\":\"2.0\",\"method\":\"unknown/notif\"}")))
    (assert-type msg 'mcp-notification)))

(define-testcase validate-parse-null-id-signals-error ()
  "A JSON null id is malformed and signals mcp-error."
  (assert-condition
   (%parse-json "{\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"foo\"}")
   mcp-error))

(define-testcase validate-parse-mcp-message ()
  (validate-parse-initialize-request)
  (validate-parse-tools-list-request)
  (validate-parse-tools-call-request)
  (validate-parse-resources-list-request)
  (validate-parse-resources-templates-list-request)
  (validate-parse-resources-read-request)
  (validate-parse-initialized-notification)
  (validate-parse-unknown-method-falls-through-to-base)
  (validate-parse-unknown-notification-falls-through)
  (validate-parse-null-id-signals-error))

;;;; End of file `message-parsing.lisp'
