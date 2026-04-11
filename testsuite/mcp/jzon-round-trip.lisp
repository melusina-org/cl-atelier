;;;; jzon-round-trip.lisp — jzon round-trip smoke tests (Step 0)

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

(define-testcase validate-jzon-empty-object-round-trip ()
  "Empty JSON object parses to an empty hash-table and stringifies back."
  (let ((parsed (decode-from-string "{}")))
    (assert-t (hash-table-p parsed))
    (assert-eql 0 (hash-table-count parsed))
    (assert-string= "{}" (encode-to-string parsed))))

(define-testcase validate-jzon-empty-array-round-trip ()
  "Empty JSON array parses to an empty vector and stringifies back."
  (let ((parsed (decode-from-string "[]")))
    (assert-equal #() parsed)
    (assert-string= "[]" (encode-to-string parsed))))

(define-testcase validate-jzon-null-true-false ()
  "JSON literals map to distinct CL values via the +json-*+ constants."
  (assert-eq +json-null+  (decode-from-string "null"))
  (assert-eq +json-true+  (decode-from-string "true"))
  (assert-eq +json-false+ (decode-from-string "false"))
  (assert-string= "null"  (encode-to-string +json-null+))
  (assert-string= "true"  (encode-to-string +json-true+))
  (assert-string= "false" (encode-to-string +json-false+)))

(define-testcase validate-make-json-object-encodes-as-object ()
  "make-json-object builds a hash-table that stringifies to a JSON object."
  (let* ((object (make-json-object "jsonrpc" "2.0" "id" 1))
         (encoded (encode-to-string object)))
    (assert-string= "{\"jsonrpc\":\"2.0\",\"id\":1}" encoded)))

(define-testcase validate-plist-to-json-object ()
  "plist-to-json-object converts to an object, not a flat array."
  (let ((encoded (encode-to-string
                  (plist-to-json-object '(:foo 1 :bar 2)))))
    (assert-string= "{\"foo\":1,\"bar\":2}" encoded)))

(define-testcase validate-alist-to-json-object ()
  "alist-to-json-object converts to an object."
  (let ((encoded (encode-to-string
                  (alist-to-json-object '(("foo" . 1) ("bar" . 2))))))
    (assert-string= "{\"foo\":1,\"bar\":2}" encoded)))

(define-testcase validate-three-state-key-extraction ()
  "(gethash key obj missing) distinguishes absent from null from value."
  (let ((notif (decode-from-string "{\"jsonrpc\":\"2.0\",\"method\":\"foo\"}"))
        (req   (decode-from-string "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"foo\"}"))
        (null  (decode-from-string "{\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"foo\"}"))
        (sentinel (missing-key)))
    (assert-eq sentinel (gethash "id" notif sentinel))
    (assert= 7 (gethash "id" req sentinel))
    (assert-eq +json-null+ (gethash "id" null sentinel))))

(define-testcase validate-jzon-round-trip-initialize-request ()
  "Round-trip the canonical MCP initialize request shape."
  (let* ((source "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2024-11-05\",\"capabilities\":{},\"clientInfo\":{\"name\":\"test\",\"version\":\"1.0\"}}}")
         (parsed (decode-from-string source))
         (params (gethash "params" parsed))
         (client-info (gethash "clientInfo" params)))
    (assert-string= "2.0" (gethash "jsonrpc" parsed))
    (assert= 1 (gethash "id" parsed))
    (assert-string= "initialize" (gethash "method" parsed))
    (assert-string= "2024-11-05" (gethash "protocolVersion" params))
    (assert-string= "test" (gethash "name" client-info))))

(define-testcase validate-jzon-round-trip-tools-list-response ()
  "Round-trip the canonical MCP tools/list response shape."
  (let* ((source "{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"tools\":[{\"name\":\"atelier:ping\",\"description\":\"smoke\",\"inputSchema\":{\"type\":\"object\",\"properties\":{}}}]}}")
         (parsed (decode-from-string source))
         (result (gethash "result" parsed))
         (tools  (gethash "tools" result)))
    (assert= 1 (length tools))
    (let ((first (aref tools 0)))
      (assert-string= "atelier:ping" (gethash "name" first))
      (assert-string= "smoke"        (gethash "description" first)))))

(define-testcase validate-jzon-round-trip ()
  "Combined entry point for the jzon round-trip suite."
  (validate-jzon-empty-object-round-trip)
  (validate-jzon-empty-array-round-trip)
  (validate-jzon-null-true-false)
  (validate-make-json-object-encodes-as-object)
  (validate-plist-to-json-object)
  (validate-alist-to-json-object)
  (validate-three-state-key-extraction)
  (validate-jzon-round-trip-initialize-request)
  (validate-jzon-round-trip-tools-list-response))

;;;; End of file `jzon-round-trip.lisp'
