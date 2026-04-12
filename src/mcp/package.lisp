;;;; package.lisp — Package for the Atelier MCP server

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(defpackage #:atelier/mcp
  (:use #:common-lisp)
  (:export
   ;; Protocol version
   #:+mcp-protocol-version+
   ;; Entry point
   #:serve-two-way-stream
   ;; Conditions
   #:mcp-error
   #:tool-not-found
   #:resource-not-found
   #:not-implemented
   #:invalid-tool-arguments
   #:invalid-uri-template
   ;; JSON utilities
   #:+json-null+
   #:+json-true+
   #:+json-false+
   #:encode-to-string
   #:decode-from-string
   #:make-json-object
   #:alist-to-json-object
   #:plist-to-json-object
   #:json-object-to-alist
   #:missing-key
   ;; Tool-name helper
   #:derive-tool-name-from-symbol
   ;; Input-schema helper
   #:derive-input-schema-from-lambda-list
   ;; URI template helper
   #:parse-uri-template
   #:match-uri-against-template
   #:validate-uri-template-against-lambda-list
   #:uri-template-static-p
   ;; Tool class and registries
   #:tool
   #:resource-tool
   #:tool-name
   #:tool-description
   #:tool-input-schema
   #:resource-uri-template
   #:resource-name
   #:resource-mime-type
   #:handle-tool-call
   #:register-tool
   #:*tool-registry*
   #:*concrete-resource-registry*
   #:*template-resource-registry*
   #:list-tools
   #:list-concrete-resources
   #:list-template-resources
   #:find-tool-by-name
   #:find-concrete-resource-by-uri
   #:match-resource-uri
   ;; Define-tool macro
   #:define-tool
   ;; Message hierarchy
   #:mcp-message
   #:mcp-request
   #:mcp-notification
   #:mcp-response
   #:mcp-success-response
   #:mcp-error-response
   #:initialize-request
   #:initialized-notification
   #:tools-list-request
   #:tools-call-request
   #:resources-list-request
   #:resources-templates-list-request
   #:resources-read-request
   #:parse-mcp-message
   #:handle-message
   #:message-raw-json
   #:message-timestamp
   #:request-id
   #:request-method
   #:request-params
   #:response-request
   #:response-result
   #:error-code
   #:error-message
   #:error-data
   ;; Transcript
   #:transcript
   #:make-transcript
   #:transcript-session-id
   #:transcript-path
   #:transcript-next-seq
   #:write-transcript-entry
   #:read-transcript
   #:read-transcript-entries
   #:sexp-to-json-entries
   #:sexp-to-markdown-entries
   ;; Image connection
   #:image-connection
   #:connection-id
   #:connection-process-info
   #:connection-eval
   #:connection-shutdown
   #:connection-alive-p
   ;; Child connection (slice 010)
   #:child-connection
   #:make-child-connection
   #:child-connection-port
   #:child-image-spawn-failed
   ;; SWANK protocol client (slice 010)
   #:swank-connection
   #:swank-connect
   #:swank-disconnect
   #:swank-send
   #:swank-send-raw
   #:swank-receive
   #:swank-eval
   ;; Session child access (slice 010)
   #:server-child-connection
   #:ensure-child-connection))

;;;; End of file `package.lisp'
