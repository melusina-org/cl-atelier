;;;; package.lisp — Packages for MCP kernel and MCP server

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT


;;;;
;;;; Reload preparation — resolve symbol conflicts in live images
;;;;

;;; When reloading in a live image where atelier/mcp already exists
;;; with its own home symbols, introducing :use of atelier/mcp-kernel
;;; would conflict. We unintern the symbols from atelier/mcp first so
;;; they can be inherited from the kernel cleanly.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((mcp-pkg (find-package :atelier/mcp))
        (kernel-pkg (find-package :atelier/mcp-kernel)))
    (when (and mcp-pkg kernel-pkg
               (not (member kernel-pkg (package-use-list mcp-pkg))))
      ;; atelier/mcp exists but doesn't yet :use the kernel.
      ;; Unintern symbols that the kernel exports, so defpackage
      ;; can import them via :use without conflict.
      (do-external-symbols (sym kernel-pkg)
        (let ((existing (find-symbol (symbol-name sym) mcp-pkg)))
          (when (and existing (eq (symbol-package existing) mcp-pkg))
            (unintern existing mcp-pkg)))))))


;;;;
;;;; MCP Kernel — reusable MCP server framework
;;;;

(defpackage #:atelier/mcp-kernel
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
   ;; Image connection (abstract)
   #:image-connection
   #:connection-id
   #:connection-process-info
   #:connection-pid
   #:connection-eval
   #:connection-shutdown
   #:connection-alive-p
   ;; Server
   #:mcp-server
   #:server-stream
   #:server-child-connection
   #:server-connection-class
   #:server-connection-initargs
   #:server-max-children
   #:server-children
   #:ensure-child-connection
   #:*current-server*
   ;; Transcript protocol (generic — transport provides methods)
   #:write-transcript-entry))


;;;;
;;;; MCP — Atelier-specific transport (SWANK, transcript, tools)
;;;;

(defpackage #:atelier/mcp
  (:use #:common-lisp #:atelier/mcp-kernel)
  (:export
   ;; Re-export all kernel symbols
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
   ;; Image connection (abstract — from kernel)
   #:image-connection
   #:connection-id
   #:connection-process-info
   #:connection-pid
   #:connection-eval
   #:connection-shutdown
   #:connection-alive-p
   ;; Server (from kernel)
   #:mcp-server
   #:server-stream
   #:server-child-connection
   #:server-connection-class
   #:server-connection-initargs
   #:server-max-children
   #:server-children
   #:ensure-child-connection
   #:*current-server*
   ;; Transcript protocol (from kernel)
   #:write-transcript-entry
   ;; Transcript (mcp-specific)
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
   ;; Child connection (SWANK transport)
   #:child-connection
   #:make-child-connection
   #:child-connection-port
   #:child-connection-swank-conn
   #:child-image-spawn-failed
   ;; SWANK protocol client
   #:swank-connection
   #:swank-connect
   #:swank-disconnect
   #:swank-send
   #:swank-send-raw
   #:swank-receive
   #:swank-eval
   ;; Session child access
   #:server-child-connection
   #:ensure-child-connection
   ;; Debug state
   #:debug-state
   #:debug-state-condition
   #:debug-state-restarts
   #:debug-state-backtrace
   #:debug-state-level
   #:debug-state-thread
   #:connection-debug-state
   #:debugger-active
   ;; SWANK debug protocol
   #:swank-invoke-restart
   #:swank-backtrace-frames
   #:swank-eval-in-frame
   #:swank-interrupt))

;;;; End of file `package.lisp'
