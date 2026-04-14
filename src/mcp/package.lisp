;;;; package.lisp — Packages for MCP kernel and MCP server

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT


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

;;; When reloading in a live image where atelier/mcp predates the kernel
;;; split, the symbols listed in :import-from already exist as home
;;; symbols in atelier/mcp. We unintern them so import-from can bring
;;; in the kernel's symbols cleanly.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((mcp-pkg (find-package :atelier/mcp))
        (kernel-pkg (find-package :atelier/mcp-kernel)))
    (when (and mcp-pkg kernel-pkg)
      (do-external-symbols (sym kernel-pkg)
        (let ((existing (find-symbol (symbol-name sym) mcp-pkg)))
          (when (and existing
                     (eq (symbol-package existing) mcp-pkg)
                     (not (eq existing sym)))
            (unintern existing mcp-pkg)))))))

(defpackage #:atelier/mcp
  (:use #:common-lisp)
  (:import-from #:atelier/mcp-kernel
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
   ;; Transcript protocol
   #:write-transcript-entry)
  (:export
   ;; Re-export kernel symbols
   #:+mcp-protocol-version+
   #:serve-two-way-stream
   #:mcp-error
   #:tool-not-found
   #:resource-not-found
   #:not-implemented
   #:invalid-tool-arguments
   #:invalid-uri-template
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
   #:derive-tool-name-from-symbol
   #:derive-input-schema-from-lambda-list
   #:parse-uri-template
   #:match-uri-against-template
   #:validate-uri-template-against-lambda-list
   #:uri-template-static-p
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
   #:define-tool
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
   #:image-connection
   #:connection-id
   #:connection-process-info
   #:connection-pid
   #:connection-eval
   #:connection-shutdown
   #:connection-alive-p
   #:mcp-server
   #:server-stream
   #:server-child-connection
   #:server-connection-class
   #:server-connection-initargs
   #:server-max-children
   #:server-children
   #:ensure-child-connection
   #:*current-server*
   #:write-transcript-entry
   ;; Transcript (mcp-specific)
   #:transcript
   #:make-transcript
   #:transcript-session-id
   #:transcript-path
   #:transcript-next-seq
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
   #:swank-interrupt
   ;; Binary entry point
   #:serve))

;;;; End of file `package.lisp'
