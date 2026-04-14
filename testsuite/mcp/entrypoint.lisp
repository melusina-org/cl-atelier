;;;; entrypoint.lisp — Entry point for the Atelier MCP testsuite

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

(define-testcase run-mcp-tests ()
  "Top-level testcase that runs every MCP test."
  ;; Helper / macro tests (fast, pure)
  (validate-jzon-round-trip)
  (validate-derive-tool-name-from-symbol)
  (validate-derive-input-schema-from-lambda-list)
  (validate-uri-template-tests)
  (validate-define-tool-tests)
  (validate-parse-mcp-message)
  ;; Dispatcher and server tests (fast, in-memory)
  (validate-dispatcher-tests)
  (validate-handshake-tests)
  ;; Concrete tool and resource tests (fast, against live registry)
  (validate-tool-invocation-tests)
  (validate-resource-read-tests)
  (validate-registry-counts)
  ;; Abstract image-connection
  (validate-image-connection-tests)
  ;; Transcript
  (validate-transcript-encoding-tests)
  (validate-transcript-filesystem-tests)
  (validate-transcript-torn-write-tests)
  ;; SWANK protocol (fast, no child)
  (validate-swank-message-encoding)
  (validate-swank-message-decoding)
  ;; Canonicalize-form tool (fast, in-process)
  (validate-canonicalize-tool-basic)
  (validate-canonicalize-tool-earmuffs)
  (validate-canonicalize-tool-forbidden)
  (validate-canonicalize-tool-progn-decompose)
  (validate-canonicalize-tool-no-child)
  ;; Tool registration count (fast)
  (validate-tool-registration-count)
  ;; Child connection spawn failure (fast, no child)
  (validate-child-connection-spawn-failure)
  ;; All child-dependent test groups share one child (slice 015 S3).
  ;; Individual group runners no longer rebind *test-child* to nil.
  (let ((*test-child* nil))
    (unwind-protect
         (progn
           (run-child-dependent-tests)
           (run-debugger-tests)
           (validate-debugger-tool-count)
           (run-asdf-tools-tests)
           (validate-asdf-tool-count)
           (run-documentation-tools-tests)
           (run-xref-tools-tests)
           (validate-xref-tool-count)
           (run-inspect-trace-tests)
           (run-journey-tests))
      (when *test-child*
        (ignore-errors (atelier/mcp:connection-shutdown *test-child*))
        (setf *test-child* nil))))
  ;; HyperSpec tests (slow, filesystem — no child needed)
  (validate-hyperspec-lookup-known)
  (validate-hyperspec-lookup-unknown)
  (validate-hyperspec-symbol-resource)
  (validate-hyperspec-issue-known)
  (validate-hyperspec-issue-unknown)
  (validate-hyperspec-issues-list)
  (validate-hyperspec-issue-resource)
  ;; Documentation tool count (fast)
  (validate-documentation-tool-count)
  ;; Separate child tests (slow, spawns fresh SBCLs)
  (validate-child-connection-shutdown)
  (validate-run-tests-fresh)
  (validate-run-tests-fresh-load-failure)
  (validate-no-orphan-sbcl)
  ;; Fresh-SBCL subprocess (slow)
  (validate-mcp-system-loads-cleanly))

;;;; End of file `entrypoint.lisp'
