;;;; entry-point.lisp — Entrypoint for the Atelier test suite Lisp System

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/test)


;;;;
;;;; Fast Tests
;;;;
;;;; In-memory only — no filesystem, no subprocess, no temporary files.
;;;; These are safe to run on every change and in inner development loops.

(define-testcase run-fast-tests ()
  "Run every testsuite whose assertions stay in memory."
  (atelier:initialize)
  (testsuite-utilities)
  (testsuite-parameter)
  (testsuite-license)
  (testsuite-finding)
  (testsuite-resolution)
  (testsuite-inspector)
  (testsuite-maintainer)
  (testsuite-check-labels-for-flet)
  (testsuite-check-testsuite-package-name)
  (testsuite-fix-header-line)
  (testsuite-fix-footer-line)
  (testsuite-fix-labels-to-flet)
  (testsuite-fix-mixed-indentation)
  (validate-lint-fast))


;;;;
;;;; Slow Tests
;;;;
;;;; Touch the filesystem, spawn subprocesses, or read fixture files.
;;;; Run before commits and in CI. The editor suite is included here
;;;; because VALIDATE-CANONICALIZE-FIXTURES reads fixture files.

(define-testcase run-slow-tests ()
  "Run every testsuite that touches the filesystem or spawns a process."
  (atelier:initialize)
  (testsuite-runner)
  (testsuite-asdf)
  ;; Inspector tests not covered by fixture auto-discovery
  ;; (temporary files, synthetic inputs, helper unit tests, or shared
  ;; test/fixtures/ root files rather than test/fixtures/inspector/).
  (testsuite-check-file-encoding)
  (testsuite-check-spdx-license-header)
  (testsuite-check-trailing-whitespace)
  (testsuite-check-mixed-indentation)
  (testsuite-check-header-line)
  (testsuite-check-footer-line)
  (testsuite-check-project-identification)
  (testsuite-pretty-printer)
  (testsuite-write-back)
  (testsuite-autofix)
  (testsuite-template)
  ;; Slice 009: project structure and hooks
  (testsuite-check-system-naming)
  (testsuite-fix-deprecated-names)
  (testsuite-git)
  (validate-lint-slow)
  (atelier/test/editor:run-all-editor-tests))


;;;;
;;;; Idempotency
;;;;

(define-testcase validate-run-all-tests-is-idempotent ()
  "Two consecutive calls to RUN-ALL-TESTS in the same image must both succeed.
This guards against test-suite pollution via dangling ASDF registry entries
left behind by testcases that load systems from temporary directories."
  (run-all-tests)
  (run-all-tests))


;;;;
;;;; Aggregate Entry Point
;;;;

(define-testcase run-all-tests ()
  "Run the fast suite followed by the slow suite."
  (run-fast-tests)
  (run-slow-tests))

;;;; End of file `entry-point.lisp'
