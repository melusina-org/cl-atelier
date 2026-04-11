;;;; entrypoint.lisp — Entrypoint for the Atelier test suite Lisp System

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)

(define-testcase run-all-tests ()
  (atelier:initialize)
  (testsuite-utilities)
  (testsuite-parameter)
  (testsuite-license)
  (testsuite-finding)
  (testsuite-resolution)
  (testsuite-inspector)
  (testsuite-maintainer)
  (testsuite-runner)
  (testsuite-asdf)
  ;; Inspector tests not covered by fixture auto-discovery
  ;; (temporary files, synthetic inputs, helper unit tests, or shared
  ;; testsuite/fixtures/ root files rather than testsuite/fixtures/inspector/).
  (testsuite-check-file-encoding)
  (testsuite-check-spdx-license-header)
  (testsuite-check-trailing-whitespace)
  (testsuite-check-mixed-indentation)
  (testsuite-check-labels-for-flet)
  (testsuite-check-header-line)
  (testsuite-check-footer-line)
  (testsuite-check-project-identification)
  (testsuite-pretty-printer)
  (testsuite-write-back)
  ;; Maintainer tests not covered by autofix-cycle fixture auto-discovery.
  ;; fix-mixed-indentation is tested ad-hoc (see slice 007 rationale);
  ;; fix-header-line and fix-footer-line assert generated replacement content
  ;; on synthetic findings; fix-labels-to-flet keeps helper unit tests
  ;; and an end-to-end pipeline test because no autofix-cycle fixture exists
  ;; for it yet.
  (testsuite-fix-mixed-indentation)
  (testsuite-fix-labels-to-flet)
  (testsuite-fix-header-line)
  (testsuite-fix-footer-line)
  (testsuite-autofix)
  (testsuite-template)
  (atelier/testsuite/mcp:run-mcp-tests))

;;;; End of file `entrypoint.lisp'
