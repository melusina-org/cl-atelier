;;;; entry-point.lisp — Entry point for the Atelier editor testsuite

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/editor)

(define-testcase run-all-editor-tests ()
  "Run all editor tests."
  ;; Eclector capability probe (T26, T27)
  (validate-eclector-preserves-feature-expression)
  (validate-eclector-cst-to-text-round-trip)
  ;; lint-string (T1–T4)
  (validate-lint-string-earmuffs)
  (validate-lint-string-clean-form)
  (validate-lint-string-syntax-only)
  (validate-lint-string-line-and-syntax)
  ;; toplevel-form record (T8–T17)
  (validate-make-toplevel-form)
  (validate-read-simple-defun)
  (validate-read-peels-eval-when)
  (validate-read-preserves-feature-conditional)
  (validate-ast-default-features)
  (validate-ast-overridden-features)
  (validate-read-unknown-macro-name)
  (validate-read-progn-signals)
  (validate-read-progn-decompose-restart)
  (validate-read-side-effect-signals)
  ;; write path (T21–T25)
  (validate-write-simple-defun)
  (validate-write-preserves-feature-conditional)
  (validate-write-wraps-eval-when)
  (validate-write-elides-default-eval-when)
  (validate-round-trip-fixed-point)
  ;; normalize pipeline (T18–T20)
  (validate-normalize-earmuffs)
  (validate-normalize-returns-findings)
  (validate-normalize-idempotent)
  (validate-normalize-preserves-feature-guards)
  (validate-normalize-preserves-body-feature-branches)
  ;; auto-discovered fixtures (T28–T40+)
  (validate-canonicalize-fixtures))

;;;; End of file `entry-point.lisp'
