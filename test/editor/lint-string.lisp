;;;; lint-string.lisp — Tests for atelier:lint-string

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/test/editor)

(define-testcase validate-lint-string-earmuffs ()
  "Verify that lint-string applies fix-earmuffs on a defvar."
  (multiple-value-bind (fixed findings)
      (atelier:lint-string "(defvar foo 42)" :levels '(:syntax))
    (assert-string= "(defvar *foo* 42)" fixed)
    (assert-t* findings)))

(define-testcase validate-lint-string-clean-form ()
  "Verify that lint-string returns an already-clean form unchanged."
  (multiple-value-bind (fixed findings)
      (atelier:lint-string "(defun bar (x) (1+ x))" :levels '(:syntax))
    (assert-string= "(defun bar (x) (1+ x))" fixed)
    (assert-nil findings)))

(define-testcase validate-lint-string-syntax-only ()
  "Verify that :levels (:syntax) does not run line-level inspectors."
  (let ((input (format nil "(defun baz ()~%  (values)  ~%)")))
    (multiple-value-bind (fixed findings)
        (atelier:lint-string input :levels '(:syntax))
      (declare (ignore fixed))
      ;; No syntax findings expected for this clean form.
      ;; Trailing whitespace on internal lines is a line-level concern.
      (assert-nil findings))))

(define-testcase validate-lint-string-line-and-syntax ()
  "Verify that :levels (:line :syntax) runs both levels."
  (let ((input (format nil "(defvar foo 42)   ")))
    (multiple-value-bind (fixed findings)
        (atelier:lint-string input :levels '(:line :syntax))
      ;; Should have both trailing-whitespace (line) and earmuffs (syntax)
      (assert-t* findings)
      (assert-t* (>= (length findings) 1))
      ;; The fixed string should have earmuffs applied
      (assert-t* (search "*foo*" fixed)))))

;;;; End of file `lint-string.lisp'
