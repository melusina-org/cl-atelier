;;;; autofix.lisp — Testsuite for lint-system :autofix integration

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)


;;;;
;;;; Autofix pipeline helpers
;;;;

(defun apply-autofix-to-file (pathname)
  "Run inspection on PATHNAME, resolve each finding using only production
maintainers (those in the ATELIER package), and apply all resolutions.
Returns the list of findings produced.
Note: test maintainers registered by other testcases are excluded to avoid
spurious overlapping-span errors."
  (let* ((atelier::*maintainers*
           ;; Shadow with a table containing only non-testsuite maintainers.
           (let ((clean (make-hash-table :test 'eq)))
             (maphash (lambda (name m)
                        (unless (eq (symbol-package name)
                                    (find-package :atelier/testsuite))
                          (setf (gethash name clean) m)))
                      atelier::*maintainers*)
             clean))
         (findings (atelier:perform-inspection pathname))
         (resolutions-by-file (make-hash-table :test 'equal)))
    (dolist (finding findings)
      (when (typep finding 'atelier:line-finding)
        (dolist (resolution (atelier:resolve-finding finding))
          (push resolution
                (gethash (atelier:finding-file finding) resolutions-by-file)))))
    (maphash (lambda (p resolutions)
               (atelier:apply-resolutions-to-file p (nreverse resolutions)))
             resolutions-by-file)
    findings))


;;;;
;;;; Testcases
;;;;

(define-testcase validate-lint-system-autofix ()
  "Verify that the autofix pipeline fixes source files.
Writes a file with trailing whitespace; after autofix the whitespace is gone."
  (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      ;; Line with 3 trailing spaces — check-trailing-whitespace will fire.
      (format s "(defvar *x* 1)   ~%"))
    (let ((findings (apply-autofix-to-file p)))
      ;; At least one finding should have been produced.
      (assert-t (not (null findings)))
      ;; After autofix, trailing whitespace must be gone.
      (assert-string= (format nil "(defvar *x* 1)~%")
                      (uiop:read-file-string p :external-format :utf-8)))))

(define-testcase validate-lint-system-no-autofix ()
  "Verify that inspection without autofix does not modify source files.
Writes a file with trailing whitespace; without autofix the file is unchanged."
  (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
    (let ((original (format nil "(defvar *x* 1)   ~%")))
      (with-open-file (s p :direction :output :if-exists :supersede
                         :external-format :utf-8)
        (write-string original s))
      (atelier:perform-inspection p)
      ;; File must be unchanged — no autofix applied.
      (assert-string= original
                      (uiop:read-file-string p :external-format :utf-8)))))

(define-testcase validate-lint-system-partial-autofix ()
  "Verify that only findings with registered maintainers are fixed.
Writes a file that will produce findings from both a fixable inspector
(check-trailing-whitespace) and a non-fixable one (check-line-length).
After autofix, trailing whitespace is removed but long lines remain."
  (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      ;; A very long line with trailing spaces: triggers both inspectors.
      (format s "(defun ~A () nil)   ~%"
              (make-string 100 :initial-element #\a)))
    (let* ((findings-before (apply-autofix-to-file p))
           (content-after (uiop:read-file-string p :external-format :utf-8)))
      ;; At least 2 findings (line-length + trailing-whitespace).
      (assert-t (>= (length findings-before) 2))
      ;; Trailing whitespace should be gone — the character immediately before
      ;; the first newline must not be a space.
      (let ((newline-pos (position #\Newline content-after)))
        (assert-t (and newline-pos
                       (not (char= #\Space (char content-after (1- newline-pos))))))))))


;;;;
;;;; Entry points
;;;;

(define-testcase validate-autofix ()
  "Run all autofix integration tests."
  (validate-lint-system-autofix)
  (validate-lint-system-no-autofix)
  (validate-lint-system-partial-autofix))

(define-testcase testsuite-autofix ()
  "Run all autofix tests."
  (validate-autofix))

;;;; End of file 'autofix.lisp'
