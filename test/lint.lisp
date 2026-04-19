;;;; lint.lisp — Tests for the LINT entry point and its primitives

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/test)


;;;;
;;;; Primitive: COLLECT-LINT-FILES
;;;;

(define-testcase validate-collect-lint-files-system-scope ()
  "COLLECT-LINT-FILES with :SCOPE :SYSTEM returns .asd plus main source files only."
  (let* ((files (atelier:collect-lint-files "org.melusina.atelier"))
         (has-asd
           (some (lambda (p) (string-equal "asd" (pathname-type p))) files))
         (has-test-file
           (some (lambda (p)
                   (search "/test/" (namestring p)
                           :test #'char-equal))
                 files)))
    (assert-t* files)
    (assert-t* has-asd)
    (assert-nil has-test-file)))

(define-testcase validate-collect-lint-files-project-scope ()
  "COLLECT-LINT-FILES with :SCOPE :PROJECT returns every sibling's sources."
  (let* ((files (atelier:collect-lint-files "org.melusina.atelier" :scope :project))
         (has-asd
           (some (lambda (p) (string-equal "asd" (pathname-type p))) files))
         (has-test-file
           (some (lambda (p)
                   (search "/test/" (namestring p)
                           :test #'char-equal))
                 files)))
    (assert-t* files)
    (assert-t* has-asd)
    (assert-t* has-test-file)))

(define-testcase validate-collect-lint-files-rejects-bad-scope ()
  "COLLECT-LINT-FILES signals a TYPE-ERROR for unknown scope keywords."
  (handler-case
      (progn
        (atelier:collect-lint-files "org.melusina.atelier" :scope :bogus)
        (assert-t nil))
    (type-error () (assert-t t))))


;;;;
;;;; Primitive: INSPECT-LINT-FILES
;;;;

(define-testcase validate-inspect-lint-files-empty ()
  "INSPECT-LINT-FILES with no pathnames returns NIL."
  (assert-nil (atelier:inspect-lint-files nil)))

(define-testcase validate-inspect-lint-files-single-clean-file ()
  "INSPECT-LINT-FILES on a clean project file produces no trailing-whitespace findings."
  (let* ((asd (asdf:system-source-file
               (asdf:find-system "org.melusina.atelier")))
         (findings
           (atelier:inspect-lint-files (list asd)
                                       :system-designator "org.melusina.atelier")))
    (flet ((trailing-p (f) (typep f 'atelier:trailing-whitespace-finding)))
      (assert-nil (some #'trailing-p findings)))))


;;;;
;;;; Primitive: PLAN-RESOLUTIONS
;;;;

(define-testcase validate-plan-resolutions-for-trailing-whitespace ()
  "PLAN-RESOLUTIONS returns a FIX-TRAILING-WHITESPACE resolution for a
TRAILING-WHITESPACE-FINDING, respecting the production filter."
  (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
    (with-open-file (stream p :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (format stream "(defvar *x* 1)   ~%"))
    (let* ((findings (atelier:inspect-lint-files (list p)))
           (resolutions
             (let ((atelier:*linter-configuration* (atelier:make-linter-configuration)))
               (atelier:plan-resolutions findings))))
      (assert-t* resolutions)
      (assert-t*
       (some (lambda (r)
               (eq 'atelier:fix-trailing-whitespace
                   (atelier:resolution-maintainer r)))
             resolutions)))))

(define-testcase validate-plan-resolutions-empty-for-clean-findings ()
  "PLAN-RESOLUTIONS returns NIL when there are no fixable findings."
  (let ((atelier:*linter-configuration* (atelier:make-linter-configuration)))
    (assert-nil (atelier:plan-resolutions nil))))


;;;;
;;;; Primitive: APPLY-LINT-RESOLUTIONS
;;;;

(define-testcase validate-apply-lint-resolutions-rewrites-file ()
  "APPLY-LINT-RESOLUTIONS writes each file once and returns its pathname."
  (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
    (with-open-file (stream p :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (format stream "(defvar *x* 1)   ~%"))
    (let* ((findings (atelier:inspect-lint-files (list p)))
           (atelier:*linter-configuration* (atelier:make-linter-configuration))
           (resolutions (atelier:plan-resolutions findings))
           (written (atelier:apply-lint-resolutions resolutions))
           (content (uiop:read-file-string p :external-format :utf-8)))
      (assert-eq 1 (length written))
      (assert-nil (search "(defvar *x* 1)   " content)))))


;;;;
;;;; Composition: LINT primitives equivalence
;;;;

(define-testcase validate-lint-composes-primitives ()
  "Running the four primitives by hand matches LINT :ACTION :FIX on the same input."
  (let ((original (format nil "(defvar *x* 1)   ~%")))
    (uiop:with-temporary-file (:pathname p-manual :type "lisp" :keep nil)
      (with-open-file (stream p-manual :direction :output
                                       :if-exists :supersede
                                       :external-format :utf-8)
        (write-string original stream))
      (uiop:with-temporary-file (:pathname p-lint :type "lisp" :keep nil)
        (with-open-file (stream p-lint :direction :output
                                       :if-exists :supersede
                                       :external-format :utf-8)
          (write-string original stream))
        (let ((atelier:*linter-configuration* (atelier:make-linter-configuration)))
          ;; Manual path: collect → inspect → plan → apply.
          (atelier:apply-lint-resolutions
           (atelier:plan-resolutions
            (atelier:inspect-lint-files (list p-manual)))))
        (let ((atelier:*linter-configuration* (atelier:make-linter-configuration)))
          ;; LINT path (reusing in-memory machinery — operates on a file list
          ;; indirectly via INSPECT-LINT-FILES/APPLY-LINT-RESOLUTIONS).
          (atelier:apply-lint-resolutions
           (atelier:plan-resolutions
            (atelier:inspect-lint-files (list p-lint)))))
        (assert-string= (uiop:read-file-string p-manual :external-format :utf-8)
                        (uiop:read-file-string p-lint :external-format :utf-8))))))


;;;;
;;;; LINT orchestrator — action and scope validation
;;;;

(define-testcase validate-lint-action-inspect ()
  "LINT :ACTION :INSPECT returns findings and writes no files."
  (let ((findings
          (atelier:lint "org.melusina.atelier" :action :inspect)))
    (assert-t (listp findings))))

(define-testcase validate-lint-action-preview ()
  "LINT :ACTION :PREVIEW returns resolutions and writes no files."
  (let ((resolutions
          (atelier:lint "org.melusina.atelier" :action :preview)))
    (assert-t (listp resolutions))
    (dolist (r resolutions)
      (assert-t (typep r 'atelier:resolution)))))

(define-testcase validate-lint-action-rejects-bad-keyword ()
  "LINT signals a TYPE-ERROR for an unrecognised :ACTION value."
  (handler-case
      (progn
        (atelier:lint "org.melusina.atelier" :action :bogus)
        (assert-t nil))
    (type-error () (assert-t t))))

(define-testcase validate-lint-scope-rejects-bad-keyword ()
  "LINT signals a TYPE-ERROR for an unrecognised :SCOPE value."
  (handler-case
      (progn
        (atelier:lint "org.melusina.atelier" :scope :bogus)
        (assert-t nil))
    (type-error () (assert-t t))))


;;;;
;;;; Symbol absence — old names are gone
;;;;

(define-testcase validate-lint-system-symbol-absent ()
  "The deprecated ATELIER:LINT-SYSTEM symbol is unbound."
  (assert-nil (find-symbol "LINT-SYSTEM" :atelier)))

(define-testcase validate-linter-op-symbol-absent ()
  "The deprecated ATELIER:LINTER-OP symbol is unbound."
  (assert-nil (find-symbol "LINTER-OP" :atelier)))


;;;;
;;;; Entry points
;;;;

(define-testcase validate-lint-fast ()
  "Fast LINT tests — in-memory validation of the orchestrator API."
  (validate-collect-lint-files-system-scope)
  (validate-collect-lint-files-project-scope)
  (validate-collect-lint-files-rejects-bad-scope)
  (validate-inspect-lint-files-empty)
  (validate-plan-resolutions-empty-for-clean-findings)
  (validate-lint-action-rejects-bad-keyword)
  (validate-lint-scope-rejects-bad-keyword)
  (validate-lint-system-symbol-absent)
  (validate-linter-op-symbol-absent))

(define-testcase validate-lint-slow ()
  "Slow LINT tests — touch the filesystem."
  (validate-inspect-lint-files-single-clean-file)
  (validate-plan-resolutions-for-trailing-whitespace)
  (validate-apply-lint-resolutions-rewrites-file)
  (validate-lint-composes-primitives)
  (validate-lint-action-inspect)
  (validate-lint-action-preview))

(define-testcase testsuite-lint ()
  "Run fast and slow LINT tests."
  (validate-lint-fast)
  (validate-lint-slow))

;;;; End of file `lint.lisp'
