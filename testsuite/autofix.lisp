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
Returns the list of findings produced."
  (let* ((atelier::*maintainers*
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
;;;; Slow tests — file-based autofix integration
;;;;

(define-testcase validate-lint-system-autofix ()
  "Verify that the autofix pipeline fixes source files."
  (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (format s "(defvar *x* 1)   ~%"))
    (let ((findings (apply-autofix-to-file p)))
      (assert-t (not (null findings)))
      (assert-string= (format nil "(defvar *x* 1)~%")
                      (uiop:read-file-string p :external-format :utf-8)))))

(define-testcase validate-lint-system-no-autofix ()
  "Verify that inspection without autofix does not modify source files."
  (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
    (let ((original (format nil "(defvar *x* 1)   ~%")))
      (with-open-file (s p :direction :output :if-exists :supersede
                         :external-format :utf-8)
        (write-string original s))
      (atelier:perform-inspection p)
      (assert-string= original
                      (uiop:read-file-string p :external-format :utf-8)))))

(define-testcase validate-lint-system-partial-autofix ()
  "Verify that only findings with registered maintainers are fixed."
  (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (format s "(defun ~A () nil)   ~%"
              (make-string 100 :initial-element #\a)))
    (let* ((findings-before (apply-autofix-to-file p))
           (content-after (uiop:read-file-string p :external-format :utf-8)))
      (assert-t (>= (length findings-before) 2))
      (let ((newline-pos (position #\Newline content-after)))
        (assert-t (and newline-pos
                       (not (char= #\Space (char content-after (1- newline-pos))))))))))


;;;;
;;;; Fast tests — fixture auto-discovery for maintainers
;;;;

(define-testcase validate-one-maintainer-fixture (maintainer-name
                                                  &optional (fixture-name "baseline"))
  "Validate a single maintainer fixture. MAINTAINER-NAME is a symbol like
ATELIER:FIX-BARE-LAMBDA. FIXTURE-NAME defaults to \"baseline\".

Example: (validate-one-maintainer-fixture 'atelier:fix-constant-naming)"
  (let ((fixture-path (maintainer-fixture maintainer-name fixture-name)))
    (unless (probe-file fixture-path)
      (format t "~&SKIP ~A/~A: ~A not found.~%" maintainer-name fixture-name fixture-path)
      (return-from validate-one-maintainer-fixture nil))
    (multiple-value-bind (inspector-name input-form expected-form)
        (read-maintainer-fixture fixture-path)
      (declare (ignore input-form))
      (when inspector-name
        (let* ((source (atelier:join-lines
                        (first (nth-value 1
                                 (atelier:read-file-documents-with-yaml-front-matter
                                  fixture-path)))))
               (cst-forms (atelier:parse-common-lisp source))
               (top-form (first cst-forms))
               (line-vector (atelier:string-to-line-vector source))
               (inspector (atelier:find-inspector inspector-name))
               (maintainer (atelier:find-maintainer maintainer-name)))
          (when (and inspector maintainer top-form)
            (let* ((findings
                     (let ((atelier::*current-pathname* #p"fixture.lisp")
                           (atelier::*current-line-vector* line-vector)
                           (atelier::*current-cst-root* top-form))
                       (atelier:inspect-syntax inspector top-form)))
                   (resolutions
                     (let ((atelier::*current-line-vector* line-vector))
                       (loop :for finding :in findings
                             :for resolution = (atelier:prepare-resolution
                                                maintainer finding)
                             :when resolution :collect resolution))))
              (when resolutions
                (let* ((result-string
                         (atelier:apply-resolutions source resolutions))
                       (result-form (read-from-string result-string)))
                  (assert-t (equal expected-form result-form)))))))))))

(define-testcase validate-maintainer-fixtures ()
  "Run every discovered maintainer fixture as a transform test."
  (dolist (entry (discover-maintainer-fixtures))
    (let ((maintainer-name (car entry)))
      (dolist (fixture-path (cdr entry))
        (let ((fixture-name (pathname-name fixture-path)))
          (validate-one-maintainer-fixture maintainer-name fixture-name))))))


;;;;
;;;; Fast tests — fixture auto-discovery for inspectors
;;;;

(define-testcase validate-one-inspector-fixture (inspector-name
                                                 &optional (fixture-name "baseline"))
  "Validate a single inspector fixture. INSPECTOR-NAME is a symbol like
ATELIER:CHECK-EARMUFFS. FIXTURE-NAME defaults to \"baseline\".
Files named \"good\" or \"clean\" should produce no findings.
Files named \"bad\", \"violation\", or anything else should produce findings.

Example: (validate-one-inspector-fixture 'atelier:check-earmuffs \"bad\")"
  (let ((fixture-path (inspector-fixture inspector-name fixture-name)))
    (unless (probe-file fixture-path)
      (format t "~&SKIP ~A/~A: ~A not found.~%" inspector-name fixture-name fixture-path)
      (return-from validate-one-inspector-fixture nil))
    (let* ((inspector (atelier:find-inspector inspector-name))
           (findings (when inspector (atelier:inspect-file inspector fixture-path)))
           (expect-clean (member fixture-name '("good" "clean" "correct" "necessary")
                                 :test #'string-equal)))
      (if expect-clean
          (assert-t (null findings))
          (assert-t (not (null findings)))))))

(define-testcase validate-inspector-fixtures ()
  "Run every discovered inspector fixture."
  (dolist (entry (discover-inspector-fixtures))
    (let ((inspector-name (car entry)))
      (dolist (fixture-path (cdr entry))
        (let ((fixture-name (pathname-name fixture-path)))
          (validate-one-inspector-fixture inspector-name fixture-name))))))


;;;;
;;;; Fast tests — fixture auto-discovery for pretty-printer
;;;;

(define-testcase validate-one-pretty-printer-fixture (fixture-name)
  "Validate a single pretty-printer fixture. FIXTURE-NAME is a string like
\"flet-single-binding\".

Example: (validate-one-pretty-printer-fixture \"flet-single-binding\")"
  (let ((fixture-path (pretty-printer-fixture fixture-name)))
    (unless (probe-file fixture-path)
      (format t "~&SKIP pretty-printer/~A: ~A not found.~%" fixture-name fixture-path)
      (return-from validate-one-pretty-printer-fixture nil))
    (multiple-value-bind (front-matter form column right-margins expected-strings)
        (read-pretty-print-fixture fixture-path)
      (declare (ignore front-matter))
      (loop :for right-margin :in right-margins
            :for expected :in expected-strings
            :for actual = (atelier:pretty-print-form form column
                                                     :right-margin right-margin)
            :do (assert-string= expected actual)))))

(define-testcase validate-pretty-printer-fixtures ()
  "Run every discovered pretty-printer fixture."
  (dolist (pathname (discover-pretty-printer-fixtures))
    (validate-one-pretty-printer-fixture (pathname-name pathname))))


;;;;
;;;; Entry points
;;;;

(define-testcase validate-autofix ()
  "Run all autofix integration tests (slow — file I/O)."
  (validate-lint-system-autofix)
  (validate-lint-system-no-autofix)
  (validate-lint-system-partial-autofix))

(define-testcase testsuite-autofix ()
  "Run all autofix and fixture-discovery tests."
  (validate-autofix)
  (validate-maintainer-fixtures)
  (validate-inspector-fixtures)
  (validate-pretty-printer-fixtures))

;;;; End of file `autofix.lisp'
