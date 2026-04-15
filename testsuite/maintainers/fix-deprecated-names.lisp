;;;; fix-deprecated-names.lisp — Tests for deprecated name maintainers

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)


;;;;
;;;; Helpers
;;;;

(defun inspect-and-fix-asd (content inspector-name maintainer-name)
  "Write CONTENT to a temp .asd file, run INSPECTOR-NAME, apply MAINTAINER-NAME
resolutions, and return (VALUES fixed-content findings resolutions)."
  (let ((inspector (atelier:find-inspector inspector-name))
        (maintainer (atelier:find-maintainer maintainer-name)))
    (uiop:call-with-temporary-file
     (lambda (stream pathname)
       (write-string content stream)
       (finish-output stream)
       (close stream)
       (let ((asd-path (make-pathname :name (pathname-name pathname)
                                      :type "asd"
                                      :defaults pathname)))
         (rename-file pathname asd-path)
         (unwind-protect
              (let* ((findings (atelier:inspect-file inspector asd-path))
                     (resolutions
                       (loop :for finding :in findings
                             :for resolution = (atelier:prepare-resolution
                                                maintainer finding)
                             :when resolution :collect resolution))
                     (fixed-content
                       (if resolutions
                           (atelier:apply-resolutions content resolutions)
                           content)))
                (values fixed-content findings resolutions))
           (when (probe-file asd-path)
             (delete-file asd-path)))))
     :keep t)))


;;;;
;;;; Deprecated System Name Maintainer Tests
;;;;

(define-testcase test-fix-deprecated-system-name ()
  "Verify that fix-deprecated-system-name renames /testsuite to /test."
  (let ((content "(defsystem \"my-project\"
  :components ((:file \"package\")))
(defsystem \"my-project/testsuite\"
  :components ((:file \"package\")))
"))
    (multiple-value-bind (fixed findings resolutions)
        (inspect-and-fix-asd content
                             'atelier:check-system-naming
                             'atelier:fix-deprecated-system-name)
      (assert-t (not (null findings)))
      (assert-t (not (null resolutions)))
      (assert-t (not (null (search "my-project/test" fixed))))
      (assert-nil (search "my-project/testsuite" fixed)))))

(define-testcase test-fix-deprecated-system-name-idempotent ()
  "Verify that fix-deprecated-system-name is self-idempotent at N=1."
  (let ((content "(defsystem \"my-project\"
  :components ((:file \"package\")))
(defsystem \"my-project/testsuite\"
  :components ((:file \"package\")))
"))
    (multiple-value-bind (fixed-1 findings-1)
        (inspect-and-fix-asd content
                             'atelier:check-system-naming
                             'atelier:fix-deprecated-system-name)
      (declare (ignore findings-1))
      ;; Now inspect the fixed content
      (multiple-value-bind (fixed-2 findings-2)
          (inspect-and-fix-asd fixed-1
                               'atelier:check-system-naming
                               'atelier:fix-deprecated-system-name)
        ;; No more deprecated system name findings
        (let ((deprecated-findings
                (remove-if-not (lambda (f)
                                 (typep f 'atelier:deprecated-system-name-finding))
                               findings-2)))
          (assert-nil deprecated-findings))
        ;; Content unchanged
        (assert-string= fixed-1 fixed-2)))))


;;;;
;;;; Deprecated Component Name Maintainer Tests
;;;;

(define-testcase test-fix-deprecated-component-name ()
  "Verify that fix-deprecated-component-name renames entrypoint to entry-point."
  (let ((content "(defsystem \"my-project\"
  :components ((:file \"package\")
               (:file \"entrypoint\")))
"))
    (multiple-value-bind (fixed findings resolutions)
        (inspect-and-fix-asd content
                             'atelier:check-system-naming
                             'atelier:fix-deprecated-component-name)
      (assert-t (not (null findings)))
      (assert-t (not (null resolutions)))
      (assert-t (not (null (search "entry-point" fixed))))
      (assert-nil (search "entrypoint" fixed)))))

(define-testcase test-fix-deprecated-component-name-idempotent ()
  "Verify that fix-deprecated-component-name is self-idempotent at N=1."
  (let ((content "(defsystem \"my-project\"
  :components ((:file \"package\")
               (:file \"entrypoint\")))
"))
    (multiple-value-bind (fixed-1 findings-1)
        (inspect-and-fix-asd content
                             'atelier:check-system-naming
                             'atelier:fix-deprecated-component-name)
      (declare (ignore findings-1))
      (multiple-value-bind (fixed-2 findings-2)
          (inspect-and-fix-asd fixed-1
                               'atelier:check-system-naming
                               'atelier:fix-deprecated-component-name)
        (let ((deprecated-findings
                (remove-if-not (lambda (f)
                                 (typep f 'atelier:deprecated-component-name-finding))
                               findings-2)))
          (assert-nil deprecated-findings))
        (assert-string= fixed-1 fixed-2)))))


;;;;
;;;; Entry Point
;;;;

(define-testcase testsuite-fix-deprecated-names ()
  "Run all deprecated name maintainer tests."
  (test-fix-deprecated-system-name)
  (test-fix-deprecated-system-name-idempotent)
  (test-fix-deprecated-component-name)
  (test-fix-deprecated-component-name-idempotent))

;;;; End of file `fix-deprecated-names.lisp'
