;;;; check-system-naming.lisp — Tests for system naming and test mirror inspectors

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)


;;;;
;;;; Helper
;;;;

(defun write-asd-content-and-inspect (content inspector-name)
  "Write CONTENT to a temporary .asd file and run INSPECTOR-NAME on it.
Return the list of findings."
  (let ((inspector (atelier:find-inspector inspector-name)))
    (uiop:call-with-temporary-file
     (lambda (stream pathname)
       (write-string content stream)
       (finish-output stream)
       (close stream)
       ;; Rename to .asd extension since inspectors check file type
       (let ((asd-path (make-pathname :name (pathname-name pathname)
                                      :type "asd"
                                      :defaults pathname)))
         (rename-file pathname asd-path)
         (unwind-protect
              (atelier:inspect-file inspector asd-path)
           (when (probe-file asd-path)
             (delete-file asd-path)))))
     :keep t)))


;;;;
;;;; System Naming Tests
;;;;

(define-testcase test-check-system-naming-canonical ()
  "Verify that canonical system names produce no findings."
  (let ((content "(defsystem \"my-project\"
  :components ((:file \"package\")))
(defsystem \"my-project/test\"
  :components ((:file \"package\")))
(defsystem \"my-project/development\"
  :components ((:file \"dev\")))
"))
    (let ((findings (write-asd-content-and-inspect content 'atelier:check-system-naming)))
      (assert-nil findings))))

(define-testcase test-check-system-naming-non-canonical ()
  "Verify that non-canonical system suffixes produce findings."
  (let ((content "(defsystem \"my-project\"
  :components ((:file \"package\")))
(defsystem \"my-project/foo\"
  :components ((:file \"package\")))
"))
    (let ((findings (write-asd-content-and-inspect content 'atelier:check-system-naming)))
      (assert-t (not (null findings)))
      (assert-type (first findings) 'atelier:non-canonical-system-name-finding))))

(define-testcase test-check-system-naming-deprecated ()
  "Verify that deprecated system names produce findings."
  (let ((content "(defsystem \"my-project\"
  :components ((:file \"package\")))
(defsystem \"my-project/testsuite\"
  :components ((:file \"package\")))
"))
    (let ((findings (write-asd-content-and-inspect content 'atelier:check-system-naming)))
      (assert-t (not (null findings)))
      (assert-type (first findings) 'atelier:deprecated-system-name-finding))))

(define-testcase test-check-component-naming-deprecated ()
  "Verify that deprecated component names produce findings."
  (let ((content "(defsystem \"my-project\"
  :components ((:file \"package\")
               (:file \"entrypoint\")))
"))
    (let ((findings (write-asd-content-and-inspect content 'atelier:check-system-naming)))
      (assert-t (not (null findings)))
      (assert-type (first findings) 'atelier:deprecated-component-name-finding))))

(define-testcase test-check-component-naming-clean ()
  "Verify that canonical component names produce no findings."
  (let ((content "(defsystem \"my-project\"
  :components ((:file \"package\")
               (:file \"entry-point\")
               (:file \"utilities\")))
"))
    (let ((findings (write-asd-content-and-inspect content 'atelier:check-system-naming)))
      ;; No deprecated component findings (may have system naming findings
      ;; if no test system — filter for component findings only)
      (let ((component-findings
              (remove-if-not (lambda (f)
                               (typep f 'atelier:deprecated-component-name-finding))
                             findings)))
        (assert-nil component-findings)))))


;;;;
;;;; Test Mirror Tests
;;;;

(define-testcase test-check-test-mirror-missing ()
  "Verify that missing test components produce findings."
  (let ((content "(defsystem \"my-project\"
  :components ((:file \"package\")
               (:file \"utilities\")
               (:file \"core\")))
(defsystem \"my-project/test\"
  :components ((:file \"package\")
               (:file \"utilities\")))
"))
    (let ((findings (write-asd-content-and-inspect content 'atelier:check-test-mirror)))
      (assert-t (not (null findings)))
      (assert-type (first findings) 'atelier:missing-test-component-finding))))

(define-testcase test-check-test-mirror-order ()
  "Verify that wrong component order produces findings."
  (let ((content "(defsystem \"my-project\"
  :components ((:file \"package\")
               (:file \"utilities\")
               (:file \"core\")))
(defsystem \"my-project/test\"
  :components ((:file \"package\")
               (:file \"core\")
               (:file \"utilities\")))
"))
    (let ((findings (write-asd-content-and-inspect content 'atelier:check-test-mirror)))
      (assert-t (not (null findings)))
      (assert-type (first findings) 'atelier:test-component-order-finding))))

(define-testcase test-check-test-mirror-clean ()
  "Verify that matching component structure produces no findings."
  (let ((content "(defsystem \"my-project\"
  :components ((:file \"package\")
               (:file \"utilities\")
               (:file \"core\")))
(defsystem \"my-project/test\"
  :components ((:file \"package\")
               (:file \"utilities\")
               (:file \"core\")))
"))
    (let ((findings (write-asd-content-and-inspect content 'atelier:check-test-mirror)))
      (assert-nil findings))))

(define-testcase test-check-test-mirror-excludes-config ()
  "Verify that non-mirror components (configuration) are excluded."
  (let ((content "(defsystem \"my-project\"
  :components ((:file \"package\")
               (:file \"project-configuration\")
               (:file \"linter-configuration\")
               (:file \"core\")))
(defsystem \"my-project/test\"
  :components ((:file \"package\")
               (:file \"core\")))
"))
    (let ((findings (write-asd-content-and-inspect content 'atelier:check-test-mirror)))
      (assert-nil findings))))


;;;;
;;;; Entry Point
;;;;

(define-testcase testsuite-check-system-naming ()
  "Run all system naming and test mirror inspector tests."
  (test-check-system-naming-canonical)
  (test-check-system-naming-non-canonical)
  (test-check-system-naming-deprecated)
  (test-check-component-naming-deprecated)
  (test-check-component-naming-clean)
  (test-check-test-mirror-missing)
  (test-check-test-mirror-order)
  (test-check-test-mirror-clean)
  (test-check-test-mirror-excludes-config))

;;;; End of file `check-system-naming.lisp'
