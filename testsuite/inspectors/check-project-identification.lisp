;;;; check-project-identification.lisp — Tests for the project identification inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)

(define-testcase validate-check-project-identification-registered ()
  (assert-t (not (null (member 'atelier:check-project-identification
                               (atelier:list-inspectors))))))

(define-testcase validate-check-project-identification-correct ()
  "Verify no finding for a file with correct project identification."
  (let ((atelier::*project-configuration*
          (atelier:make-project-configuration
           :project-name "Example"
           :homepage "https://example.com"
           :copyright-year "2025"
           :copyright-holder "A. U. Thor")))
    (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
      (with-open-file (s p :direction :output :if-exists :supersede
                         :external-format :utf-8)
        (format s ";;;; ~A — Test~%~%" (file-namestring p))
        (format s ";;;; Example (https://example.com)~%")
        (format s ";;;; This file is part of Example.~%")
        (format s ";;;;~%")
        (format s ";;;; Copyright © 2025 A. U. Thor~%")
        (format s ";;;; All rights reserved.~%"))
      (let* ((inspector (atelier:find-inspector 'atelier:check-project-identification))
             (findings (atelier:inspect-file inspector p)))
        (assert-t (null findings))))))

(define-testcase validate-check-project-identification-missing ()
  "Verify finding for a file with no project identification."
  (let ((atelier::*project-configuration*
          (atelier:make-project-configuration
           :project-name "Example"
           :homepage "https://example.com"
           :copyright-year "2025"
           :copyright-holder "A. U. Thor")))
    (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
      (with-open-file (s p :direction :output :if-exists :supersede
                         :external-format :utf-8)
        (format s "(in-package #:cl-user)~%"))
      (let* ((inspector (atelier:find-inspector 'atelier:check-project-identification))
             (findings (atelier:inspect-file inspector p)))
        (assert-t (not (null findings)))))))

(define-testcase validate-check-project-identification-no-config ()
  "Verify no finding when project configuration is absent."
  (let ((atelier::*project-configuration* nil))
    (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
      (with-open-file (s p :direction :output :if-exists :supersede
                         :external-format :utf-8)
        (format s "(in-package #:cl-user)~%"))
      (let* ((inspector (atelier:find-inspector 'atelier:check-project-identification))
             (findings (atelier:inspect-file inspector p)))
        (assert-t (null findings))))))

(define-testcase testsuite-check-project-identification ()
  (validate-check-project-identification-registered)
  (validate-check-project-identification-correct)
  (validate-check-project-identification-missing)
  (validate-check-project-identification-no-config))

;;;; End of file `check-project-identification.lisp'
