;;;; check-header-line.lisp — Tests for the canonical header line inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:atelier/testsuite)

(define-testcase validate-check-header-line-registered ()
  (assert-t (not (null (member 'atelier:check-header-line
                               (atelier:list-inspectors))))))

(define-testcase validate-check-header-line-correct ()
  "Verify no finding for a file with a correct canonical header."
  (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (format s ";;;; ~A — Test fixture~%(in-package #:cl-user)~%"
              (file-namestring p)))
    (let* ((inspector (atelier:find-inspector 'atelier:check-header-line))
           (findings (atelier:inspect-file inspector p)))
      (assert-t (null findings)))))

(define-testcase validate-check-header-line-violation ()
  "Verify finding for a file with wrong header."
  (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (write-string ";; wrong header" s)
      (terpri s))
    (let* ((inspector (atelier:find-inspector 'atelier:check-header-line))
           (findings (atelier:inspect-file inspector p)))
      (assert-t (not (null findings)))
      (assert-t (typep (first findings) 'atelier:header-line-finding)))))

(define-testcase validate-check-header-line-non-lisp-skipped ()
  "Verify no finding for a non-Lisp file."
  (uiop:with-temporary-file (:pathname p :type "txt" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (write-string "not a lisp file" s))
    (let* ((inspector (atelier:find-inspector 'atelier:check-header-line))
           (findings (atelier:inspect-file inspector p)))
      (assert-t (null findings)))))

(define-testcase testsuite-check-header-line ()
  (validate-check-header-line-registered)
  (validate-check-header-line-correct)
  (validate-check-header-line-violation)
  (validate-check-header-line-non-lisp-skipped))

;;;; End of file `check-header-line.lisp'
