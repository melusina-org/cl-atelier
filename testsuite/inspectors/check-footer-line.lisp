;;;; check-footer-line.lisp — Tests for the canonical footer line inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)

(define-testcase validate-check-footer-line-registered ()
  (assert-t (not (null (member 'atelier:check-footer-line
                               (atelier:list-inspectors))))))

(define-testcase validate-check-footer-line-correct ()
  "Verify no finding for a file with a correct canonical footer."
  (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (format s "(in-package #:cl-user)~%~%;;;; End of file `~A'~%"
              (file-namestring p)))
    (let* ((inspector (atelier:find-inspector 'atelier:check-footer-line))
           (findings (atelier:inspect-file inspector p)))
      (assert-t (null findings)))))

(define-testcase validate-check-footer-line-violation ()
  "Verify finding for a file with wrong footer."
  (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (write-string "(in-package #:cl-user)" s)
      (terpri s))
    (let* ((inspector (atelier:find-inspector 'atelier:check-footer-line))
           (findings (atelier:inspect-file inspector p)))
      (assert-t (not (null findings)))
      (assert-t (typep (first findings) 'atelier:footer-line-finding)))))

(define-testcase testsuite-check-footer-line ()
  (validate-check-footer-line-registered)
  (validate-check-footer-line-correct)
  (validate-check-footer-line-violation))

;;;; End of file `check-footer-line.lisp'
