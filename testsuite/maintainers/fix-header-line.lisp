;;;; fix-header-line.lisp — Tests for the header line maintainer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)

(define-testcase validate-fix-header-line-registered ()
  (assert-t (not (null (atelier:find-maintainer 'atelier:fix-header-line)))))

(define-testcase validate-fix-header-line ()
  "Verify fix-header-line produces a text-resolution with the correct canonical header."
  (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (write-string ";; wrong — Test description" s)
      (terpri s))
    (let* ((inspector (atelier:find-inspector 'atelier:check-header-line))
           (findings (atelier:inspect-file inspector p))
           (finding (first findings))
           (maintainer (atelier:find-maintainer 'atelier:fix-header-line))
           (resolution (atelier:prepare-resolution maintainer finding)))
      (assert-type resolution 'atelier:text-resolution)
      ;; The replacement should be a canonical header line with extracted description
      (let ((replacement (atelier:resolution-replacement resolution)))
        (assert-t (not (null (search ";;;;" replacement))))
        (assert-t (not (null (search (file-namestring p) replacement))))))))

(define-testcase testsuite-fix-header-line ()
  (validate-fix-header-line-registered)
  (validate-fix-header-line))

;;;; End of file `fix-header-line.lisp'
