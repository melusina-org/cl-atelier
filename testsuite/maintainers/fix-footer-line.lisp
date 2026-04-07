;;;; fix-footer-line.lisp — Tests for the footer line maintainer

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

(define-testcase validate-fix-footer-line-registered ()
  (assert-t (not (null (atelier:find-maintainer 'atelier:fix-footer-line)))))

(define-testcase validate-fix-footer-line ()
  "Verify fix-footer-line produces a text-resolution with the correct canonical footer."
  (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (write-string "(in-package #:cl-user)" s)
      (terpri s))
    (let* ((inspector (atelier:find-inspector 'atelier:check-footer-line))
           (findings (atelier:inspect-file inspector p))
           (finding (first findings))
           (maintainer (atelier:find-maintainer 'atelier:fix-footer-line))
           (resolution (atelier:prepare-resolution maintainer finding)))
      (assert-type resolution 'atelier:text-resolution)
      (let ((replacement (atelier:resolution-replacement resolution)))
        (assert-t (not (null (search "End of file" replacement))))
        (assert-t (not (null (search (file-namestring p) replacement))))))))

(define-testcase testsuite-fix-footer-line ()
  (validate-fix-footer-line-registered)
  (validate-fix-footer-line))

;;;; End of file `fix-footer-line.lisp'
