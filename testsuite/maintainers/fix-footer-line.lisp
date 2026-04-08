;;;; fix-footer-line.lisp — Tests for the footer line maintainer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)

(define-testcase validate-fix-footer-line-registered ()
  (assert-t (not (null (atelier:find-maintainer 'atelier:fix-footer-line)))))

(define-testcase validate-fix-footer-line ()
  "Verify fix-footer-line produces a text-resolution with the correct canonical footer.
Constructs a finding directly (no file I/O needed)."
  (let* ((finding (make-instance 'atelier:footer-line-finding
                   :inspector 'atelier:check-footer-line
                   :severity :style
                   :observation "Wrong footer."
                   :rationale "Expected canonical footer."
                   :file #p"example.lisp"
                   :line 10 :column 0 :end-line 10 :end-column 30
                   :source-text ";;;; End of file 'example.lisp'"))
         (maintainer (atelier:find-maintainer 'atelier:fix-footer-line))
         (resolution (atelier:prepare-resolution maintainer finding)))
    (assert-type resolution 'atelier:text-resolution)
    (let ((replacement (atelier:resolution-replacement resolution)))
      (assert-t (not (null (search "End of file" replacement))))
      (assert-t (not (null (search "example.lisp" replacement))))
      ;; Must use backtick, not straight quote
      (assert-t (not (null (search "`" replacement)))))))

(define-testcase testsuite-fix-footer-line ()
  (validate-fix-footer-line-registered)
  (validate-fix-footer-line))

;;;; End of file `fix-footer-line.lisp'
