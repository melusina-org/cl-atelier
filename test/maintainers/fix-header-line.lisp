;;;; fix-header-line.lisp — Tests for the header line maintainer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)

(define-testcase validate-fix-header-line ()
  "Verify fix-header-line produces a text-resolution with the correct canonical header.
Constructs a finding directly (no file I/O needed)."
  (let* ((finding (make-instance 'atelier:header-line-finding
                   :inspector 'atelier:check-header-line
                   :severity :style
                   :observation "Wrong header."
                   :rationale "Expected canonical header."
                   :file #p"example.lisp"
                   :line 1 :column 0 :end-line 1 :end-column 16
                   :source-text ";; wrong — Test"))
         (maintainer (atelier:find-maintainer 'atelier:fix-header-line))
         (resolution (atelier:prepare-resolution maintainer finding)))
    (assert-type resolution 'atelier:text-resolution)
    (let ((replacement (atelier:resolution-replacement resolution)))
      (assert-t (not (null (search ";;;;" replacement))))
      (assert-t (not (null (search "example.lisp" replacement))))
      (assert-t (not (null (search "Test" replacement)))))))

(define-testcase testsuite-fix-header-line ()
  (validate-fix-header-line))

;;;; End of file `fix-header-line.lisp'
