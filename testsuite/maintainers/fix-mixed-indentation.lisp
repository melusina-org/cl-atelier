;;;; fix-mixed-indentation.lisp — Testsuite for the mixed indentation maintainer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)

(define-testcase validate-fix-mixed-indentation ()
  "Verify fix-mixed-indentation produces a text-resolution replacing the tab with spaces.
Uses a synthetic finding where column 0 to 1 is a tab (style :spaces expected)."
  (let* ((finding
           (make-instance 'atelier:mixed-indentation-finding
            :inspector 'atelier:check-mixed-indentation
            :severity :style
            :observation "Line 1 uses tabs for indentation (expected spaces)."
            :rationale "Consistent indentation prevents alignment issues."
            :file #p"/tmp/test.lisp"
            :line 1
            :column 0
            :end-line 1
            :end-column 1
            :source-text (format nil "~C(defvar *x* 1)" #\Tab)))
         (maintainer (atelier:find-maintainer 'atelier:fix-mixed-indentation))
         (resolution (atelier:prepare-resolution maintainer finding)))
    (assert-type resolution 'atelier:text-resolution)
    ;; Default style is :spaces → tab is replaced with 2 spaces.
    (assert-string= "  " (atelier:resolution-replacement resolution))
    (assert-eq 'atelier:fix-mixed-indentation
               (atelier:resolution-maintainer resolution))))

(define-testcase testsuite-fix-mixed-indentation ()
  "Run all fix-mixed-indentation tests."
  (validate-fix-mixed-indentation))

;;;; End of file 'fix-mixed-indentation.lisp'
