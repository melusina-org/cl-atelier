;;;; fix-project-identification.lisp — Tests for the project identification maintainer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)

(define-testcase validate-fix-project-identification-registered ()
  (assert-t (not (null (atelier:find-maintainer 'atelier:fix-project-identification)))))

(define-testcase testsuite-fix-project-identification ()
  (validate-fix-project-identification-registered))

;;;; End of file `fix-project-identification.lisp'
