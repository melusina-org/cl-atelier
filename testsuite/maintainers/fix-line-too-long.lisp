;;;; fix-line-too-long.lisp — Tests for the line-too-long maintainer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)

(define-testcase validate-fix-line-too-long-registered ()
  (assert-t (not (null (atelier:find-maintainer 'atelier:fix-line-too-long)))))

(define-testcase testsuite-fix-line-too-long ()
  (validate-fix-line-too-long-registered))

;;;; End of file `fix-line-too-long.lisp'
