;;;; fix-project-identification.lisp — Tests for the project identification maintainer

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

(define-testcase validate-fix-project-identification-registered ()
  (assert-t (not (null (atelier:find-maintainer 'atelier:fix-project-identification)))))

(define-testcase testsuite-fix-project-identification ()
  (validate-fix-project-identification-registered))

;;;; End of file `fix-project-identification.lisp'
