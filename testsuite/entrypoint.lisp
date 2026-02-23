;;;; entrypoint.lisp — Entrypoint for the Atelier test suite Lisp System

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:atelier/testsuite)

(define-testcase run-all-tests ()
  (atelier:initialize)
  (testsuite-utilities)
  (testsuite-parameter)
  (testsuite-license)
  (testsuite-linter)
  (testsuite-template))

;;;; End of file `entrypoint.lisp'
