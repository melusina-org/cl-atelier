;;;; entrypoint.lisp — Entrypoint for the Atelier test suite Lisp System

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

(define-testcase run-all-tests ()
  (atelier:initialize)
  (testsuite-utilities)
  (testsuite-parameter)
  (testsuite-license)
  (testsuite-finding)
  (testsuite-resolution)
  (testsuite-inspector)
  (testsuite-maintainer)
  (testsuite-bridge)
  (testsuite-runner)
  (testsuite-asdf)
  (testsuite-check-file-encoding)
  (testsuite-check-spdx-license-header)
  (testsuite-check-trailing-whitespace)
  (testsuite-check-line-length)
  (testsuite-check-mixed-indentation)
  (testsuite-check-earmuffs)
  (testsuite-check-constant-naming)
  (testsuite-check-bare-lambda)
  (testsuite-check-loop-keywords)
  (testsuite-check-labels-for-flet)
  (testsuite-pretty-printer)
  (testsuite-write-back)
  (testsuite-fix-trailing-whitespace)
  (testsuite-fix-mixed-indentation)
  (testsuite-fix-earmuffs)
  (testsuite-fix-constant-naming)
  (testsuite-fix-bare-loop-keywords)
  (testsuite-fix-bare-lambda)
  (testsuite-fix-labels-to-flet)
  (testsuite-autofix)
  (testsuite-linter)
  (testsuite-template))

;;;; End of file `entrypoint.lisp'
