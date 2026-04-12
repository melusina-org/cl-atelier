;;;; package.lisp — Exploratory tests for pipe I/O behavior

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(defpackage #:atelier/testsuite/input-output
  (:use #:common-lisp)
  (:import-from #:confidence
   #:define-testcase
   #:assert-t
   #:assert-t*
   #:assert-nil
   #:assert-string=
   #:assert=
   #:assert-condition)
  (:export
   #:run-io-tests))

;;;; End of file `package.lisp'
