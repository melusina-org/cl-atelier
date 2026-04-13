;;;; package.lisp — Exploratory tests for SWANK protocol behavior

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(defpackage #:atelier/testsuite/swank
  (:use #:common-lisp)
  (:import-from #:confidence
   #:define-testcase
   #:assert-t
   #:assert-t*
   #:assert-nil
   #:assert-string=
   #:assert-equal
   #:assert=
   #:assert-condition
   #:assert-eq)
  (:export
   #:run-swank-tests))

;;;; End of file `package.lisp'
