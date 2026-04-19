;;;; package.lisp — Package for the Atelier test suite Lisp System

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(defpackage #:atelier/test
  (:use #:common-lisp)
  (:import-from #:confidence
   #:define-testcase
   #:define-assertion
   #:assert-t
   #:assert-nil
   #:assert-t*
   #:assert-eq
   #:assert-equal
   #:assert-set-equal
   #:assert-string=
   #:assert-type)
  (:export
   #:run-all-tests
   #:run-fast-tests
   #:run-slow-tests))

;;;; End of file `package.lisp'
