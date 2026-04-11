;;;; package.lisp — Package for the Atelier MCP test suite

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(defpackage #:atelier/testsuite/mcp
  (:use #:common-lisp #:atelier/mcp)
  (:import-from #:confidence
   #:define-testcase
   #:define-assertion
   #:assert-t
   #:assert-t*
   #:assert-nil
   #:assert-eq
   #:assert-eql
   #:assert-equal
   #:assert-string=
   #:assert-type
   #:assert=
   #:assert-condition
   #:assert-list-equal)
  (:export
   #:run-mcp-tests))

;;;; End of file `package.lisp'
