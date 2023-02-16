;;;; package.lisp — Package for the Atelier test suite Lisp System

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.atelier/testsuite
  (:local-nicknames
   (#:rashell #:org.melusina.rashell)
   (#:atelier #:org.melusina.atelier))
  (:use #:common-lisp)
  (:import-from #:org.melusina.confidence
   #:define-testcase
   #:define-assertion
   #:assert-t
   #:assert-eq
   #:assert-set-equal
   #:assert-string=
   #:assert-type))

;;;; End of file `package.lisp'
