;;;; package.lisp — Package for the Atelier editor testsuite

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(defpackage #:atelier/test/editor
  (:use #:cl)
  (:import-from #:org.melusina.confidence
    #:define-testcase
    #:assert-t
    #:assert-t*
    #:assert-nil
    #:assert-equal
    #:assert-string=
    #:assert-condition)
  (:export
   #:run-all-editor-tests)
  (:documentation "Testsuite for the Atelier projectional editor."))

;;;; End of file `package.lisp'
