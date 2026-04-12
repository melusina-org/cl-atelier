;;;; package.lisp — Package for the Atelier child worker

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(defpackage #:atelier/child-worker
  (:use #:common-lisp)
  (:export
   ;; Entry point
   #:start-worker
   ;; Introspection helpers
   #:list-packages-data
   #:list-package-symbols-data
   #:describe-symbol-data
   #:find-definition-data
   #:run-testsuite-data))

;;;; End of file `package.lisp'
