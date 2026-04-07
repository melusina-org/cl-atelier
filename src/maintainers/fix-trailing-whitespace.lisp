;;;; fix-trailing-whitespace.lisp — Trailing whitespace automatic maintainer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)

(define-automatic-maintainer fix-trailing-whitespace
    ((finding trailing-whitespace-finding))
  "Remove trailing whitespace from the affected line."
  (make-text-resolution
   :maintainer 'fix-trailing-whitespace
   :finding finding
   :kind :automatic
   :description "Remove trailing whitespace."
   :replacement ""))

;;;; End of file `fix-trailing-whitespace.lisp'
