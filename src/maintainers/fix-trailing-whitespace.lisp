;;;; fix-trailing-whitespace.lisp — Trailing whitespace automatic maintainer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

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

;;;; End of file 'fix-trailing-whitespace.lisp'
