;;;; fix-constant-naming.lisp — Constant naming automatic maintainer

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

(define-automatic-maintainer fix-constant-naming
    ((finding constant-naming-finding))
  "Add +plus-surrounded+ markers to a DEFCONSTANT constant name."
  (let* ((name (concrete-syntax-tree:raw (finding-cst-node finding)))
         (replacement (format nil "+~(~A~)+" (symbol-name name))))
    (make-text-resolution
     :maintainer 'fix-constant-naming
     :finding finding
     :kind :automatic
     :description (format nil "Add +plus-surrounded+ markers to ~S." name)
     :replacement replacement)))

;;;; End of file 'fix-constant-naming.lisp'
