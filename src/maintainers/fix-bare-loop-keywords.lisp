;;;; fix-bare-loop-keywords.lisp — Bare loop keyword automatic maintainer

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

(define-automatic-maintainer fix-bare-loop-keywords
    ((finding bare-loop-keyword-finding))
  "Replace a bare LOOP clause keyword symbol with its keyword symbol form."
  (let* ((keyword (concrete-syntax-tree:raw (finding-cst-node finding)))
         (replacement (format nil ":~(~A~)" (symbol-name keyword))))
    (make-text-resolution
     :maintainer 'fix-bare-loop-keywords
     :finding finding
     :kind :automatic
     :description (format nil "Replace bare ~S with keyword form." keyword)
     :replacement replacement)))

;;;; End of file 'fix-bare-loop-keywords.lisp'
