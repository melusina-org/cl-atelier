;;;; fix-footer-line.lisp — Footer line automatic maintainer

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

(define-automatic-maintainer fix-footer-line
    ((finding footer-line-finding))
  "Replace the last non-blank line with the canonical footer line."
  (let* ((filename (file-namestring (finding-file finding)))
         (prefix (or (file-comment-prefix (finding-file finding)) ";;;; "))
         (replacement (format nil "~AEnd of file `~A'" prefix filename)))
    (make-text-resolution
     :maintainer 'fix-footer-line
     :finding finding
     :kind :automatic
     :description (format nil "Set canonical footer line for ~A." filename)
     :replacement replacement)))

;;;; End of file `fix-footer-line.lisp'
