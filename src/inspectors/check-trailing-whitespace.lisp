;;;; check-trailing-whitespace.lisp — Trailing whitespace inspector

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

(define-line-inspector check-trailing-whitespace ((pathname pathname))
  "Check for trailing whitespace on source lines.
Return a list of TRAILING-WHITESPACE-FINDING for each line with trailing spaces or tabs."
  (with-open-file (stream pathname :direction :input :external-format :utf-8)
    (loop :for line = (read-line stream nil nil)
          :for line-number :from 1
          :while line
          :for trimmed-length = (length (string-right-trim '(#\Space #\Tab) line))
          :when (< trimmed-length (length line))
          :collect (make-instance 'trailing-whitespace-finding
                     :inspector 'check-trailing-whitespace
                     :severity :style
                     :observation (format nil "Line ~D has ~D trailing whitespace character~:P."
                                         line-number (- (length line) trimmed-length))
                     :rationale "Trailing whitespace creates noisy diffs and wastes bytes."
                     :file pathname
                     :line line-number
                     :column trimmed-length
                     :end-line line-number
                     :end-column (length line)
                     :source-text line))))

;;;; End of file `check-trailing-whitespace.lisp'
