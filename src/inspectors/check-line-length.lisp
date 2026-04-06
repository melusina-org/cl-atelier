;;;; check-line-length.lisp — Line length inspector

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

(defparameter *default-maximum-line-length* 100
  "The default maximum line length in characters.")

(define-line-inspector check-line-length
    ((line string) (line-number integer))
  "Check a single line for excessive length.
Return a list containing a LINE-TOO-LONG-FINDING if LINE exceeds the
maximum length, or NIL. Skip Lisp definition forms and single-word lines."
  (flet ((definition-line-p (line)
           (ppcre:scan "^ *\\(def" line))
         (single-word-line-p (line)
           (<= (count-string-words line) 1)))
    (when (and (> (length line) *default-maximum-line-length*)
               (not (definition-line-p line))
               (not (single-word-line-p line)))
      (list (make-instance 'line-too-long-finding
              :inspector 'check-line-length
              :severity :style
              :observation (format nil "Line ~D is ~D characters long (maximum ~D)."
                                  line-number (length line)
                                  *default-maximum-line-length*)
              :rationale "Long lines reduce readability and cause horizontal scrolling."
              :file *current-pathname*
              :line line-number
              :column *default-maximum-line-length*
              :end-line line-number
              :end-column (length line)
              :source-text line)))))

;;;; End of file `check-line-length.lisp'
