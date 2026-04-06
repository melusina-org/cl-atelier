;;;; check-mixed-indentation.lisp — Mixed indentation inspector

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

(define-line-inspector check-mixed-indentation
    ((pathname pathname) (lines vector))
  "Check for mixed tabs and spaces in indentation.
Return a list of MIXED-INDENTATION-FINDING for lines using the wrong
indentation character. The expected style is read from
*CURRENT-LINTER-CONFIGURATION* and defaults to :SPACES."
  (let ((style (if *current-linter-configuration*
                   (linter-configuration-indentation-style
                    *current-linter-configuration*)
                   :spaces)))
    (flet ((wrong-indentation-p (line)
             (let ((leading-whitespace
                     (subseq line 0 (position-if-not
                                     (lambda (character)
                                       (or (char= character #\Space)
                                           (char= character #\Tab)))
                                     line))))
               (case style
                 (:spaces (position #\Tab leading-whitespace))
                 (:tabs (position #\Space leading-whitespace))))))
      (loop :for line :across lines
            :for line-number :from 1
            :for wrong-position = (wrong-indentation-p line)
            :when wrong-position
            :collect (make-instance 'mixed-indentation-finding
                       :inspector 'check-mixed-indentation
                       :severity :style
                       :observation (format nil "Line ~D uses ~A for indentation (expected ~A)."
                                           line-number
                                           (if (eq style :spaces) "tabs" "spaces")
                                           (string-downcase (symbol-name style)))
                       :rationale "Consistent indentation prevents alignment issues across editors."
                       :file pathname
                       :line line-number
                       :column wrong-position
                       :end-line line-number
                       :end-column (1+ wrong-position)
                       :source-text line)))))

;;;; End of file `check-mixed-indentation.lisp'
