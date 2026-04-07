;;;; check-mixed-indentation.lisp — Mixed indentation inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)

(define-line-inspector check-mixed-indentation
    ((line string) (line-number integer))
  "Check a single line for wrong indentation characters.
Return a list containing a MIXED-INDENTATION-FINDING if LINE uses the
wrong indentation character, or NIL. The expected style is read from
*LINTER-CONFIGURATION* and defaults to :SPACES."
  (let ((style (if *linter-configuration*
                   (linter-configuration-indentation-style
                    *linter-configuration*)
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
      (let ((wrong-position (wrong-indentation-p line)))
        (when wrong-position
          (list (make-instance 'mixed-indentation-finding
                  :inspector 'check-mixed-indentation
                  :severity :style
                  :observation (format nil "Line ~D uses ~A for indentation (expected ~A)."
                                      line-number
                                      (if (eq style :spaces) "tabs" "spaces")
                                      (string-downcase (symbol-name style)))
                  :rationale "Consistent indentation prevents alignment issues across editors."
                  :file *current-pathname*
                  :line line-number
                  :column wrong-position
                  :end-line line-number
                  :end-column (1+ wrong-position)
                  :source-text line)))))))

;;;; End of file `check-mixed-indentation.lisp'
