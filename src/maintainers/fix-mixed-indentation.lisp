;;;; fix-mixed-indentation.lisp — Mixed indentation automatic maintainer

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

(define-automatic-maintainer fix-mixed-indentation
    ((finding mixed-indentation-finding))
  "Normalise the leading whitespace of the affected line to the configured style.
Reads *LINTER-CONFIGURATION* for the expected style (defaulting to :SPACES).
Each tab replaced with 2 spaces; each errant space replaced with a tab."
  (let* ((style (if *linter-configuration*
                    (linter-configuration-indentation-style *linter-configuration*)
                    :spaces))
         ;; Replace the single wrong-indentation character in the finding span.
         (replacement (ecase style
                        (:spaces "  ")
                        (:tabs (string #\Tab)))))
    (make-text-resolution
     :maintainer 'fix-mixed-indentation
     :finding finding
     :kind :automatic
     :description "Replace wrong indentation character."
     :replacement replacement)))

;;;; End of file 'fix-mixed-indentation.lisp'
