;;;; bridge.lisp — Bridge from legacy hints to findings

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:atelier/legacy)

(defun hint-to-finding (hint)
  "Convert a legacy HINT instance to the corresponding FINDING instance.
HINT-AT-FILE becomes FILE-FINDING. HINT-AT-FILE-LINE becomes LINE-FINDING.
The hint's keyword code is used as the inspector identity.
Severity defaults to :WARNING."
  (declare (type hint hint))
  (etypecase hint
    (hint-at-file-line
     (atelier:make-line-finding
       :inspector (slot-value hint 'code)
       :severity :warning
       :observation (slot-value hint 'description)
       :rationale (or (slot-value hint 'explanation) "")
       :file (or (slot-value hint 'pathname) #p"unknown")
       :line (or (slot-value hint 'line) 1)
       :column 0
       :end-line (or (slot-value hint 'line) 1)
       :end-column 0
       :source-text ""))
    (hint-at-file
     (atelier:make-file-finding
       :inspector (slot-value hint 'code)
       :severity :warning
       :observation (slot-value hint 'description)
       :rationale (or (slot-value hint 'explanation) "")
       :file (or (slot-value hint 'pathname) #p"unknown")))
    (hint
     (atelier:make-file-finding
       :inspector (slot-value hint 'code)
       :severity :warning
       :observation (slot-value hint 'description)
       :rationale (or (slot-value hint 'explanation) "")
       :file #p"unknown"))))

(defun lint-with-findings (&rest pathnames)
  "Run the legacy linter on PATHNAMES and return both hints and findings.
Returns two values: the list of hints (legacy format) and the list of
findings (new schema)."
  (let ((hints (apply #'lint pathnames)))
    (flet ((convert-hint (hint)
             (hint-to-finding hint)))
      (values hints (mapcar #'convert-hint hints)))))

;;;; End of file `bridge.lisp'
