;;;; fix-testsuite-package-name.lisp — /testsuite package-name maintainer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)

(defun finding-source-text-substring (finding)
  "Return the substring of FINDING's source text delimited by its column range."
  (declare (type line-finding finding)
           (values string))
  (subseq (finding-source-text finding)
          (finding-column finding)
          (finding-end-column finding)))

(define-automatic-maintainer fix-testsuite-package-name
    ((finding testsuite-package-name-finding))
  "Replace the /testsuite suffix of a package-name token with /test.
The finding's COLUMN and END-COLUMN delimit the /testsuite occurrence;
the replacement preserves its letter case — /TESTSUITE becomes /TEST,
/testsuite becomes /test."
  (let* ((matched (finding-source-text-substring finding))
         (replacement (canonical-testsuite-replacement matched)))
    (make-text-resolution
     :maintainer 'fix-testsuite-package-name
     :finding finding
     :kind :automatic
     :description (format nil "Rename ~A to ~A." matched replacement)
     :replacement replacement)))

;;;; End of file `fix-testsuite-package-name.lisp'
