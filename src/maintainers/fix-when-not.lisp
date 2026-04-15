;;;; fix-when-not.lisp — WHEN (NOT X) to UNLESS automatic maintainer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)

(define-automatic-maintainer fix-when-not
    ((finding when-not-finding))
  "Replace (WHEN (NOT X) BODY...) with (UNLESS X BODY...).
Returns a SYNTAX-RESOLUTION whose transform extracts the inner test
from (NOT X) and wraps the body with UNLESS."
  (make-syntax-resolution
   :maintainer 'fix-when-not
   :finding finding
   :kind :automatic
   :description "Replace WHEN (NOT ...) with UNLESS."
   :transform (lambda (raw-form)
                (let* ((test-form (second raw-form))
                       (inner-test (second test-form))
                       (body (cddr raw-form)))
                  `(unless ,inner-test ,@body)))))

;;;; End of file `fix-when-not.lisp'
