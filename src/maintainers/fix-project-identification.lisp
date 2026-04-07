;;;; fix-project-identification.lisp — Project identification automatic maintainer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)

(define-automatic-maintainer fix-project-identification
    ((finding project-identification-finding))
  "Replace the project identification block with the canonical form.
Requires *PROJECT-CONFIGURATION* to be bound."
  ;; Only handle line-findings (which have positional information for text-resolution).
  ;; File-findings (no identification block found at all) are not auto-fixable.
  (when (typep finding 'line-finding)
    (let ((replacement (canonical-project-identification-text)))
      (when replacement
        (make-text-resolution
         :maintainer 'fix-project-identification
         :finding finding
         :kind :automatic
         :description "Replace project identification with canonical form."
         :replacement replacement)))))

;;;; End of file `fix-project-identification.lisp'
