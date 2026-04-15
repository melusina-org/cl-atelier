;;;; fix-spdx-license-header.lisp — SPDX license header automatic maintainer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)

(define-automatic-maintainer fix-spdx-license-header
    ((finding spdx-license-header-finding))
  "Insert or correct the SPDX-License-Identifier header line.
When the SPDX marker is missing, insert a new line at the finding's location.
When the identifier is wrong, replace the existing line with the correct one.
Uses the license from *PROJECT-CONFIGURATION* when available, otherwise MIT."
  (let* ((prefix (or (file-comment-prefix (finding-file finding)) ";;;; "))
         (license-id (if *project-configuration*
                         (or (project-configuration-license *project-configuration*)
                             "MIT")
                         "MIT"))
         (spdx-line (format nil "~ASPDX-License-Identifier: ~A" prefix license-id))
         (source (finding-source-text finding)))
    (if (string= "" source)
        ;; Missing: insert a new SPDX line (the span is zero-width, so this is an insertion)
        (make-text-resolution
         :maintainer 'fix-spdx-license-header
         :finding finding
         :kind :automatic
         :description (format nil "Insert ~A~A header." *spdx-marker* license-id)
         :replacement (format nil "~A~%" spdx-line))
        ;; Wrong identifier: replace the existing line
        (make-text-resolution
         :maintainer 'fix-spdx-license-header
         :finding finding
         :kind :automatic
         :description (format nil "Correct SPDX identifier to ~A." license-id)
         :replacement spdx-line))))

;;;; End of file `fix-spdx-license-header.lisp'
