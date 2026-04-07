;;;; fix-header-line.lisp — Header line automatic maintainer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)

(define-automatic-maintainer fix-header-line
    ((finding header-line-finding))
  "Correct the canonical header line to: <prefix>filename — description.
Extracts the description from the existing line when possible."
  (let* ((source (finding-source-text finding))
         (filename (file-namestring (finding-file finding)))
         (prefix (or (file-comment-prefix (finding-file finding)) ";;;; "))
         ;; Try to extract the description from the existing line.
         ;; Strip any comment prefix characters and the filename, then look for a dash.
         (description
           (let ((stripped (string-left-trim '(#\; #\# #\/ #\% #\Space #\Tab) source)))
             ;; Strip "dnl " prefix if present
             (when (string-prefix-p "dnl " stripped)
               (setf stripped (subseq stripped 4)))
             ;; If it starts with the filename, skip it
             (when (string-prefix-p filename stripped)
               (setf stripped (subseq stripped (length filename))))
             ;; Strip dash separators
             (setf stripped (string-left-trim '(#\Space #\— #\– #\- #\Tab) stripped))
             (if (string= "" stripped)
                 "DESCRIPTION"
                 stripped)))
         (replacement (format nil "~A~A — ~A" prefix filename description)))
    (make-text-resolution
     :maintainer 'fix-header-line
     :finding finding
     :kind :automatic
     :description (format nil "Set canonical header line for ~A." filename)
     :replacement replacement)))

;;;; End of file `fix-header-line.lisp'
