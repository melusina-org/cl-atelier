;;;; check-footer-line.lisp — Canonical footer line inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)

(define-file-inspector check-footer-line ((pathname pathname))
  "Check that the last non-blank line is a canonical footer line.
The canonical footer line has the form: <comment-prefix>End of file `filename'
Applies to all files with a recognised comment style."
  (let ((prefix (file-comment-prefix pathname)))
    (when prefix
      (let* ((lines (read-file-into-line-vector pathname))
             (filename (file-namestring pathname))
             (expected-footer (format nil "~AEnd of file `~A'" prefix filename)))
        ;; Find the last non-blank line.
        (let ((last-index
                (loop :for i :from (1- (length lines)) :downto 0
                      :unless (string= "" (aref lines i))
                      :return i)))
          (when last-index
            (let ((last-line (aref lines last-index)))
              (unless (string= expected-footer last-line)
                (list (make-instance 'footer-line-finding
                       :inspector 'check-footer-line
                       :severity :style
                       :observation "Last line is not a canonical footer line."
                       :rationale (format nil "The last non-blank line should be: ~A"
                                          expected-footer)
                       :file pathname
                       :line (1+ last-index)
                       :column 0
                       :end-line (1+ last-index)
                       :end-column (length last-line)
                       :source-text last-line))))))))))

;;;; End of file `check-footer-line.lisp'
