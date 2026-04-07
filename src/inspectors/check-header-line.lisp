;;;; check-header-line.lisp — Canonical header line inspector

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


;;;;
;;;; Helpers
;;;;

(defun lisp-source-file-p (pathname)
  "Return T if PATHNAME has a Lisp source file extension."
  (declare (type pathname pathname))
  (and (member (pathname-type pathname) '("lisp" "asd" "asdf") :test #'string-equal) t))


;;;;
;;;; Inspector
;;;;

(define-file-inspector check-header-line ((pathname pathname))
  "Check that the first non-shebang line is a canonical header line.
The canonical header line has the form: ;;;; filename — description.
Only applies to Lisp source files (.lisp, .asd, .asdf)."
  (when (lisp-source-file-p pathname)
    (let* ((lines (read-file-into-line-vector pathname))
           (first-line-index
             (if (and (> (length lines) 0)
                      (string-prefix-p "#!" (aref lines 0)))
                 1
                 0))
           (first-line (when (< first-line-index (length lines))
                         (aref lines first-line-index)))
           (filename (file-namestring pathname))
           (expected-prefix (format nil ";;;; ~A —" filename)))
      (when (and first-line
                 (not (string-prefix-p expected-prefix first-line)))
        (list (make-instance 'header-line-finding
               :inspector 'check-header-line
               :severity :style
               :observation "First line is not a canonical header line."
               :rationale (format nil
                                  "The first line should begin with: ~A"
                                  expected-prefix)
               :file pathname
               :line (1+ first-line-index)
               :column 0
               :end-line (1+ first-line-index)
               :end-column (length first-line)
               :source-text first-line))))))

;;;; End of file `check-header-line.lisp'
