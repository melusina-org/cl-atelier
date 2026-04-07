;;;; check-spdx-license-header.lisp — SPDX license identifier inspector

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

(defun read-file-header (pathname &key (maximum-lines 20))
  "Read the first MAXIMUM-LINES lines of PATHNAME and return them as a single string.
Return an empty string if the file cannot be read."
  (declare (type pathname pathname)
           (type (integer 1) maximum-lines)
           (values string))
  (handler-case
      (with-open-file (stream pathname :direction :input :external-format :utf-8)
        (with-output-to-string (buffer)
          (loop :for line-number :from 1 :to maximum-lines
                :for line = (read-line stream nil nil)
                :while line
                :do (write-string line buffer)
                    (write-char #\Newline buffer))))
    (error () "")))

(defparameter *spdx-marker* "SPDX-License-Identifier:"
  "The marker string that identifies an SPDX license header line.")


;;;;
;;;; Inspector
;;;;

(define-file-inspector check-spdx-license-header ((pathname pathname))
  "Check that source files contain the correct SPDX license identifier.
Return a SPDX-LICENSE-HEADER-FINDING when the header is missing or mismatched, or NIL.
When *PROJECT-CONFIGURATION* is NIL, only check for presence of the SPDX marker."
  (let ((header (read-file-header pathname))
        (expected-identifier
          (when *project-configuration*
            (project-configuration-license *project-configuration*))))
    (cond
      ;; No SPDX marker found
      ((null (search *spdx-marker* header))
       (list (make-instance 'spdx-license-header-finding
               :inspector 'check-spdx-license-header
               :severity :warning
               :observation (format nil "File ~A has no SPDX-License-Identifier header."
                                   (file-namestring pathname))
               :rationale "SPDX identifiers enable machine-verifiable license compliance."
               :file pathname)))
      ;; SPDX marker found but identifier mismatches project configuration
      ((and expected-identifier
            (null (search (concatenate 'string *spdx-marker* " " expected-identifier)
                          header)))
       (list (make-instance 'spdx-license-header-finding
               :inspector 'check-spdx-license-header
               :severity :warning
               :observation (format nil "File ~A declares a license other than ~A."
                                   (file-namestring pathname) expected-identifier)
               :rationale "All files in a project should use the license declared in project configuration."
               :file pathname)))
      ;; All good
      (t nil))))

;;;; End of file `check-spdx-license-header.lisp'
