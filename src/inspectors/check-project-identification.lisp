;;;; check-project-identification.lisp — Project identification block inspector

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

(defun canonical-project-identification-text ()
  "Return the canonical project identification block as a string.
Uses *PROJECT-CONFIGURATION* for project name, homepage, copyright year,
and copyright holder. Returns NIL when no project configuration is available."
  (declare (values (or null string)))
  (when *project-configuration*
    (let ((project-name (project-configuration-project-name *project-configuration*))
          (homepage (project-configuration-homepage *project-configuration*))
          (copyright-year (project-configuration-copyright-year *project-configuration*))
          (copyright-holder (project-configuration-copyright-holder *project-configuration*)))
      (when (and project-name homepage copyright-year copyright-holder)
        (format nil "~
;;;; ~A (~A)~%~
;;;; This file is part of ~A.~%~
;;;;~%~
;;;; Copyright © ~A ~A~%~
;;;; All rights reserved."
                project-name homepage project-name
                copyright-year copyright-holder)))))

(defun find-identification-block (lines)
  "Find the project identification block in LINES.
Look for a contiguous block of ;;;; comment lines that contains at least
one of the markers: 'This file is part of', 'Copyright', or a URL in
parentheses. Return (values start-index end-index) or NIL."
  (declare (type vector lines)
           (values (or null (integer 0)) (or null (integer 0))))
  (let ((start nil)
        (end nil))
    (loop :for i :from 0 :below (length lines)
          :for line = (aref lines i)
          :do (cond
                ;; Inside a block of ;;;; comments
                ((string-prefix-p ";;;;" line)
                 (unless start (setf start i))
                 (setf end (1+ i))
                 ;; Check for identification markers
                 (when (or (search "This file is part of" line)
                           (search "Copyright" line)
                           (search "All rights reserved" line))
                   ;; Found a marker — we're in the right block
                   (return-from find-identification-block
                     (values start end))))
                ;; Blank line inside a comment block — continue
                ((and start (string= "" line))
                 nil)
                ;; Non-comment, non-blank line — reset
                (t
                 (setf start nil end nil))))
    nil))


;;;;
;;;; Inspector
;;;;

(define-file-inspector check-project-identification ((pathname pathname))
  "Check that the file contains a canonical project identification block.
The block should include project name, homepage URL, copyright year and holder.
Only applies to Lisp source files and requires *PROJECT-CONFIGURATION*."
  (when (and (lisp-source-file-p pathname)
             *project-configuration*)
    (let* ((lines (read-file-into-line-vector pathname))
           (expected (canonical-project-identification-text)))
      (when expected
        (let ((header (read-file-header pathname :maximum-lines 15)))
          (unless (search expected header)
            (multiple-value-bind (start end)
                (find-identification-block lines)
              (if start
                  ;; Found a block but it doesn't match
                  (list (make-instance 'project-identification-finding
                         :inspector 'check-project-identification
                         :severity :style
                         :observation "Project identification block does not match configuration."
                         :rationale "The identification block should contain the project name, homepage, copyright year and holder from project configuration."
                         :file pathname
                         :line (1+ start)
                         :column 0
                         :end-line (1+ (1- end))
                         :end-column (length (aref lines (1- end)))
                         :source-text (join-lines
                                       (loop :for i :from start :below end
                                             :collect (aref lines i)))))
                  ;; No identification block found at all
                  (list (make-file-finding
                         :inspector 'check-project-identification
                         :severity :style
                         :observation "File lacks a project identification block."
                         :rationale "The file should contain a project identification block after the header line."
                         :file pathname))))))))))

;;;; End of file `check-project-identification.lisp'
