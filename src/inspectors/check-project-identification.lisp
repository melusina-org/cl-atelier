;;;; check-project-identification.lisp — Project identification block inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)


;;;;
;;;; Helpers
;;;;

(defun canonical-project-identification-text (&optional (prefix ";;;; "))
  "Return the canonical project identification block as a string.
PREFIX is the line comment prefix (default: Lisp's ;;;; ).
Uses *PROJECT-CONFIGURATION* for project name, homepage, copyright year,
and copyright holder. Returns NIL when configuration is incomplete."
  (declare (type string prefix)
           (values (or null string)))
  (when *project-configuration*
    (let ((project-name (project-configuration-project-name *project-configuration*))
          (homepage (project-configuration-homepage *project-configuration*))
          (copyright-year (project-configuration-copyright-year *project-configuration*))
          (copyright-holder (project-configuration-copyright-holder *project-configuration*)))
      (when (and project-name homepage copyright-year copyright-holder)
        (let ((empty-comment (string-right-trim '(#\Space) prefix)))
          (format nil "~
~A~A (~A)~%~
~AThis file is part of ~A.~%~
~A~%~
~ACopyright © ~A ~A~%~
~AAll rights reserved."
                  prefix project-name homepage
                  prefix project-name
                  empty-comment
                  prefix copyright-year copyright-holder
                  prefix))))))

(defun find-identification-block (lines prefix)
  "Find the project identification block in LINES.
PREFIX is the comment prefix to look for. Look for a contiguous block
of comment lines that contains at least one identification marker:
'This file is part of', 'Copyright', or 'All rights reserved'.
Return (values start-index end-index) or NIL."
  (declare (type vector lines)
           (type string prefix)
           (values (or null (integer 0)) (or null (integer 0))))
  (let ((start nil)
        (end nil))
    (loop :for i :from 0 :below (length lines)
          :for line = (aref lines i)
          :do (cond
                ;; Inside a block of comments with the right prefix
                ((string-prefix-p (string-right-trim '(#\Space) prefix) line)
                 (unless start (setf start i))
                 (setf end (1+ i))
                 ;; Check for identification markers
                 (when (or (search "This file is part of" line)
                           (search "Copyright" line)
                           (search "All rights reserved" line))
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
Applies to all files with a recognised comment style. Requires *PROJECT-CONFIGURATION*."
  (let ((prefix (file-comment-prefix pathname)))
    (when (and prefix *project-configuration*)
      (let* ((lines (read-file-into-line-vector pathname))
             (expected (canonical-project-identification-text prefix)))
        (when expected
          (let ((header (read-file-header pathname :maximum-lines 15)))
            (unless (search expected header)
              (multiple-value-bind (start end)
                  (find-identification-block lines prefix)
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
                           :file pathname)))))))))))

;;;; End of file `check-project-identification.lisp'
