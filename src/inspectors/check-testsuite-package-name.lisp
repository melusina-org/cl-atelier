;;;; check-testsuite-package-name.lisp — /testsuite package-name inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)


;;;;
;;;; /testsuite Package-Name Regex
;;;;

(defparameter *testsuite-package-name-scanner*
  (cl-ppcre:create-scanner
   "(?<=[A-Za-z0-9._-])/testsuite(?![A-Za-z0-9_])"
   :case-insensitive-mode t)
  "Scanner for the /testsuite suffix when it appears immediately after a
package-name token character. The lookbehind requires an identifier character,
which rules out bare occurrences of the word testsuite in prose, while the
lookahead rules out identifiers such as /testsuites. The scanner is
case-insensitive so that both /testsuite and /TESTSUITE are detected.")

(defun canonical-testsuite-replacement (matched)
  "Return the canonical replacement for MATCHED preserving case.
MATCHED is the substring captured by *TESTSUITE-PACKAGE-NAME-SCANNER*
— that is, the slash followed by the string testsuite in some casing.
Return /TEST when MATCHED is fully upper-case, /test otherwise."
  (declare (type string matched)
           (values string))
  (flet ((letter-p (c) (alpha-char-p c)))
    (let ((letters (remove-if-not #'letter-p matched)))
      (if (and (plusp (length letters))
               (every #'upper-case-p letters))
          "/TEST"
          "/test"))))

(defun collect-testsuite-matches (line)
  "Return a list of (start end matched replacement) tuples for every
/testsuite occurrence on LINE."
  (declare (type string line)
           (values list))
  (let ((matches nil)
        (start 0))
    (loop
      (multiple-value-bind (match-start match-end)
          (cl-ppcre:scan *testsuite-package-name-scanner* line :start start)
        (unless match-start
          (return (nreverse matches)))
        (let* ((matched (subseq line match-start match-end))
               (replacement (canonical-testsuite-replacement matched)))
          (push (list match-start match-end matched replacement) matches))
        (setf start match-end)))))


;;;;
;;;; Inspector
;;;;

(define-line-inspector check-testsuite-package-name
    ((line string) (line-number integer))
  "Check a line for /testsuite occurrences in package names.
Return one TESTSUITE-PACKAGE-NAME-FINDING per occurrence. The inspector
flags the /testsuite suffix whenever it appears at the end of a
package-name token, which covers DEFPACKAGE and IN-PACKAGE forms,
qualified symbol prefixes, uninterned symbol readers, and string
arguments passed to FIND-PACKAGE or other PACKAGE-dictionary calls
such as UIOP:FIND-SYMBOL*."
  (flet ((make-finding (match)
           (destructuring-bind (match-start match-end matched replacement) match
             (declare (ignore replacement))
             (make-instance 'testsuite-package-name-finding
               :inspector 'check-testsuite-package-name
               :severity :warning
               :observation (format nil "Package-name token uses deprecated suffix ~S."
                                    matched)
               :rationale "The canonical suffix for the test package is /test, not /testsuite."
               :file *current-pathname*
               :line line-number
               :column match-start
               :end-line line-number
               :end-column match-end
               :source-text line))))
    (mapcar #'make-finding (collect-testsuite-matches line))))

;;;; End of file `check-testsuite-package-name.lisp'
