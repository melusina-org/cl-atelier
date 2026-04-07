;;;; pretty-printer.lisp — Testsuite for the Atelier pretty-printer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:atelier/testsuite)


;;;;
;;;; Fixture Parsing
;;;;

(defun parse-right-margin-array (string)
  "Parse a right-margin array string like \"[nil, 40, 20]\" into a list.
Returns a list of (NIL or integer) values, max 3 entries.
Recognises the literal token \"nil\" as the Lisp NIL value."
  (declare (type string string)
           (values list))
  (let ((matches nil))
    (cl-ppcre:do-matches-as-strings (token "[^\\[\\],\\s]+" string)
      (push token matches))
    (flet ((parse-token (token)
             (if (string-equal token "nil")
                 nil
                 (parse-integer token))))
      (mapcar #'parse-token (reverse (subseq matches 0 (min 3 (length matches))))))))

(defun read-pretty-print-fixture (pathname)
  "Read a pretty-printer fixture from PATHNAME.
Returns (values front-matter input-form column right-margins expected-strings).
FRONT-MATTER is an alist. INPUT-FORM is a Lisp form read from document 0.
COLUMN is the insertion column (integer >= 0). RIGHT-MARGINS is a list of
(NIL or integer). EXPECTED-STRINGS is a list of strings, one per right-margin
entry, joined from document lines."
  (declare (type pathname pathname)
           (values list t (integer 0) list list))
  (multiple-value-bind (front-matter documents)
      (atelier:read-file-documents-with-yaml-front-matter pathname)
    (let* ((column-str (cdr (assoc :column front-matter)))
           (column (if column-str (parse-integer column-str) 0))
           (margin-str (cdr (assoc :right-margin front-matter)))
           (right-margins (if margin-str
                              (parse-right-margin-array margin-str)
                              (list nil)))
           (input-form (read-from-string
                        (atelier:join-lines (first documents))))
           (expected-strings
             (flet ((join-document (doc)
                      (atelier:join-lines doc)))
               (mapcar #'join-document (rest documents)))))
      (values front-matter input-form column right-margins expected-strings))))


;;;;
;;;; Testcases
;;;;

(define-testcase validate-one-pretty-print-fixture (pathname)
  "Validate all margin cases in the pretty-printer fixture at PATHNAME."
  (multiple-value-bind (front-matter form column right-margins expected-strings)
      (read-pretty-print-fixture pathname)
    (declare (ignore front-matter))
    (loop :for right-margin :in right-margins
          :for expected :in expected-strings
          :for actual = (atelier:pretty-print-form form column :right-margin right-margin)
          :do (assert-string= expected actual))))

(define-testcase validate-pretty-print-fixtures ()
  "Validate all pretty-printer fixtures in testsuite/fixtures/pretty-print/."
  (let ((fixture-directory
          (merge-pathnames "testsuite/fixtures/pretty-print/"
                           (asdf:system-source-directory "org.melusina.atelier"))))
    (dolist (pathname (directory (merge-pathnames "*.text" fixture-directory)))
      (validate-one-pretty-print-fixture pathname))))

(define-testcase testsuite-pretty-printer ()
  "Run all pretty-printer tests."
  (validate-pretty-print-fixtures))

;;;; End of file 'pretty-printer.lisp'
