;;;; pretty-printer.lisp — Testsuite for the Atelier pretty-printer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

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
Returns (values front-matter input-form column right-margins expected-strings)."
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
;;;; Testcase — delegates to autofix.lisp auto-discovery
;;;;

(define-testcase testsuite-pretty-printer ()
  "Run all pretty-printer tests via fixture auto-discovery."
  (validate-pretty-printer-fixtures))

;;;; End of file `pretty-printer.lisp'
