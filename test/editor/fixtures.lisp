;;;; fixtures.lisp — Auto-discovered fixture runner for canonicalize tests

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/editor)


;;;;
;;;; Fixture Directory
;;;;

(defun canonicalize-fixture-directory ()
  "Return the pathname to the canonicalize fixture directory."
  (merge-pathnames
   #p"test/fixtures/editor/canonicalize/"
   (asdf:system-source-directory :org.melusina.atelier)))


;;;;
;;;; Fixture Parsing
;;;;

(defun parse-canonicalize-fixture (pathname)
  "Parse a canonicalize fixture file. Return (VALUES input expected description).
The file format is: YAML front-matter, ---, input, ---, expected."
  (declare (type pathname pathname)
           (values string string string))
  (let ((content (uiop:read-file-string pathname :external-format :utf-8)))
    (let* ((lines (uiop:split-string content :separator '(#\Newline)))
           ;; Skip first line (---) of front-matter
           (fm-start (position "---" lines :test #'string=))
           ;; Find the second --- (end of front-matter / start of input)
           (input-start (position "---" lines :test #'string=
                                               :start (1+ fm-start)))
           ;; Find the third --- (end of input / start of expected)
           (expected-start (position "---" lines :test #'string=
                                                  :start (1+ input-start))))
      (unless (and fm-start input-start expected-start)
        (error "Malformed fixture file ~A: expected three --- separators." pathname))
      ;; Extract description from front-matter
      (let ((description
              (loop :for line :in (subseq lines (1+ fm-start) input-start)
                    :when (alexandria:starts-with-subseq "description:" line)
                    :return (string-trim '(#\Space #\Tab #\")
                                         (subseq line (length "description:"))))))
        ;; Extract input and expected, trimming trailing empty lines
        (flet ((join-section (start end)
                 (string-trim '(#\Newline #\Space)
                              (format nil "~{~A~^~%~}"
                                      (subseq lines (1+ start) end)))))
          (let ((input (join-section input-start expected-start))
                (expected (join-section expected-start (length lines))))
            (values input expected (or description (pathname-name pathname)))))))))


;;;;
;;;; Fixture Runner
;;;;

(define-testcase validate-one-canonicalize-fixture (pathname)
  "Run one canonicalize fixture: normalize input, compare to expected,
assert idempotency, assert round-trip."
  (multiple-value-bind (input expected description)
      (parse-canonicalize-fixture pathname)
    (declare (ignore description))
    ;; 1. Normalize input and compare to expected
    (let* ((form (atelier/editor:read-toplevel-form-from-string input))
           (normalized (atelier/editor:normalize-toplevel-form form))
           (actual (atelier/editor:write-toplevel-form-to-string normalized)))
      (assert-string= expected actual)
      ;; 2. Idempotency: normalizing the result yields the same text
      (let* ((form2 (atelier/editor:read-toplevel-form-from-string actual))
             (norm2 (atelier/editor:normalize-toplevel-form form2))
             (text2 (atelier/editor:write-toplevel-form-to-string norm2)))
        (assert-string= actual text2))
      ;; 3. Round-trip: write(read(actual)) = actual
      (let* ((form3 (atelier/editor:read-toplevel-form-from-string actual))
             (text3 (atelier/editor:write-toplevel-form-to-string form3)))
        (assert-string= actual text3)))))

(define-testcase validate-canonicalize-fixtures ()
  "Auto-discover and run all .text fixtures in the canonicalize directory."
  (let* ((dir (canonicalize-fixture-directory))
         (fixtures (sort (directory (merge-pathnames "*.text" dir))
                         #'string< :key #'pathname-name)))
    (dolist (fixture fixtures)
      (validate-one-canonicalize-fixture fixture))))

;;;; End of file `fixtures.lisp'
