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
             (unless (string-equal token "nil") (parse-integer token))))
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
;;;; Emacs Reformatting Tool
;;;;

(defvar *emacs-pretty-printer-configuration*
  "(progn
     (package-initialize)
     (require 'slime)
     (require 'slime-indentation)
     (setq indent-tabs-mode nil
           lisp-lambda-list-keyword-parameter-alignment t
           lisp-loop-indent-forms-like-keywords t
           lisp-loop-indent-subclauses nil)
     (put 'concatenate 'common-lisp-indent-function '(4 &body)))"
  "Default Elisp s-expression string eval'd before Emacs reformats a file.
Loads SLIME indentation contrib and sets indentation variables to match
the project's Emacs conventions.  Used by REFORMAT-FILE-WITH-EMACS as
the default for its :ELISP-SETUP keyword argument.")

(defun reformat-file-with-emacs (pathname &key (elisp-setup
                                                *emacs-pretty-printer-configuration*))
  "Reformat PATHNAME using Emacs in batch mode.
Opens the file, optionally evaluates ELISP-SETUP, indents the entire
buffer, saves and exits.  Modifies the file in place.
Signals an error if emacs is not found on PATH."
  (declare (type (or pathname string) pathname)
           (type (or string null) elisp-setup)
           (values pathname))
  (let* ((pathname (pathname pathname))
         (namestring (namestring pathname))
         (args (list "--batch"
                     "--eval" "(require 'cl-indent)"
                     "--eval" (format nil "(find-file ~S)" namestring))))
    (when elisp-setup
      (setf args (append args (list "--eval" elisp-setup))))
    (setf args (append args
                       (list "--eval" "(indent-region (point-min) (point-max))"
                             "--eval" "(save-buffer)"
                             "--eval" "(kill-emacs)")))
    (uiop:run-program (cons "emacs" args)
                       :output nil
                       :error-output nil)
    pathname))


;;;;
;;;; Reference Fixture Path
;;;;

(defun reference-fixture-pathname ()
  "Return the pathname of the pretty-printer reference fixture."
  (merge-pathnames #p"test/fixtures/pretty-print/reference.lisp"
                   (asdf:system-source-directory "org.melusina.atelier")))


;;;;
;;;; Testcases
;;;;

(define-testcase validate-reference-fixture-fixed-point ()
  "Verify that the reference fixture is a fixed point of REFORMAT-FILE.
Copies the fixture to a temp file, reformats it, and asserts the content
is unchanged."
  (let* ((fixture (reference-fixture-pathname))
         (original (alexandria:read-file-into-string
                    fixture :external-format :utf-8)))
    (uiop:with-temporary-file (:pathname tmp :type "lisp" :keep nil)
      (with-open-file (stream tmp :direction :output
                                  :if-exists :supersede
                                  :external-format :utf-8)
        (write-string original stream))
      (atelier:reformat-file tmp)
      (let ((reformatted (alexandria:read-file-into-string
                          tmp :external-format :utf-8)))
        (assert-string= original reformatted)))))

(define-testcase validate-reference-fixture-emacs-fixed-point ()
  "Verify that the reference fixture is a fixed point of Emacs indentation.
Copies the fixture to a temp file, reformats with Emacs, and asserts the
content is unchanged.  Skips if emacs is not on PATH."
  (unless (ignore-errors
           (uiop:run-program '("which" "emacs") :output :string)
           t)
    (format t "~&SKIP: emacs not found on PATH.~%")
    (return-from validate-reference-fixture-emacs-fixed-point nil))
  (let* ((fixture (reference-fixture-pathname))
         (original (alexandria:read-file-into-string
                    fixture :external-format :utf-8)))
    (uiop:with-temporary-file (:pathname tmp :type "lisp" :keep nil)
      (with-open-file (stream tmp :direction :output
                                  :if-exists :supersede
                                  :external-format :utf-8)
        (write-string original stream))
      (reformat-file-with-emacs tmp)
      (let ((reformatted (alexandria:read-file-into-string
                          tmp :external-format :utf-8)))
        (assert-string= original reformatted)))))

(define-testcase testsuite-pretty-printer ()
  "Run all pretty-printer tests via fixture auto-discovery."
  (validate-pretty-printer-fixtures)
  (validate-reference-fixture-fixed-point)
  (validate-reference-fixture-emacs-fixed-point))

;;;; End of file `pretty-printer.lisp'
