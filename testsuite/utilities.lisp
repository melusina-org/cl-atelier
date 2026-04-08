;;;; utilities.lisp — Testing the utilities of Atelier

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)

(defmacro with-fixed-parameter-bindings
    ((&key (copyright-holder "A. U. Thor")
     (copyright-year "2017–2022")
     (project-filename  "net.cl-user.acme.example")
           (project-name "Example")
     (project-description "Example for Atelier test")
           (project-long-description
      "The Example for the Atelier is used for tests.")
           (homepage "https://cl-user.net/acme/example")
           (license :cecill-b))
     &body body-forms)
  `(let ((atelier:*parameter-bindings*
     (list
      (cons :copyright-holder ,copyright-holder)
            (cons :copyright-year ,copyright-year)
      (cons :project-filename ,project-filename)
            (cons :project-name ,project-name)
      (cons :project-description ,project-description)
            (cons :project-long-description ,project-long-description)
            (cons :homepage ,homepage)
            (cons :license ,license))))
     ,@body-forms))




;;;;
;;;; Fixture Directory
;;;;

(defun testsuite-fixtures-directory ()
  "Return the pathname to the testsuite fixtures directory."
  (merge-pathnames #p"testsuite/fixtures/"
                   (asdf:system-source-directory "org.melusina.atelier")))

(defun fixture (kind class-name &optional (name "baseline"))
  "Return the pathname of a test fixture.
KIND is :INSPECTOR, :MAINTAINER, or :PRETTY-PRINTER.
CLASS-NAME is a symbol (for inspector/maintainer) or a string (for pretty-printer).
NAME defaults to \"baseline\".

Examples:
  (fixture :maintainer 'atelier:fix-bare-lambda)
  (fixture :maintainer 'atelier:fix-bare-lambda \"chain\")
  (fixture :inspector 'atelier:check-earmuffs \"bad\")
  (fixture :pretty-printer \"flet-single-binding\")"
  (let ((subdir (ecase kind
                  (:inspector "inspector")
                  (:maintainer "maintainer")
                  (:pretty-printer "pretty-print")))
        (class-dir (etypecase class-name
                     (symbol (string-downcase (symbol-name class-name)))
                     (string class-name)))
        (extension (ecase kind
                     (:inspector "lisp")
                     ((:maintainer :pretty-printer) "text"))))
    (merge-pathnames
     (if (eq kind :pretty-printer)
         (make-pathname :directory (list :relative "testsuite" "fixtures" subdir)
                        :name class-dir :type extension)
         (make-pathname :directory (list :relative "testsuite" "fixtures" subdir class-dir)
                        :name name :type extension))
     (asdf:system-source-directory "org.melusina.atelier"))))

(defun inspector-fixture (inspector-name &optional (name "baseline"))
  "Return the pathname of fixture NAME for INSPECTOR-NAME."
  (fixture :inspector inspector-name name))

(defun maintainer-fixture (maintainer-name &optional (name "baseline"))
  "Return the pathname of fixture NAME for MAINTAINER-NAME."
  (fixture :maintainer maintainer-name name))

(defun pretty-printer-fixture (name)
  "Return the pathname of pretty-printer fixture NAME."
  (fixture :pretty-printer name))


;;;;
;;;; Fixture Auto-Discovery
;;;;

(defun discover-fixtures (kind extension)
  "Return an alist of (symbol . list-of-fixture-pathnames) for KIND.
KIND is \"inspector\", \"maintainer\", or \"pretty-print\".
Walks testsuite/fixtures/KIND/*/ and maps each subdirectory name to
its files matching *.EXTENSION."
  (let ((base (merge-pathnames
               (make-pathname :directory (list :relative "testsuite" "fixtures" kind))
               (asdf:system-source-directory "org.melusina.atelier"))))
    (loop :for dir :in (uiop:subdirectories base)
          :for dir-name = (car (last (pathname-directory dir)))
          :for sym = (find-symbol (string-upcase dir-name) :atelier)
          :for files = (directory (merge-pathnames
                                  (make-pathname :name :wild :type extension) dir))
          :when (and sym files)
          :collect (cons sym files))))

(defun discover-maintainer-fixtures ()
  "Return an alist of (maintainer-symbol . list-of-fixture-pathnames)."
  (discover-fixtures "maintainer" "text"))

(defun discover-inspector-fixtures ()
  "Return an alist of (inspector-symbol . list-of-fixture-pathnames)."
  (discover-fixtures "inspector" "lisp"))

(defun discover-pretty-printer-fixtures ()
  "Return a list of fixture pathnames from testsuite/fixtures/pretty-print/."
  (let ((base (merge-pathnames #p"testsuite/fixtures/pretty-print/"
                               (asdf:system-source-directory "org.melusina.atelier"))))
    (directory (merge-pathnames "*.text" base))))

(defun read-maintainer-fixture (pathname)
  "Read a maintainer fixture from PATHNAME.
Returns (values inspector-name input-form expected-form) where INSPECTOR-NAME
is interned in the ATELIER package from the front-matter inspector: field."
  (declare (type pathname pathname))
  (multiple-value-bind (front-matter documents)
      (atelier:read-file-documents-with-yaml-front-matter pathname)
    (let* ((inspector-str (cdr (assoc :inspector front-matter)))
           (inspector-name (when inspector-str
                             (find-symbol (string-upcase inspector-str) :atelier)))
           (input-form (read-from-string (atelier:join-lines (first documents))))
           (expected-form (read-from-string (atelier:join-lines (second documents)))))
      (values inspector-name input-form expected-form))))


;;;;
;;;; File Utilities
;;;;

(defun file-regular-p (pathname)
  (and (uiop:file-exists-p pathname) t))

(defun file-mode (pathname)
  "Return the Unix file mode of PATHNAME."
  (parse-integer
   (uiop:run-program (list "stat" #+darwin "-f%Op" #+linux "-c%a" (namestring pathname))
         :output '(:string :stripped t))
   :radix 8))

(defun file-has-required-permissions-p (pathname required-permissions)
  "Predicate that recognises if file under PATHNAME has at least the REQUIRED-PERMISSIONS."
  (let ((actual-permissions
    (file-mode pathname)))
    (eq required-permissions (logand actual-permissions required-permissions))))


;;;;
;;;; Testsuite Utilities
;;;;

(define-testcase validate-first-line ()
  (assert-string= "A" (atelier::first-line (atelier::join-lines '("A" "B"))))
  (assert-string= "A" (atelier::first-line "A"))
  (let ((string
    (atelier::join-lines '("A" "B"))))
    (setf (atelier::first-line string) "C")
    (assert-string= (atelier::join-lines '("C" "B")) string))
  (let ((string
    (atelier::join-lines '("A" "B"))))
    (assert-string= (atelier::join-lines '("C" "B"))
        (atelier::edit-first-line string "C"))))

(define-testcase validate-last-line ()
  (assert-string= "B" (atelier::last-line (atelier::join-lines '("A" "B"))))
  (assert-string= "B" (atelier::last-line (atelier::join-lines '("A" "B" ""))))
  (assert-string= "A" (atelier::last-line "A"))
  (let ((string "A"))
    (setf (atelier::last-line string) "C")
    (assert-string= "C" string))
  (let ((string
    (atelier::join-lines '("A" "B"))))
    (setf (atelier::last-line string) "C")
    (assert-string=
     (atelier::join-lines '("A" "C"))
     string))
  (let ((string
    (atelier::join-lines '("A" "B" ""))))
    (setf (atelier::last-line string) "C")
    (assert-string= (atelier::join-lines '("A" "C" "")) string))
  (let ((string "A"))
    (assert-string=
     "C"
     (atelier::edit-last-line string "C")))
  (let ((string
    (atelier::join-lines '("A" "B"))))
    (assert-string=
     (atelier::join-lines '("A" "C"))
     (atelier::edit-last-line string "C")))
  (let ((string
    (atelier::join-lines '("A" "B" ""))))
    (assert-string=
     (atelier::join-lines '("A" "C" ""))
     (atelier::edit-last-line string "C"))))

(define-testcase testsuite-utilities ()
  (validate-first-line)
  (validate-last-line))

;;;; End of file `utilities.lisp'
