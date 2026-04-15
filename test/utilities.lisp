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
  (merge-pathnames #p"test/fixtures/"
                   (asdf:system-source-directory "org.melusina.atelier")))

(defun fixture (kind class-name &optional (name "baseline"))
  "Return the pathname of a test fixture.
KIND is :INSPECTOR, :AUTOFIX, or :PRETTY-PRINTER.
CLASS-NAME is a symbol (for inspector/autofix) or a string (for pretty-printer).
NAME defaults to \"baseline\".

Examples:
  (fixture :autofix 'atelier:fix-bare-lambda)
  (fixture :autofix 'atelier:fix-bare-lambda \"chain\")
  (fixture :inspector 'atelier:check-earmuffs \"bad\")
  (fixture :pretty-printer \"flet-single-binding\")"
  (let ((subdir (ecase kind
                  (:inspector "inspector")
                  (:autofix "autofix")
                  (:pretty-printer "pretty-print")))
        (class-dir (etypecase class-name
                     (symbol (string-downcase (symbol-name class-name)))
                     (string class-name)))
        (extension (ecase kind
                     (:inspector "lisp")
                     ((:autofix :pretty-printer) "text"))))
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

(defun autofix-cycle-fixture (maintainer-name &optional (name "baseline"))
  "Return the pathname of autofix-cycle fixture NAME for MAINTAINER-NAME.
Resolves against testsuite/fixtures/autofix/."
  (fixture :autofix maintainer-name name))

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
               (make-pathname :directory (list :relative "test" "fixtures" kind))
               (asdf:system-source-directory "org.melusina.atelier"))))
    (loop :for dir :in (uiop:subdirectories base)
          :for dir-name = (car (last (pathname-directory dir)))
          :for sym = (find-symbol (string-upcase dir-name) :atelier)
          :for files = (directory (merge-pathnames
                                  (make-pathname :name :wild :type extension) dir))
          :when (and sym files)
          :collect (cons sym files))))

(defun discover-autofix-cycle-fixtures ()
  "Return an alist of (maintainer-symbol . list-of-fixture-pathnames).
Walks testsuite/fixtures/autofix/*/ and maps each subdirectory name to its
.text files. If the autofix/ directory does not yet exist (pre-migration),
returns NIL rather than signalling.

Fixtures whose subdirectory name cannot be interned as an existing symbol
in :ATELIER are silently skipped — this gives an in-progress maintainer
directory a safe landing place that will start being exercised as soon as
the corresponding symbol exists."
  (let ((base (merge-pathnames
               (make-pathname :directory '(:relative "test" "fixtures" "autofix"))
               (asdf:system-source-directory "org.melusina.atelier"))))
    (when (uiop:directory-exists-p base)
      (discover-fixtures "autofix" "text"))))

(defun discover-inspector-fixtures ()
  "Return an alist of (inspector-symbol . list-of-fixture-pathnames)."
  (discover-fixtures "inspector" "lisp"))

(defun discover-pretty-printer-fixtures ()
  "Return a list of fixture pathnames from testsuite/fixtures/pretty-print/."
  (let ((base (merge-pathnames #p"test/fixtures/pretty-print/"
                               (asdf:system-source-directory "org.melusina.atelier"))))
    (directory (merge-pathnames "*.text" base))))

(define-condition autofix-cycle-fixture-error (error)
  ((pathname
    :initarg :pathname
    :reader autofix-cycle-fixture-error-pathname
    :type pathname
    :documentation "The fixture file that failed to load.")
   (reason
    :initarg :reason
    :reader autofix-cycle-fixture-error-reason
    :type string
    :documentation "A human-readable description of why the fixture failed to load."))
  (:report (lambda (condition stream)
             (format stream "Failed to load autofix-cycle fixture ~A: ~A"
                     (autofix-cycle-fixture-error-pathname condition)
                     (autofix-cycle-fixture-error-reason condition))))
  (:documentation
   "Signalled when an autofix-cycle fixture cannot be loaded, either because
its front-matter is missing a mandatory field or because its body does not
contain exactly three documents."))

(defun read-autofix-cycle-fixture (pathname)
  "Read an autofix-cycle fixture from PATHNAME.

An autofix-cycle fixture declares the complete diagnostic cycle
(INSPECTOR, FINDING, MAINTAINER, RESOLUTION) in its YAML front-matter,
followed by exactly three body documents:

  1. Input source code.
  2. Expected finding slot values as a plist.
  3. Expected fixed code.

Returns (values FRONT-MATTER INPUT-SOURCE EXPECTED-FINDING-SLOTS
EXPECTED-FIXED-CODE) where:

  FRONT-MATTER is the raw alist returned by
    ATELIER:READ-FILE-DOCUMENTS-WITH-YAML-FRONT-MATTER — it contains at
    least the keys :DESCRIPTION, :INSPECTOR, :FINDING, :MAINTAINER,
    :RESOLUTION as strings.

  INPUT-SOURCE is the first document as a single string.

  EXPECTED-FINDING-SLOTS is the second document parsed as a plist via
    READ-FROM-STRING. It may be NIL (empty plist) to skip slot checks.

  EXPECTED-FIXED-CODE is the third document as a single string.

Signals AUTOFIX-CYCLE-FIXTURE-ERROR when the front-matter is missing any
of the mandatory symbol fields (INSPECTOR, FINDING, MAINTAINER, RESOLUTION)
or when the body does not contain exactly three documents."
  (declare (type pathname pathname)
           (values list string t string))
  (flet ((fixture-error (reason)
           (error 'autofix-cycle-fixture-error
                  :pathname pathname
                  :reason reason)))
    (multiple-value-bind (front-matter documents)
        (atelier:read-file-documents-with-yaml-front-matter pathname)
      (dolist (key '(:inspector :finding :maintainer :resolution))
        (unless (cdr (assoc key front-matter))
          (fixture-error (format nil "front-matter missing mandatory field ~S" key))))
      (unless (= 3 (length documents))
        (fixture-error (format nil "body must contain exactly 3 documents, found ~D"
                               (length documents))))
      (let* ((input-source (atelier:join-lines (first documents)))
             (expected-finding-slots
               (read-from-string (atelier:join-lines (second documents))))
             (expected-fixed-code (atelier:join-lines (third documents))))
        (values front-matter input-source expected-finding-slots expected-fixed-code)))))


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
