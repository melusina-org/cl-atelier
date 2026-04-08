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

(defun inspector-fixture (inspector-name &optional (name "baseline"))
  "Return the pathname of fixture NAME for INSPECTOR-NAME.
INSPECTOR-NAME is a symbol like ATELIER:CHECK-BARE-LAMBDA.
NAME defaults to \"baseline\". Extension is .lisp."
  (declare (type symbol inspector-name)
           (type string name))
  (merge-pathnames
   (make-pathname :directory (list :relative "testsuite" "fixtures" "inspector"
                                   (string-downcase (symbol-name inspector-name)))
                  :name name :type "lisp")
   (asdf:system-source-directory "org.melusina.atelier")))

(defun maintainer-fixture (maintainer-name &optional (name "baseline"))
  "Return the pathname of fixture NAME for MAINTAINER-NAME.
MAINTAINER-NAME is a symbol like ATELIER:FIX-BARE-LAMBDA.
NAME defaults to \"baseline\". Extension is .text."
  (declare (type symbol maintainer-name)
           (type string name))
  (merge-pathnames
   (make-pathname :directory (list :relative "testsuite" "fixtures" "maintainer"
                                   (string-downcase (symbol-name maintainer-name)))
                  :name name :type "text")
   (asdf:system-source-directory "org.melusina.atelier")))

(defun pretty-printer-fixture (name)
  "Return the pathname of pretty-printer fixture NAME.
NAME is a string like \"flet-single-binding\". Extension is .text."
  (declare (type string name))
  (merge-pathnames
   (make-pathname :directory '(:relative "testsuite" "fixtures" "pretty-print")
                  :name name :type "text")
   (asdf:system-source-directory "org.melusina.atelier")))


;;;;
;;;; Fixture Auto-Discovery
;;;;

(defun discover-maintainer-fixtures ()
  "Return an alist of (maintainer-symbol . list-of-fixture-pathnames).
Walks testsuite/fixtures/maintainer/*/  and maps each subdirectory name
to its .text files. The subdirectory name is interned in the ATELIER package."
  (let ((base (merge-pathnames #p"testsuite/fixtures/maintainer/"
                               (asdf:system-source-directory "org.melusina.atelier"))))
    (loop :for dir :in (uiop:subdirectories base)
          :for dir-name = (car (last (pathname-directory dir)))
          :for name = (find-symbol (string-upcase dir-name) :atelier)
          :for files = (directory (merge-pathnames "*.text" dir))
          :when (and name files)
          :collect (cons name files))))

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
