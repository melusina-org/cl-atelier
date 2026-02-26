;;;; utilities.lisp — Testing the utilities of Atelier

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

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
  

(defmacro with-fixed-linter-environment (&body body-forms)
  `(let ((atelier:*parameter-bindings*
	   '((:copyright-holder . "A. U. Thor")
             (:copyright-year . "2017–2022")
	     (:project-filename . "net.cl-user.acme.example")
             (:project-name . "Example")
	     (:project-description . "Example for Atelier test")
             (:project-long-description .
	      "The Example for the Atelier test.")
             (:homepage . "https://cl-user.net/acme/example")
             (:license . :cecill-b)))
	 (atelier::*hint-pathname*
	   "test.lisp"))
     (handler-bind
	 ((atelier::anomaly
	    (lambda (c)
	      (declare (ignore c))
	      (invoke-restart 'atelier::autocorrect))))
       ,@body-forms)))


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
