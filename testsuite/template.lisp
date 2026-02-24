;;;; template.lisp — Tests for the template functionalities

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

(defmacro run-predicate (command &key directory)
  `(multiple-value-bind (standard-output error-output exit-code)
       (uiop:run-program ,command :ignore-error-status t :directory ,directory)
     (declare (ignore standard-output error-output))
     (zerop exit-code)))

(defun grep (pattern pathname &key fixed-string ignore-case directory)
  (let ((command (list "grep")))
    (when fixed-string (push "-F" command))
    (when ignore-case (push "-i" command))
    (setf command (nconc (nreverse command) (list pattern (namestring pathname))))
    (run-predicate command :directory directory)))

(defun sh (pathname &key directory)
  (run-predicate (list "sh" (namestring pathname))
		 :directory directory))

(defun git-init (&key directory)
 (run-predicate (list "git" "init")
		:directory directory))

(declaim (inline call-with-temporary-directory))
(defun call-with-temporary-directory (function)
  (flet ((make-directory-pathname ()
	  (uiop:ensure-directory-pathname
	   (merge-pathnames
	    (format nil "atelier-test-~6X/" (random #xFFFFFF))
	    (uiop:temporary-directory))))
	 (validate-directory-pathname (pathname)
	   (let ((directory-name
		   (car (last (pathname-directory pathname)))))
	     (and (uiop:ensure-directory-pathname pathname)
		  (zerop (search "atelier-test-" directory-name :test #'string=))
		  (= (length directory-name) #.(length "atelier-test-123456"))))))
    (let ((directory
	    (make-directory-pathname)))
      (ensure-directories-exist directory)
      (unwind-protect
	   (funcall function directory)
	(uiop:delete-directory-tree directory
				    :validate #'validate-directory-pathname
				    :if-does-not-exist :ignore)))))

(defmacro with-temporary-directory ((pathname) &body body)
  `(locally (declare (inline call-with-temporary-directory))
     (call-with-temporary-directory (lambda (,pathname) ,@body))))

(define-testcase ensure-development-script-satisfy-formal-requirements (pathname)
  (assert-t (file-regular-p pathname))
  (assert-t (file-has-required-permissions-p pathname #o700)))

(define-testcase ensure-a-lisp-project-is-created-for-the-golden-path ()
  (with-temporary-directory (pathname)
    (with-fixed-parameter-bindings nil (atelier:new-lisp-project pathname))
    (let ((asdf:*central-registry* (cons pathname asdf:*central-registry*)))
      (asdf:clear-system "net.cl-user.acme.example")
      (asdf:clear-system "net.cl-user.acme.example/testsuite")
      (asdf:clear-system "net.cl-user.acme.example/development")
      (asdf:load-system "net.cl-user.acme.example/testsuite")
      (asdf:operate 'asdf:test-op "net.cl-user.acme.example")
      (uiop:symbol-call "EXAMPLE/TESTSUITE" "RUN-ALL-TESTS")
      (asdf:load-system "net.cl-user.acme.example/development")
      (uiop:symbol-call "EXAMPLE/DEVELOPMENT" "LINT"))))
  
(define-testcase ensure-a-lisp-project-is-created-with-a-valid-testsuite (pathname)
  (ensure-development-script-satisfy-formal-requirements
   (merge-pathnames #p"development/testsuite" pathname))
  (assert-t
   (sh "development/testsuite" :directory pathname)))

(define-testcase ensure-a-lisp-project-is-created-with-a-valid-documentation (pathname)
  (ensure-development-script-satisfy-formal-requirements
   (merge-pathnames #p"development/makedoc" pathname))
  (assert-t
   (sh "development/makedoc" :directory pathname)))

(define-testcase ensure-a-lisp-project-is-created-with-a-valid-package-structure
    (pathname &key project-filename system-name package-name)
  (loop :for source-file
	:in (list (make-pathname :name project-filename
				 :type "asd")
		  (make-pathname :directory (list :relative "src")
				 :name "package"
				 :type "lisp")
		  (make-pathname :directory (list :relative "testsuite")
				 :name "package"
				 :type "lisp")
		  (make-pathname :directory (list :relative "doc")
				 :name project-filename
				 :type "texinfo"))
	:do (assert-t (file-regular-p (merge-pathnames source-file pathname))))
  (let ((system-definition
	  (make-pathname :name project-filename :type "asd")))
    (assert-t
     (grep "org.melusina.confidence" (merge-pathnames system-definition pathname)
	   :fixed-string t
	   :ignore-case t))
    (assert-t
     (grep (concatenate 'string system-name "/testsuite")
	   (merge-pathnames system-definition pathname)
	   :fixed-string t
	   :ignore-case t))
    (assert-t
     (grep (concatenate 'string system-name "/development")
	   (merge-pathnames system-definition pathname)
	   :fixed-string t
	   :ignore-case t))
    (assert-t
     (grep (concatenate 'string "(defpackage #:" package-name)
	   (merge-pathnames (make-pathname :directory (list :relative "src")
					   :name "package"
					   :type "lisp" )
			    pathname)
	   :fixed-string t))
    (assert-t
     (grep (concatenate 'string "(defpackage #:" package-name "/testsuite")
	   (merge-pathnames (make-pathname :directory (list :relative "testsuite")
					   :name "package"
					   :type "lisp" )
			    pathname)
	   :fixed-string t))))

(define-testcase ensure-a-lisp-project-is-created-fully-functional (&key project-filename package-name system-name)
  (with-temporary-directory (pathname)
    (with-fixed-parameter-bindings (:project-filename project-filename)
      (git-init :directory pathname)
      (atelier:new-lisp-project pathname))
    (ensure-a-lisp-project-is-created-with-a-valid-testsuite pathname)
    (ensure-a-lisp-project-is-created-with-a-valid-documentation pathname)
    (ensure-a-lisp-project-is-created-with-a-valid-package-structure
     pathname
     :project-filename project-filename
     :system-name system-name
     :package-name package-name)))

(define-testcase testsuite-template ()
  (ensure-a-lisp-project-is-created-for-the-golden-path)
  (ensure-a-lisp-project-is-created-fully-functional
   :project-filename "example"
   :system-name "example"
   :package-name "example")
  (ensure-a-lisp-project-is-created-fully-functional
   :project-filename "net.cl-user.example"
   :system-name "net.cl-user.example"
   :package-name "example"))

;;;; End of file `template.lisp'
