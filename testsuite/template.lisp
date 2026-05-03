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

(define-testcase ensure-development-script-satisfy-formal-requirements (pathname)
  (assert-t (file-regular-p pathname))
  (assert-t (file-has-required-permissions-p pathname #o700)))
  
(define-testcase ensure-a-lisp-project-is-created-with-a-valid-testsuite (pathname)
  (ensure-development-script-satisfy-formal-requirements
   (merge-pathnames #p"development/testsuite" pathname))
  (assert-t
   (grep "confidence" (merge-pathnames #p"example.asd" pathname)
	 :fixed-string t
	 :ignore-case t))
  (assert-t
   (sh "development/testsuite" :directory pathname)))

(define-testcase ensure-a-lisp-project-is-created-with-a-valid-documentation (pathname)
  (ensure-development-script-satisfy-formal-requirements
   (merge-pathnames #p"development/makedoc" pathname))
  (assert-t
   (sh "development/makedoc" :directory pathname)))

(define-testcase ensure-a-lisp-project-is-created-fully-functional ()
  (with-temporary-directory (pathname)
    (with-fixed-parameter-bindings ()
      (git-init :directory pathname)
      (atelier:new-lisp-project pathname))
    (ensure-a-lisp-project-is-created-with-a-valid-testsuite pathname)
    (ensure-a-lisp-project-is-created-with-a-valid-documentation pathname)))

(define-testcase testsuite-template ()
  (ensure-a-lisp-project-is-created-fully-functional))

;;;; End of file `template.lisp'
