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

(in-package #:org.melusina.atelier/testsuite)

(rashell:define-test grep (pattern pathname)
  ((fixed-string :flag "-F")
   (ignore-case :flag "-i"))
  (:program #p"/usr/bin/grep"
	    :rest (list pattern pathname)))

(rashell:define-test sh (pathname)
  nil
  (:program #p"/bin/sh"
   :rest (list pathname)))	    

(rashell:define-utility git-init ()
  nil
  (:program #p"/opt/local/bin/git"
   :rest '("init")))

(define-testcase ensure-development-script-satisfy-formal-requirements (pathname)
  (assert-t (rashell:test '(:has-kind :regular) pathname))
  (assert-t (rashell:test '(:has-at-least-permission #o700) pathname)))
  
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
  (rashell:with-temporary-directory (pathname)
    (with-fixed-parameter-bindings ()
      (git-init :directory pathname)
      (atelier:new-lisp-project pathname))
    (ensure-a-lisp-project-is-created-with-a-valid-testsuite pathname)
    (ensure-a-lisp-project-is-created-with-a-valid-documentation pathname)))

(define-testcase testsuite-template ()
  (ensure-a-lisp-project-is-created-fully-functional))

;;;; End of file `template.lisp'
