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

(define-testcase ensure-development-script-satisfy-formal-requirements (pathname)
  (assert-t (rashell:test '(:has-kind :regular) pathname))
  (assert-t (rashell:test '(:has-at-least-permission #o700) pathname)))
  
(define-testcase ensure-a-lisp-project-is-created-with-a-valid-testsuite (pathname)
  (ensure-development-script-satisfy-formal-requirements
   (merge-pathnames #p"development/testsuite" pathname))
  (assert-t
   (grep "confidence" (merge-pathnames #p"example.asd" pathname)
	 :fixed-string t
	 :ignore-case t)))

(define-testcase ensure-a-lisp-project-is-created-fully-functional ()
  (rashell:with-temporary-directory (pathname)
    (with-fixed-parameter-bindings ()
      (atelier:new-lisp-project pathname))
    (ensure-a-lisp-project-is-created-with-a-valid-testsuite pathname)))

(define-testcase testsuite-template ()
  (ensure-a-lisp-project-is-created-fully-functional))

;;;; End of file `template.lisp'
