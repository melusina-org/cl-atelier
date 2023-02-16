;;;; org.melusina.atelier.asd — Lisp system for Atelier

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(asdf:defsystem #:org.melusina.atelier
  :description "An atelier for Lisp developers"
  :author "Michaël Le Barbier"
  :depends-on (#:alexandria
	       #:cl-ppcre
	       #:org.melusina.rashell
	       #:osicat
	       #:trivia)
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "utilities")
		 (:file "configuration")
		 (:file "license")
		 (:file "parameter")
		 (:file "template")
		 (:file "lint")
		 (:module "inspector"
		  :components
		  ((:file "codestyle-0001")
		   (:file "codestyle-0002")
		   (:file "codestyle-0003")
		   (:file "codestyle-0004")
		   (:file "codestyle-0005")
		   (:file "codestyle-0006")))
		 (:file "main")))))

(asdf:defsystem #:org.melusina.atelier/testsuite
  :description "Testsuite for an atelier for Lisp developers"
  :author "Michaël Le Barbier"
  :depends-on (#:alexandria
	       #:org.melusina.atelier
	       #:org.melusina.confidence)
  :components
  ((:module "testsuite"
    :components ((:file "package")
		 (:file "utilities")
		 (:file "parameter")
		 (:file "template")
		 (:module "inspector"
		  :components
		  ((:file "codestyle-0001")
		   (:file "codestyle-0002")
		   (:file "codestyle-0003")
		   (:file "codestyle-0004")
		   (:file "codestyle-0005")
		   (:file "codestyle-0006")))
		 (:file "lint")
		 (:file "entrypoint")))))

;;;; End of file `org.melusina.atelier.asd'
