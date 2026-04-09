;;;; org.melusina.atelier.asd — Lisp system for Atelier

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
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
	       #:trivia
	       #:uiop
	       #:eclector-concrete-syntax-tree)
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "utilities")
		 (:file "configuration")
		 (:file "license")
		 (:file "parameter")
		 (:file "template")
		 (:file "finding")
		 (:file "resolution")
		 (:file "inspector")
		 (:file "maintainer")
		 (:file "asdf")
		 (:file "runner")
		 (:module "inspectors"
		  :components
		  ((:file "check-file-encoding")
		   (:file "check-spdx-license-header")
		   (:file "check-trailing-whitespace")
		   (:file "check-line-length")
		   (:file "check-mixed-indentation")
		   (:file "check-earmuffs")
		   (:file "check-constant-naming")
		   (:file "check-bare-lambda")
		   (:file "check-loop-keywords")
		   (:file "check-labels-for-flet")
		   (:file "check-header-line")
		   (:file "check-footer-line")
		   (:file "check-project-identification")
		   (:file "check-single-branch-if")
		   (:file "check-single-form-progn")
		   (:file "check-when-not")))
		 (:file "pretty-printer")
		 (:file "write-back")
		 (:module "maintainers"
		  :components
		  ((:file "fix-trailing-whitespace")
		   (:file "fix-mixed-indentation")
		   (:file "fix-earmuffs")
		   (:file "fix-constant-naming")
		   (:file "fix-bare-loop-keywords")
		   (:file "fix-bare-lambda")
		   (:file "fix-labels-to-flet")
		   (:file "fix-header-line")
		   (:file "fix-footer-line")
		   (:file "fix-project-identification")
		   (:file "fix-line-too-long")))
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
		 (:file "license")
		 (:file "template")
		 (:file "finding")
		 (:file "resolution")
		 (:file "inspector")
		 (:file "maintainer")
		 (:file "runner")
		 (:file "asdf")
		 (:module "inspectors"
		  :components
		  ((:file "check-file-encoding")
		   (:file "check-spdx-license-header")
		   (:file "check-trailing-whitespace")
		   (:file "check-line-length")
		   (:file "check-mixed-indentation")
		   (:file "check-labels-for-flet")
		   (:file "check-header-line")
		   (:file "check-footer-line")
		   (:file "check-project-identification")))
		 (:file "pretty-printer")
		 (:file "write-back")
		 (:module "maintainers"
		  :components
		  ((:file "fix-mixed-indentation")
		   (:file "fix-labels-to-flet")
		   (:file "fix-header-line")
		   (:file "fix-footer-line")))
		 (:file "autofix")
		 (:file "entrypoint")))))

(asdf:defsystem #:org.melusina.atelier/development
  :description "Development tools for Atelier"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:org.melusina.atelier)
  :components
  ((:module "libexec/lisp"
    :components ((:file "development")))))

;;;; End of file `org.melusina.atelier.asd'
