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
		 (:file "pretty-printer")
		 (:file "write-back")
		 (:module "inspectors"
		  :components
		  ((:file "check-file-encoding")
		   (:file "check-spdx-license-header")
		   (:file "check-trailing-whitespace")
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
		   (:file "check-when-not")
		   (:file "check-system-naming")
		   (:file "check-test-mirror")))
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
		   (:file "fix-deprecated-names")
		   (:file "fix-when-not")
		   (:file "fix-single-branch-if")))
		 (:module "editor"
		  :components ((:file "package")
			       (:file "conditions")
			       (:file "toplevel-form")
			       (:file "eclector-client")
			       (:file "read-form")
			       (:file "write-form")
			       (:file "canonicalize")))
		 (:file "main")
		 (:file "git")))))

(asdf:defsystem #:org.melusina.atelier/testsuite
  :description "Test suite for the Atelier for Lisp developers"
  :author "Michaël Le Barbier"
  :depends-on (#:org.melusina.confidence
	       #:org.melusina.atelier)
  :components
  ((:module "testsuite"
    :serial t
    :components ((:file "package")
		 (:file "utilities")
		 (:file "parameter")
		 (:file "license")
		 (:file "template")
		 (:file "finding")
		 (:file "resolution")
		 (:file "inspector")
		 (:file "maintainer")
		 (:file "asdf")
		 (:file "runner")
		 (:file "pretty-printer")
		 (:file "write-back")
		 (:module "inspectors"
		  :components
		  ((:file "check-file-encoding")
		   (:file "check-spdx-license-header")
		   (:file "check-trailing-whitespace")
		   (:file "check-mixed-indentation")
		   (:file "check-labels-for-flet")
		   (:file "check-header-line")
		   (:file "check-footer-line")
		   (:file "check-project-identification")
		   (:file "check-system-naming")))
		 (:module "maintainers"
		  :components
		  ((:file "fix-mixed-indentation")
		   (:file "fix-labels-to-flet")
		   (:file "fix-header-line")
		   (:file "fix-footer-line")
		   (:file "fix-deprecated-names")))
		 (:file "autofix")
		 (:file "git")
		 (:module "editor"
		  :components ((:file "package")
			       (:file "eclector-capability")
			       (:file "lint-string")
			       (:file "toplevel-form")
			       (:file "write-form")
			       (:file "normalize")
			       (:file "fixtures")
			       (:file "entrypoint")))
		 (:file "entrypoint")))))

(asdf:defsystem #:org.melusina.atelier/development
  :description "Development tools for Atelier"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:bordeaux-threads
	       #:uiop
	       #:org.melusina.atelier
	       #:org.melusina.confidence)
  :components
  ((:module "libexec/lisp"
    :components ((:file "development")
		 (:file "uiop-pipe-behavior")))))

;;;; End of file `org.melusina.atelier.asd'
