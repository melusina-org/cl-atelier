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
		   (:file "fix-project-identification")))
		 (:file "main")))))

(asdf:defsystem #:org.melusina.atelier/editor
  :description "Projectional editor for managed Common Lisp files."
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:org.melusina.atelier)
  :components
  ((:module "src/editor"
    :serial t
    :components ((:file "package")
		 (:file "conditions")
		 (:file "toplevel-form")
		 (:file "eclector-client")
		 (:file "read-form")
		 (:file "write-form")
		 (:file "canonicalize")))))

(asdf:defsystem #:org.melusina.atelier/child-worker
  :description "Child SBCL worker for Atelier MCP eval."
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:closer-mop)
  :components
  ((:module "src/child-worker"
    :serial t
    :components ((:file "package")
		 (:file "introspection")
		 (:file "documentation")
		 (:file "entry-point")))))

(asdf:defsystem #:org.melusina.atelier/mcp
  :description "MCP server skeleton for Atelier."
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:alexandria
	       #:bordeaux-threads
	       #:uiop
	       #:usocket
	       #:flexi-streams
	       #:com.inuoe.jzon
	       #:org.melusina.atelier
	       #:org.melusina.atelier/editor)
  :components
  ((:module "src/mcp"
    :serial t
    :components ((:file "package")
		 (:file "conditions")
		 (:file "protocol-version")
		 (:file "json-util")
		 (:file "tool-name")
		 (:file "input-schema")
		 (:file "uri-template")
		 (:file "tool")
		 (:file "message")
		 (:file "define-tool")
		 (:file "transcript-render")
		 (:file "transcript")
		 (:file "image-connection")
		 (:file "swank-protocol")
		 (:file "dispatcher")
		 (:file "server")
		 (:file "hyperspec")
		 (:module "tools"
		  :serial t
		  :components ((:file "probe-environment")
			       (:file "list-inspectors")
			       (:file "list-maintainers")
			       (:file "list-systems")
			       (:file "inspector-detail")
			       (:file "maintainer-detail")
			       (:file "transcript-resources")
			       (:file "canonicalize-form")
			       (:file "eval-form")
			       (:file "list-packages")
			       (:file "list-package-symbols")
			       (:file "describe-symbol")
			       (:file "find-definition")
			       (:file "run-tests-fresh")
			       (:file "run-tests-in-child")
			       (:file "select-restart")
			       (:file "abort-debug")
			       (:file "backtrace")
			       (:file "eval-in-frame")
			       (:file "quickload")
			       (:file "system-info")
			       (:file "system-apropos")
			       (:file "list-testcases")
			       (:file "run-testcase")
			       (:file "apropos-search")
			       (:file "hyperspec-lookup")
			       (:file "hyperspec-issue")
			       (:file "hyperspec-issues")
			       (:file "macroexpand-form")
			       (:file "disassemble-symbol")
			       (:file "compile-form")
			       ;; Slice 014: xref, CLOS inspector, trace
			       (:file "who-calls")
			       (:file "who-references")
			       (:file "who-binds")
			       (:file "who-specializes")
			       (:file "who-macroexpands")
			       (:file "inspect-class")
			       (:file "trace-function")
			       (:file "untrace-function")
			       (:file "who-tests")
			       (:file "run-impacted")))))))

(asdf:defsystem #:org.melusina.atelier/testsuite
  :description "Testsuite for an atelier for Lisp developers"
  :author "Michaël Le Barbier"
  :depends-on (#:alexandria
	       #:org.melusina.atelier
	       #:org.melusina.atelier/editor
	       #:org.melusina.atelier/child-worker
	       #:org.melusina.atelier/mcp
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
		 (:module "editor"
		  :serial t
		  :components ((:file "package")
			       (:file "eclector-capability")
			       (:file "lint-string")
			       (:file "toplevel-form")
			       (:file "write-form")
			       (:file "normalize")
			       (:file "fixtures")
			       (:file "fresh-sbcl-load")
			       (:file "entrypoint")))
		 (:module "mcp"
		  :serial t
		  :components ((:file "package")
			       (:file "utilities")
			       (:file "jzon-round-trip")
			       (:file "tool-name-derivation")
			       (:file "input-schema-derivation")
			       (:file "uri-template")
			       (:file "define-tool-macro")
			       (:file "message-parsing")
			       (:file "dispatcher")
			       (:file "protocol-handshake")
			       (:file "tool-invocation")
			       (:file "resource-read")
			       (:file "image-connection")
			       (:file "transcript-encoding")
			       (:file "transcript-filesystem")
			       (:file "transcript-torn-write")
			       (:file "registry-counts")
			       (:file "swank-protocol")
			       (:file "canonicalize-tool")
			       (:file "child-tests")
			       (:file "debugger-tests")
			       (:file "asdf-tools-tests")
			       (:file "documentation-tools-tests")
			       (:file "xref-tools-tests")
			       (:file "inspect-trace-tests")
			       (:file "journey-tests")
			       (:file "fresh-sbcl-load")
			       (:file "entrypoint")))
		 (:file "entrypoint")))))

(asdf:defsystem #:org.melusina.atelier/testsuite/input-output
  :description "Exploratory tests for pipe I/O behavior"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:bordeaux-threads
	       #:uiop
	       #:org.melusina.confidence)
  :components
  ((:module "testsuite/input-output"
    :serial t
    :components ((:file "package")
		 (:file "pipe-behavior")))))

(asdf:defsystem #:org.melusina.atelier/testsuite/swank
  :description "Exploratory tests for SWANK protocol behavior"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:usocket
	       #:flexi-streams
	       #:org.melusina.atelier/mcp
	       #:org.melusina.confidence)
  :components
  ((:module "testsuite/swank"
    :serial t
    :components ((:file "package")
		 (:file "wire-protocol")))))

(asdf:defsystem #:org.melusina.atelier/development
  :description "Development tools for Atelier"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:org.melusina.atelier)
  :components
  ((:module "libexec/lisp"
    :components ((:file "development")))))

(asdf:defsystem #:org.melusina.atelier/mcp-server
  :description "Build MCP Server from ASDF System"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:org.melusina.atelier/mcp)
  :build-operation program-op
  :build-pathname "atelier_mcp"
  :entry-point "atelier/mcp:serve-two-way-stream")

;;;; End of file `org.melusina.atelier.asd'
