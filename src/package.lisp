;;;; package.lisp — Package for the Atelier Lisp System

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(defpackage #:atelier
  (:use #:common-lisp)
  (:import-from #:alexandria
   #:hash-table-keys
   #:lastcar
   #:make-keyword
   #:read-file-into-string
   #:assoc-value
   #:ensure-list)
  (:export
   #:*parameter-bindings*
   #:parameter-keyword
   #:parameter-replacement-text

   ;; Template
   #:template
   #:template-name
   #:template-identifier
   #:template-description
   #:template-text
   #:list-templates
   #:find-template
   #:list-template-parameter-names
   #:list-licenses
   #:write-template
   #:new-lisp-project
   #:new-lisp-file

   ;; Templates
   #:license
   #:readme
   #:lisp-asdf
   #:lisp-development
   #:lisp-docstrings
   #:lisp-system-package
   #:lisp-system-entrypoint
   #:lisp-test-package
   #:lisp-test-entrypoint
   #:lisp-source
   #:shell-stdlib
   #:shell-script
   #:shell-source
   #:lisp-development-makedoc
   #:texinfo
   #:lisp-development-lint
   #:lisp-development-build
   #:lisp-development-test
   #:lisp-system-entry-point
   #:lisp-test-entry-point
   #:project-files
   #:lisp-system-scaffolding
   #:devops-actions
   #:lisp-devops-actions
   #:lisp-documentation
   #:lisp-git-ignore
   #:lisp-project

   ;; License
   #:license
   #:license-id
   #:license-name
   #:license-header
   #:license-text
   #:license-spdx-identifier
   #:find-license

   ;; Finding hierarchy
   #:finding
   #:file-finding
   #:line-finding
   #:region-finding
   #:syntax-finding
   #:make-file-finding
   #:make-line-finding
   #:make-region-finding
   #:make-syntax-finding
   #:finding-inspector
   #:finding-severity
   #:finding-observation
   #:finding-rationale
   #:finding-file
   #:finding-line
   #:finding-column
   #:finding-end-line
   #:finding-end-column
   #:finding-source-text
   #:finding-start-line
   #:finding-cst-node
   #:finding-cst-root

   ;; Resolution hierarchy
   #:resolution
   #:text-resolution
   #:syntax-resolution
   #:agent-resolution
   #:composite-resolution
   #:make-text-resolution
   #:make-syntax-resolution
   #:make-agent-resolution
   #:make-composite-resolution
   #:resolution-maintainer
   #:resolution-finding
   #:resolution-kind
   #:resolution-description
   #:resolution-replacement
   #:resolution-transform
   #:resolution-cst-node
   #:resolution-prompt
   #:resolution-transforms

   ;; Inspector registry
   #:inspector
   #:file-inspector
   #:line-inspector
   #:region-inspector
   #:syntax-inspector
   #:*inspectors*
   #:define-inspector
   #:define-file-inspector
   #:define-line-inspector
   #:define-region-inspector
   #:define-syntax-inspector
   #:find-inspector
   #:symbol-inspector
   #:inspector-name
   #:inspector-level
   #:inspector-description
   #:list-inspectors
   #:inspect-file

   ;; Maintainer registry
   #:maintainer
   #:automatic-maintainer
   #:agent-maintainer
   #:*maintainers*
   #:define-maintainer
   #:define-automatic-maintainer
   #:define-agent-maintainer
   #:find-maintainer
   #:symbol-maintainer
   #:maintainer-name
   #:maintainer-supersedes
   #:maintainer-kind
   #:maintainer-maturity
   #:maintainer-description
   #:list-maintainers

   ;; Protocol
   #:prepare-resolution
   #:resolve-finding

   ;; Pretty-printer
   #:*atelier-pprint-dispatch*
   #:pretty-print-form
   #:reformat-file
   #:reformat-system

   ;; Write-back engine
   #:apply-resolutions
   #:apply-resolutions-to-file
   #:compute-resolution-spans
   #:resolution-text-span

   ;; Concrete maintainers
   #:fix-trailing-whitespace
   #:fix-mixed-indentation
   #:fix-earmuffs
   #:fix-constant-naming
   #:fix-bare-loop-keywords
   #:fix-bare-lambda
   #:fix-labels-to-flet
   #:compute-flet-depth-table
   #:labels-transform-to-flet
   #:fix-header-line
   #:fix-footer-line
   #:fix-project-identification
   #:fix-deprecated-system-name
   #:fix-deprecated-component-name
   #:fix-when-not
   #:fix-single-branch-if
   #:fix-spdx-license-header
   #:fix-testsuite-package-name
   #:inspect-file
   #:inspect-line
   #:*current-pathname*
   #:perform-inspection
   #:perform-file-inspection
   #:perform-line-inspection
   #:perform-syntax-inspection
   #:read-file-into-line-vector

   ;; Git hooks
   #:install-pre-commit-hook
   #:pre-commit-hook-exists

   ;; Concrete finding subclasses
   #:encoding-finding
   #:spdx-license-header-finding
   #:trailing-whitespace-finding
   #:mixed-indentation-finding
   #:earmuffs-finding
   #:constant-naming-finding
   #:bare-lambda-finding
   #:bare-loop-keyword-finding
   #:spurious-labels-finding
   #:header-line-finding
   #:footer-line-finding
   #:project-identification-finding
   #:single-branch-if-finding
   #:single-form-progn-finding
   #:when-not-finding
   #:non-canonical-system-name-finding
   #:deprecated-system-name-finding
   #:deprecated-component-name-finding
   #:testsuite-package-name-finding
   #:missing-test-component-finding
   #:test-component-order-finding

   ;; Concrete inspectors
   #:check-file-encoding
   #:check-spdx-license-header
   #:check-trailing-whitespace
   #:check-mixed-indentation
   #:read-file-header
   #:check-earmuffs
   #:check-constant-naming
   #:check-bare-lambda
   #:check-loop-keywords
   #:check-labels-for-flet
   #:collect-calls-in-forms
   #:labels-call-graph-has-cycles-p
   #:check-header-line
   #:check-footer-line
   #:check-project-identification
   #:check-single-branch-if
   #:check-single-form-progn
   #:check-when-not
   #:check-system-naming
   #:check-test-mirror
   #:check-testsuite-package-name
   #:file-comment-prefix
   #:lisp-source-file-p
   #:canonical-project-identification-text

   ;; Syntax inspection infrastructure
   #:inspect-syntax
   #:parse-common-lisp
   #:parse-lisp-file
   #:string-to-line-vector
   #:make-syntax-finding-from-form
   #:source-position-to-line-column
   #:*current-cst-root*
   #:*current-line-vector*
   #:cst-form-operator
   #:cst-form-operator-p

   ;; ASDF integration
   #:project-configuration
   #:make-project-configuration
   #:project-configuration-copyright-holder
   #:project-configuration-copyright-year
   #:project-configuration-project-filename
   #:project-configuration-project-name
   #:project-configuration-project-description
   #:project-configuration-project-long-description
   #:project-configuration-homepage
   #:project-configuration-license
   #:read-project-configuration
   #:linter-configuration
   #:make-linter-configuration
   #:linter-configuration-disabled-inspectors
   #:linter-configuration-severity-overrides
   #:linter-configuration-indentation-style
   #:linter-configuration-maintainer-overrides
   #:linter-configuration-mirror-excluded-components
   #:linter-configuration-inspector-file-exclusions
   #:read-linter-configuration
   #:project-configuration-parameter-bindings
   #:*project-configuration*
   #:*linter-configuration*
   #:asdf-project-configuration
   #:asdf-linter-configuration
   #:resolution-proposed
   #:resolution-proposed-resolution
   #:resolution-proposed-finding
   #:resolution-proposed-maintainer-name
   #:effective-maintainer-disposition
   #:linter-op
   #:lint-system
   #:lint-string
   #:collect-sibling-systems
   #:collect-all-source-files

   ;; Utilities (used by legacy bridge)
   #:string-match
   #:string-suffix-p
   #:string-prefix-p
   #:string-lines
   #:join-lines
   #:first-line
   #:last-line
   #:read-file-into-list
   #:find-regular-files
   #:break-down
   #:edit-first-line
   #:edit-last-line
   #:string-words
   #:count-string-words
   #:indent
   #:read-file-documents-with-yaml-front-matter
   #:read-stream-documents-with-yaml-front-matter
   #:define-named-class

   ;; Initialization
   #:initialize
   #:initialized-p))

(in-package #:atelier)

(declaim (ftype (function nil (values null)) initialize))
(declaim (ftype (function nil (values (or nil t))) initialized-p))

;;;; End of file `package.lisp'
