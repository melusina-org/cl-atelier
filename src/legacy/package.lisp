;;;; package.lisp — Package for the Atelier legacy linter

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:atelier/legacy
  (:use #:common-lisp)
  (:import-from #:alexandria
   #:read-file-into-string
   #:make-keyword
   #:lastcar)
  (:import-from #:atelier
   ;; Utilities
   #:string-match
   #:string-suffix-p
   #:string-prefix-p
   #:string-lines
   #:join-lines
   #:first-line
   #:last-line
   #:read-file-into-list
   #:find-regular-files
   #:string-words
   #:count-string-words
   #:indent
   #:break-down
   #:edit-first-line
   #:edit-last-line
   ;; Parameter system
   #:*parameter-bindings*
   #:parameter-replacement-text
   ;; License
   #:find-license
   #:license-header
   #:list-licenses
   ;; Template
   #:initialize
   #:template-repository-empty-p
   ;; Finding types (for bridge)
   #:file-finding
   #:line-finding
   ;; Decorating
   #:read-file-documents-with-yaml-front-matter)
  (:export
   ;; Hint classes
   #:hint
   #:hint-at-file
   #:hint-at-file-line
   ;; Anomaly condition
   #:anomaly
   ;; Linter
   #:*linter-interactive-p*
   #:lint
   #:lint-file
   #:lint-contents
   #:lint-lines
   #:linter
   #:extensive-linter
   #:canonical-source-linter
   #:inline-comment-linter
   #:block-comment-linter
   #:plain-line-comment-linter
   #:plain-block-comment-linter
   #:define-plain-linter
   #:find-plain-linter
   #:file-inspectors
   #:content-inspectors
   #:line-inspectors
   #:decorate-line-comment
   #:decorate-block-comment
   ;; Legacy inspector system
   #:define-inspector
   #:find-inspector
   #:list-inspectors
   ;; Bridge
   #:hint-to-finding
   #:lint-with-findings))

;;;; End of file `package.lisp'
