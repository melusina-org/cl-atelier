;;;; package.lisp — Package for the Atelier Lisp System

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

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
   #:list-templates
   #:find-template
   #:list-template-parameter-names
   #:list-licenses
   #:write-template
   #:initialize
   #:new-lisp-project
   #:new-lisp-file
   
   ;; License
   #:license
   #:license-id
   #:license-name
   #:license-header
   #:license-text
   
   ;; Lint
   #:*linter-interactive-p*
   #:hint-at-file
   #:hint-at-file-line
   #:lint-file
   #:lint
   #:linter
   #:extensive-linter
   #:canonical-source-linter
   #:inline-comment-linter
   #:block-comment-linter
   #:plain-line-comment-linter
   #:plain-block-comment-linter
   #:file-inspectors
   #:content-inspectors
   #:line-inspectors
   #:define-linter
   #:decorate-line
   #:decorate-block
   ;; Inspectors
   #:define-inspector
   #:list-inspectors
   ))

(in-package #:atelier)

(declaim (ftype (function nil (values null)) initialize))

;;;; End of file `package.lisp'
