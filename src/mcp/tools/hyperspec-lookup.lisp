;;;; hyperspec-lookup.lisp — lisp:hyperspec-lookup MCP tool + resource

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool hyperspec-lookup (&key symbol-name)
  (:description
   "Look up a Common Lisp symbol in the locally-installed HyperSpec.
    Returns the CLHS dictionary entry as HTML. Reads from the local
    filesystem only — never makes network requests.")
  (:resource :uri       "lisp://hyperspec/symbol/{symbol-name}"
             :name      "HyperSpec symbol entry"
             :mime-type :text/html)
  (declare (type string symbol-name))
  (unless (hyperspec-available-p)
    (error 'mcp-error
           :message "HyperSpec is not installed locally. Install via: sudo port install lisp-hyperspec"))
  (multiple-value-bind (content relative-path)
      (hyperspec-symbol-lookup symbol-name)
    (unless content
      (error 'resource-not-found
             :uri (concatenate 'string "lisp://hyperspec/symbol/" symbol-name)
             :message (format nil "No HyperSpec entry for ~A." symbol-name)))
    (list (cons "symbol" symbol-name)
          (cons "path" (or relative-path ""))
          (cons "content" content))))

;;;; End of file `hyperspec-lookup.lisp'
