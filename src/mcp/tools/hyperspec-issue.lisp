;;;; hyperspec-issue.lisp — lisp:hyperspec-issue MCP tool + resource

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool hyperspec-issue (&key issue-name)
  (:description
   "Look up an X3J13 issue in the locally-installed HyperSpec.
    Returns the issue writeup as HTML. Reads from the local
    filesystem only — never makes network requests.")
  (:resource :uri       "lisp://hyperspec/issues/{issue-name}"
             :name      "HyperSpec X3J13 issue"
             :mime-type :text/html)
  (declare (type string issue-name))
  (unless (hyperspec-available-p)
    (error 'mcp-error
           :message "HyperSpec is not installed locally. Install via: sudo port install lisp-hyperspec"))
  (multiple-value-bind (content relative-path)
      (hyperspec-issue-lookup issue-name)
    (declare (ignore relative-path))
    (unless content
      (error 'resource-not-found
             :uri (concatenate 'string "lisp://hyperspec/issues/" issue-name)
             :message (format nil "No X3J13 issue named ~A." issue-name)))
    (list (cons "issue" issue-name)
          (cons "content" content))))

;;;; End of file `hyperspec-issue.lisp'
