;;;; hyperspec-issues.lisp — lisp:hyperspec-issues MCP tool + resource

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool hyperspec-issues ()
  (:description
   "List all X3J13 issue names from the locally-installed HyperSpec.
    Returns a sorted list of issue name strings.")
  (:resource :uri       "lisp://hyperspec/issues"
             :name      "HyperSpec X3J13 issue index"
             :mime-type :application/json)
  (unless (hyperspec-available-p)
    (error 'mcp-error
           :message "HyperSpec is not installed locally. Install via: sudo port install lisp-hyperspec"))
  (hyperspec-issue-names))

;;;; End of file `hyperspec-issues.lisp'
