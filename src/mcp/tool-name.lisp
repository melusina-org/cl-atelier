;;;; tool-name.lisp — Derive MCP tool name from a Lisp symbol

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp-kernel)

;;; Tool names are derived from the symbol's home package and name per
;;; the rules in references/define-tool-macro.md:
;;;
;;; 1. Shortest nickname if any, else package name.
;;; 2. Lowercased.
;;; 3. If the result ends in "/mcp", strip the "/mcp" suffix.
;;; 4. Concatenate with ":" and the (lowercased) symbol name.

(defun %package-designator-for-tool-name (package)
  "Return the shortest name for PACKAGE — a nickname when shorter than
   the primary name, otherwise the primary name. Returned as a string."
  (let ((primary   (package-name package))
        (nicknames (package-nicknames package)))
    (if nicknames
        (reduce (lambda (a b) (if (< (length a) (length b)) a b))
                (cons primary nicknames))
        primary)))

(defun %strip-mcp-suffix (name)
  "If NAME ends in \"/mcp\", return NAME without that suffix; otherwise
   return NAME unchanged."
  (let ((suffix "/mcp"))
    (if (and (>= (length name) (length suffix))
             (string= name suffix
                      :start1 (- (length name) (length suffix))))
        (subseq name 0 (- (length name) (length suffix)))
        name)))

(defun derive-tool-name-from-symbol (symbol)
  "Derive the MCP tool name string from SYMBOL.
   See references/define-tool-macro.md for the full rule.
   Signals ERROR if SYMBOL has no home package."
  (let ((package (symbol-package symbol)))
    (unless package
      (error "Cannot derive a tool name from ~S: symbol has no home package."
             symbol))
    (let* ((pkg-name (%package-designator-for-tool-name package))
           (lower    (string-downcase pkg-name))
           (stripped (%strip-mcp-suffix lower))
           (sym-name (string-downcase (symbol-name symbol))))
      (concatenate 'string stripped ":" sym-name))))

;;;; End of file `tool-name.lisp'
