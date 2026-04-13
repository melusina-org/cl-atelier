;;;; hyperspec.lisp — Local HyperSpec lookup infrastructure

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

;;; HyperSpec tools read from a local MacPorts installation.
;;; INV-33: Never make network requests.
;;; INV-34: Gracefully unavailable when not installed.

(defvar *hyperspec-root*
  #p"/opt/local/share/doc/lisp/HyperSpec-7-0/HyperSpec/"
  "Root directory of the local HyperSpec installation.")

(defvar *hyperspec-symbol-map* nil
  "Hash table mapping uppercase symbol names to relative Body file paths.
   Lazily loaded from Map_Sym.txt.")

(defvar *hyperspec-issue-map* nil
  "Hash table mapping uppercase issue names to relative Issues file paths.
   Lazily loaded from Map_IssX.txt.")

(defun hyperspec-available-p ()
  "Return T if the local HyperSpec installation is available."
  (let ((map-sym (merge-pathnames "Data/Map_Sym.txt" *hyperspec-root*)))
    (probe-file map-sym)))

(defun %parse-map-file (pathname)
  "Parse a HyperSpec map file into a hash table.
   Format: alternating lines of NAME and RELATIVE-PATH."
  (let ((table (make-hash-table :test 'equal)))
    (with-open-file (stream pathname :direction :input
                                     :external-format :latin-1)
      (loop :for name = (read-line stream nil nil)
            :for path = (read-line stream nil nil)
            :while (and name path)
            :do (setf (gethash (string-trim '(#\Space #\Tab) name) table)
                      (string-trim '(#\Space #\Tab) path))))
    table))

(defun %ensure-symbol-map ()
  "Ensure *hyperspec-symbol-map* is loaded."
  (unless *hyperspec-symbol-map*
    (let ((map-file (merge-pathnames "Data/Map_Sym.txt" *hyperspec-root*)))
      (unless (probe-file map-file)
        (error 'mcp-error :message "HyperSpec not available: Map_Sym.txt not found."))
      (setf *hyperspec-symbol-map* (%parse-map-file map-file))))
  *hyperspec-symbol-map*)

(defun %ensure-issue-map ()
  "Ensure *hyperspec-issue-map* is loaded."
  (unless *hyperspec-issue-map*
    (let ((map-file (merge-pathnames "Data/Map_IssX.txt" *hyperspec-root*)))
      (unless (probe-file map-file)
        (error 'mcp-error :message "HyperSpec not available: Map_IssX.txt not found."))
      (setf *hyperspec-issue-map* (%parse-map-file map-file))))
  *hyperspec-issue-map*)

(defun %read-hyperspec-file (relative-path)
  "Read a HyperSpec HTML file given a path relative to Data/.
   Returns the file content as a string."
  ;; Map files use paths like ../Body/f_mapcar.htm relative to Data/
  (let ((full-path (merge-pathnames relative-path
                                    (merge-pathnames "Data/" *hyperspec-root*))))
    (unless (probe-file full-path)
      (return-from %read-hyperspec-file nil))
    (with-open-file (stream full-path :direction :input
                                      :external-format :latin-1)
      (let ((content (make-string (file-length stream))))
        (read-sequence content stream)
        content))))

(defun hyperspec-symbol-lookup (symbol-name)
  "Look up SYMBOL-NAME in the local HyperSpec.
   Returns (VALUES html-content relative-path) or (VALUES NIL NIL)."
  (let* ((map (%ensure-symbol-map))
         (relative-path (gethash (string-upcase symbol-name) map)))
    (unless relative-path
      (return-from hyperspec-symbol-lookup (values nil nil)))
    (let ((content (%read-hyperspec-file relative-path)))
      (values content relative-path))))

(defun hyperspec-issue-lookup (issue-name)
  "Look up ISSUE-NAME in the local HyperSpec issues.
   Returns (VALUES html-content relative-path) or (VALUES NIL NIL)."
  (let* ((map (%ensure-issue-map))
         (relative-path (gethash (string-upcase issue-name) map)))
    (unless relative-path
      (return-from hyperspec-issue-lookup (values nil nil)))
    (let ((content (%read-hyperspec-file relative-path)))
      (values content relative-path))))

(defun hyperspec-issue-names ()
  "Return a sorted list of all X3J13 issue names."
  (let ((map (%ensure-issue-map))
        (names nil))
    (maphash (lambda (name path)
               (declare (ignore path))
               (push name names))
             map)
    (sort names #'string<)))

;;;; End of file `hyperspec.lisp'
