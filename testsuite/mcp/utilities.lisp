;;;; utilities.lisp — Test utilities for the Atelier MCP testsuite

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

(defmacro with-isolated-registries (&body body)
  "Execute BODY with the four MCP registries dynamically rebound to
   fresh empty hash-tables. Test-defined tools and resources do not
   leak into the global production state. Hard fix for the test-
   registry pollution pattern documented in
   product/knowledge/patterns.md."
  `(let ((atelier/mcp:*tool-registry* (make-hash-table :test 'equal))
         (atelier/mcp:*concrete-resource-registry* (make-hash-table :test 'equal))
         (atelier/mcp:*template-resource-registry* (make-hash-table :test 'equal)))
     (let ((atelier/mcp::*input-schema-cache*  (make-hash-table :test 'eq))
           (atelier/mcp::*input-schema-source* (make-hash-table :test 'eq)))
       ,@body)))

(defun transcript-filesystem-available-p ()
  "Return T when the per-test transcript directory is writable.
   Slow tests that touch the filesystem use this as a skip predicate."
  (let ((dir (uiop:ensure-directory-pathname
              (format nil "/tmp/atelier-mcp-fs-probe-~A/" (random 1000000)))))
    (handler-case
        (progn
          (ensure-directories-exist dir)
          (with-open-file (s (merge-pathnames "probe" dir)
                             :direction :output
                             :if-exists :supersede)
            (write-string "probe" s))
          (delete-file (merge-pathnames "probe" dir))
          (uiop:delete-empty-directory dir)
          t)
      (error () nil))))

(defun fresh-temp-directory ()
  "Return a fresh, existing temporary directory pathname for use by
   filesystem tests. Caller is responsible for cleanup."
  (let ((dir (uiop:ensure-directory-pathname
              (format nil "/tmp/atelier-mcp-test-~A/" (random 100000000)))))
    (ensure-directories-exist dir)
    dir))

(defun delete-temp-directory (dir)
  "Recursively delete DIR if it exists. Defensive — wraps errors."
  (handler-case
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)
    (error () nil)))

(defun parse-json-line (line)
  "Convenience: parse one line of JSON and return the hash-table."
  (atelier/mcp:decode-from-string line))

(defun count-non-blank-lines (string)
  "Return the number of non-blank lines in STRING. Used by tests
   that drive serve-two-way-stream and need to count responses."
  (count-if (lambda (line) (plusp (length (string-trim '(#\Space #\Tab #\Return) line))))
            (uiop:split-string string :separator '(#\Newline))))

(defun split-non-blank-lines (string)
  "Split STRING into lines and drop blank ones."
  (loop :for line :in (uiop:split-string string :separator '(#\Newline))
        :for trimmed := (string-trim '(#\Space #\Tab #\Return) line)
        :when (plusp (length trimmed)) :collect trimmed))

;;;; End of file `utilities.lisp'
