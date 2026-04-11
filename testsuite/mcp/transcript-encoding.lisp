;;;; transcript-encoding.lisp — Tests for the in-memory transcript walkers

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

(defun %sample-entries ()
  "Return a small list of transcript entries for encoder tests."
  (list (list :seq 1 :timestamp "2026-04-10T14:32:11Z"
              :kind :tools-call :tool "atelier:ping")
        (list :seq 2 :timestamp "2026-04-10T14:32:11Z"
              :kind :tools-result :tool "atelier:ping" :result "pong")))

(define-testcase validate-sexp-to-json-walker ()
  "sexp-to-json-entries walks a plist sequence into a JSON array."
  (let ((json (sexp-to-json-entries (%sample-entries))))
    (assert-t* (search "\"seq\":1" json))
    (assert-t* (search "\"kind\":\"tools-call\"" json))
    (assert-t* (search "\"result\":\"pong\"" json))
    (assert-eq #\[ (char json 0))
    (assert-eq #\] (char json (1- (length json))))))

(define-testcase validate-sexp-to-markdown-renderer ()
  "sexp-to-markdown-entries produces a Markdown document with one
   section per entry."
  (let ((md (sexp-to-markdown-entries (%sample-entries) :session-id "abc")))
    (assert-t* (search "# MCP session abc" md))
    (assert-t* (search "## Entry 1" md))
    (assert-t* (search "## Entry 2" md))
    (assert-t* (search "**tool**: atelier:ping" md))))

(define-testcase validate-sexp-walkers-handle-empty-sequence ()
  "An empty entry sequence produces an empty JSON array and a heading-only
   Markdown document."
  (let ((json (sexp-to-json-entries '()))
        (md (sexp-to-markdown-entries '() :session-id "empty")))
    (assert-string= "[]" json)
    (assert-t* (search "# MCP session empty" md))))

(define-testcase validate-transcript-encoding-tests ()
  (validate-sexp-to-json-walker)
  (validate-sexp-to-markdown-renderer)
  (validate-sexp-walkers-handle-empty-sequence))

;;;; End of file `transcript-encoding.lisp'
