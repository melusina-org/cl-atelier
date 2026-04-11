;;;; transcript-filesystem.lisp — Slow tests that touch the filesystem

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

(defmacro with-temp-transcript ((transcript-var path-var) &body body)
  "Create a transcript in a fresh temp directory, bind TRANSCRIPT-VAR
   and PATH-VAR to the instance and its pathname, run BODY, and clean
   up regardless of outcome."
  `(let* ((%dir (fresh-temp-directory))
          (,transcript-var (make-transcript :session-id "test" :directory %dir))
          (,path-var (transcript-path ,transcript-var)))
     (unwind-protect (progn ,@body)
       (delete-temp-directory %dir))))

(define-testcase validate-transcript-file-opens ()
  "A fresh transcript creates its directory and writes a file."
  (unless (transcript-filesystem-available-p)
    (return-from validate-transcript-file-opens))
  (with-temp-transcript (transcript path)
    (write-transcript-entry transcript (list :kind :test))
    (assert-t* (probe-file path))))

(define-testcase validate-transcript-appends-entries ()
  "Each write-transcript-entry appends one entry; entries are readable back."
  (unless (transcript-filesystem-available-p)
    (return-from validate-transcript-appends-entries))
  (with-temp-transcript (transcript path)
    (write-transcript-entry transcript (list :kind :tools-call :tool "atelier:ping"))
    (write-transcript-entry transcript (list :kind :tools-result :tool "atelier:ping"))
    (let ((entries (read-transcript-entries path)))
      (assert-eql 2 (length entries))
      (assert-eq :tools-call  (getf (first entries) :kind))
      (assert-eq :tools-result (getf (second entries) :kind)))))

(define-testcase validate-transcript-read-raw ()
  "read-transcript returns the raw file contents as a string."
  (unless (transcript-filesystem-available-p)
    (return-from validate-transcript-read-raw))
  (with-temp-transcript (transcript path)
    (write-transcript-entry transcript (list :kind :marker))
    (let ((raw (read-transcript path)))
      (assert-t* (stringp raw))
      (assert-t* (search ":MARKER" raw)))))

(define-testcase validate-transcript-filesystem-tests ()
  (validate-transcript-file-opens)
  (validate-transcript-appends-entries)
  (validate-transcript-read-raw))

;;;; End of file `transcript-filesystem.lisp'
