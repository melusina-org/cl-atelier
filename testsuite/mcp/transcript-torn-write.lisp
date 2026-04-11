;;;; transcript-torn-write.lisp — Torn-write recovery tests

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

(define-testcase validate-transcript-torn-write-recovery ()
  "A file that ends with a partial entry is readable up to the last
   complete entry. The reader must not signal or return garbage."
  (unless (transcript-filesystem-available-p)
    (return-from validate-transcript-torn-write-recovery))
  (let* ((dir (fresh-temp-directory))
         (transcript (make-transcript :session-id "torn" :directory dir))
         (path (transcript-path transcript)))
    (unwind-protect
         (progn
           (write-transcript-entry transcript (list :kind :tools-call :tool "p"))
           (write-transcript-entry transcript (list :kind :tools-result :tool "p"))
           (write-transcript-entry transcript (list :kind :tools-call :tool "q"))
           ;; Manually append a torn tail — simulates SIGKILL mid-entry.
           (with-open-file (s path :direction :output
                                   :if-exists :append
                                   :external-format :utf-8)
             (write-string "(:seq 99 :kind :tools-call :tool \"bro" s))
           (let ((entries (read-transcript-entries path)))
             (assert-eql 3 (length entries))
             (assert-eq :tools-call (getf (first entries) :kind))
             (assert-eq :tools-result (getf (second entries) :kind))
             (assert-eq :tools-call (getf (third entries) :kind))))
      (delete-temp-directory dir))))

(define-testcase validate-transcript-write-protocol-flushes-per-entry ()
  "Each entry is complete on disk after WRITE-TRANSCRIPT-ENTRY returns;
   a subsequent read sees the entry even without an explicit flush."
  (unless (transcript-filesystem-available-p)
    (return-from validate-transcript-write-protocol-flushes-per-entry))
  (let* ((dir (fresh-temp-directory))
         (transcript (make-transcript :session-id "flush" :directory dir))
         (path (transcript-path transcript)))
    (unwind-protect
         (progn
           (write-transcript-entry transcript (list :kind :first))
           ;; Read immediately without any manual flush — the per-entry
           ;; finish-output makes this safe.
           (let ((after-first (read-transcript-entries path)))
             (assert-eql 1 (length after-first)))
           (write-transcript-entry transcript (list :kind :second))
           (let ((after-second (read-transcript-entries path)))
             (assert-eql 2 (length after-second))))
      (delete-temp-directory dir))))

(define-testcase validate-transcript-torn-write-tests ()
  (validate-transcript-torn-write-recovery)
  (validate-transcript-write-protocol-flushes-per-entry))

;;;; End of file `transcript-torn-write.lisp'
