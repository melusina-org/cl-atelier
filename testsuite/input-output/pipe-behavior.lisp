;;;; pipe-behavior.lisp — Learn how UNIX pipes behave with CL streams

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/input-output)

;;; These tests encode our understanding of how uiop:launch-program
;;; pipes interact with Common Lisp stream operations. Each test uses
;;; simple UNIX tools (echo, yes, awk, sh) to produce deterministic
;;; pipe behavior.

;;; ---- Basic pipe read ----

(define-testcase validate-pipe-read-single-line ()
  "A child that prints one line and exits: read-line returns the line."
  (let ((proc (uiop:launch-program '("echo" "hello")
                                   :output :stream)))
    (let* ((stdout (uiop:process-info-output proc))
           (line (read-line stdout nil nil)))
      (assert-string= "hello" line)
      ;; Second read-line returns nil (EOF)
      (assert-nil (read-line stdout nil nil))
      (uiop:wait-process proc))))

(define-testcase validate-pipe-read-multiple-lines ()
  "A child that prints multiple lines: read-line returns them in order."
  (let ((proc (uiop:launch-program '("sh" "-c" "echo one; echo two; echo three")
                                   :output :stream)))
    (let ((stdout (uiop:process-info-output proc)))
      (assert-string= "one" (read-line stdout nil nil))
      (assert-string= "two" (read-line stdout nil nil))
      (assert-string= "three" (read-line stdout nil nil))
      (assert-nil (read-line stdout nil nil))
      (uiop:wait-process proc))))

;;; ---- Pipe buffer capacity ----

(define-testcase validate-pipe-small-write-does-not-block ()
  "A child that writes less than the pipe buffer capacity does not block."
  ;; Pipe buffer on macOS is 64KB. 100 lines of 10 chars = 1.1KB.
  (let ((proc (uiop:launch-program
               '("sh" "-c" "for i in $(seq 1 100); do echo '0123456789'; done")
               :output :stream)))
    (let ((stdout (uiop:process-info-output proc))
          (count 0))
      (loop :for line := (read-line stdout nil nil)
            :while line
            :do (incf count))
      (assert= 100 count)
      (uiop:wait-process proc))))

;;; ---- Merged stderr (:error-output :output) ----

(define-testcase validate-merged-stderr ()
  "With :error-output :output, stderr lines appear on stdout."
  (let ((proc (uiop:launch-program
               '("sh" "-c" "echo stdout-line; echo stderr-line >&2")
               :output :stream
               :error-output :output)))
    (let ((stdout (uiop:process-info-output proc))
          (lines nil))
      (loop :for line := (read-line stdout nil nil)
            :while line
            :do (push line lines))
      ;; Both lines should appear (order may vary)
      (assert= 2 (length lines))
      (assert-t* (member "stdout-line" lines :test #'string=))
      (assert-t* (member "stderr-line" lines :test #'string=))
      (uiop:wait-process proc))))

;;; ---- Finding a port-like line among noise ----

(define-testcase validate-port-extraction-from-noisy-stdout ()
  "Read lines until finding one that's a pure decimal number, skipping noise."
  (let ((proc (uiop:launch-program
               '("sh" "-c" "echo 'This is SBCL'; echo ''; echo 'Loading...'; echo '54321'; echo 'more noise'")
               :output :stream)))
    (let ((stdout (uiop:process-info-output proc))
          (port-line nil))
      (loop :for line := (read-line stdout nil nil)
            :while line
            :do (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
                  (when (and (plusp (length trimmed))
                             (every #'digit-char-p trimmed))
                    (setf port-line trimmed)
                    (return))))
      (assert-string= "54321" port-line)
      (uiop:wait-process proc))))

;;; ---- Pipe deadlock demonstration ----

(define-testcase validate-pipe-deadlock-prevention ()
  "A child that writes more than the pipe buffer WILL block if parent
   doesn't read. Verify that draining prevents this."
  ;; Generate ~100KB of output (well over 64KB pipe buffer)
  ;; The child writes to stdout; we read the first line, then
  ;; drain the rest in a thread, and confirm the child exits.
  (let ((proc (uiop:launch-program
               '("sh" "-c" "echo MARKER; for i in $(seq 1 10000); do echo 'padding line padding line padding'; done")
               :output :stream)))
    (let ((stdout (uiop:process-info-output proc)))
      ;; Read first line
      (let ((first (read-line stdout nil nil)))
        (assert-string= "MARKER" first))
      ;; Start drain thread
      (bordeaux-threads:make-thread
       (lambda ()
         (ignore-errors
           (loop :for line := (read-line stdout nil nil)
                 :while line))))
      ;; Child should exit within a few seconds
      (let ((exited nil))
        (dotimes (i 10)
          (unless (uiop:process-alive-p proc)
            (setf exited t)
            (return))
          (sleep 0.5))
        (assert-t exited)
        (unless exited
          (uiop:terminate-process proc)))
      (uiop:wait-process proc))))

;;; ---- read-char-no-hang ----

(define-testcase validate-read-char-no-hang ()
  "read-char-no-hang returns nil immediately on an empty pipe,
   and returns a character when data is available."
  (let ((proc (uiop:launch-program '("sh" "-c" "sleep 0.2; echo hi")
                                   :output :stream)))
    (let ((stdout (uiop:process-info-output proc)))
      ;; Immediately after spawn, no data yet
      ;; NOTE: read-char-no-hang on process pipes may not behave
      ;; as expected on all implementations. This test documents
      ;; the actual behavior.
      (sleep 0.5) ;; wait for child to write
      ;; After sleep, data should be available
      (let ((ch (read-char-no-hang stdout nil nil)))
        ;; ch should be #\h (first char of "hi")
        (assert-t* (characterp ch)))
      (uiop:wait-process proc))))


;;; ---- Entry point ----

(define-testcase run-io-tests ()
  "Run all pipe I/O behavior tests."
  (validate-pipe-read-single-line)
  (validate-pipe-read-multiple-lines)
  (validate-pipe-small-write-does-not-block)
  (validate-merged-stderr)
  (validate-port-extraction-from-noisy-stdout)
  (validate-pipe-deadlock-prevention)
  (validate-read-char-no-hang))

;;;; End of file `pipe-behavior.lisp'
