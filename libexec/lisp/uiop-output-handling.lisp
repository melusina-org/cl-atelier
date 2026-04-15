;;;; uiop-output-handling.lisp — Discovery tests for UIOP subprocess output handling

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

;;;; These experiments validate assertions about UIOP's run-program
;;;; and launch-program output handling. Results inform slice 015
;;;; (MCP server reliability) design decisions.
;;;;
;;;; Run with: sbcl --non-interactive --load this-file.lisp

(require :uiop)

(defvar *test-command*
  '("/bin/sh" "-c"
    "printf 'A\\n'; printf 'X\\n' 1>&2; sleep 0.1; printf 'B\\n'; printf 'Y\\n' 1>&2")
  "Command producing interleaved stdout (A, B) and stderr (X, Y).")


;;;;
;;;; Experiment 1: Both streams as :string
;;;;

(format t "~%=== Experiment 1: :output :string, :error-output :string ===~%")
(multiple-value-bind (stdout stderr exit-code)
    (uiop:run-program *test-command*
                       :output :string :error-output :string
                       :ignore-error-status t)
  (format t "  stdout: ~S~%" stdout)
  (format t "  stderr: ~S~%" stderr)
  (format t "  exit:   ~D~%" exit-code)
  (assert (search "A" stdout) () "stdout should contain A")
  (assert (search "B" stdout) () "stdout should contain B")
  (assert (search "X" stderr) () "stderr should contain X")
  (assert (search "Y" stderr) () "stderr should contain Y")
  (format t "  PASS: Both streams captured separately.~%"))


;;;;
;;;; Experiment 2: :output as function, :error-output as :string
;;;;

(format t "~%=== Experiment 2: :output function, :error-output :string ===~%")
(let ((out-lines nil))
  (multiple-value-bind (out-result stderr exit-code)
      (uiop:run-program *test-command*
                         :output (lambda (stream)
                                   (loop :for line := (read-line stream nil nil)
                                         :while line
                                         :do (push line out-lines)))
                         :error-output :string
                         :ignore-error-status t)
    (declare (ignore out-result))
    (setf out-lines (nreverse out-lines))
    (format t "  stdout lines: ~S~%" out-lines)
    (format t "  stderr:       ~S~%" stderr)
    (format t "  exit:         ~D~%" exit-code)
    (assert (member "A" out-lines :test #'string=) () "stdout should contain A")
    (assert (member "B" out-lines :test #'string=) () "stdout should contain B")
    (assert (search "X" stderr) () "stderr should contain X")
    (format t "  PASS: Function processor works for stdout, string for stderr.~%")))


;;;;
;;;; Experiment 3: Both as functions (does UIOP support it?)
;;;;

(format t "~%=== Experiment 3: Both as functions ===~%")
(let ((out-lines nil)
      (err-lines nil))
  (handler-case
      (progn
        (uiop:run-program *test-command*
                           :output (lambda (stream)
                                     (loop :for line := (read-line stream nil nil)
                                           :while line
                                           :do (push line out-lines)))
                           :error-output (lambda (stream)
                                           (loop :for line := (read-line stream nil nil)
                                                 :while line
                                                 :do (push line err-lines)))
                           :ignore-error-status t)
        (setf out-lines (nreverse out-lines))
        (setf err-lines (nreverse err-lines))
        (format t "  stdout lines: ~S~%" out-lines)
        (format t "  stderr lines: ~S~%" err-lines)
        (format t "  PASS: Both functions accepted.~%")
        ;; Check if both actually got data
        (if (and out-lines err-lines)
            (format t "  Both captured data concurrently.~%")
            (format t "  WARNING: One stream may have been processed via temp file.~%")))
    (error (c)
      (format t "  ERROR: ~A~%" c)
      (format t "  UIOP does not support both as active functions.~%"))))


;;;;
;;;; Experiment 4: :error-output :output (merge stderr into stdout)
;;;;

(format t "~%=== Experiment 4: :error-output :output (merge) ===~%")
(let ((lines nil))
  (uiop:run-program *test-command*
                     :output (lambda (stream)
                               (loop :for line := (read-line stream nil nil)
                                     :while line
                                     :do (push line lines)))
                     :error-output :output
                     :ignore-error-status t)
  (setf lines (nreverse lines))
  (format t "  merged lines: ~S~%" lines)
  (assert (>= (length lines) 4) ()
          "merged stream should have at least 4 lines (A, X, B, Y)")
  (format t "  PASS: Merge captures both streams.~%"))


;;;;
;;;; Experiment 5: launch-program with :stream output
;;;;

(format t "~%=== Experiment 5: launch-program with stream processing ===~%")
(let* ((process-info (uiop:launch-program
                       *test-command*
                       :output :stream
                       :error-output :stream))
       (stdout-stream (uiop:process-info-output process-info))
       (stderr-stream (uiop:process-info-error-output process-info))
       (out-lines nil)
       (err-lines nil))
  ;; Read stdout (blocking)
  (loop :for line := (read-line stdout-stream nil nil)
        :while line
        :do (push line out-lines))
  ;; Read stderr (process should be done by now)
  (loop :for line := (read-line stderr-stream nil nil)
        :while line
        :do (push line err-lines))
  (uiop:wait-process process-info)
  (setf out-lines (nreverse out-lines))
  (setf err-lines (nreverse err-lines))
  (format t "  stdout lines: ~S~%" out-lines)
  (format t "  stderr lines: ~S~%" err-lines)
  (if (and out-lines err-lines)
      (format t "  PASS: Both streams accessible from launch-program.~%")
      (format t "  PARTIAL: Some data may have been lost.~%")))


;;;;
;;;; Experiment 6: Timing — SBCL spawn overhead
;;;;

(format t "~%=== Experiment 6: Spawn timing ===~%")

(let ((start (get-internal-real-time)))
  (uiop:run-program '("true") :output nil :error-output nil
                               :ignore-error-status t)
  (let ((ms (round (* 1000 (- (get-internal-real-time) start))
                   internal-time-units-per-second)))
    (format t "  'true' spawn+exit: ~Dms~%" ms)))

(let ((start (get-internal-real-time)))
  (uiop:run-program '("sbcl" "--non-interactive" "--eval" "(sb-ext:exit)")
                     :output nil :error-output nil
                     :ignore-error-status t)
  (let ((ms (round (* 1000 (- (get-internal-real-time) start))
                   internal-time-units-per-second)))
    (format t "  bare SBCL spawn+exit: ~Dms~%" ms)))

(let ((start (get-internal-real-time)))
  (uiop:run-program
   (list "sbcl" "--non-interactive"
         "--eval" (format nil "(load ~S)"
                          (namestring
                           (merge-pathnames "setup.lisp"
                                            ql:*quicklisp-home*)))
         "--eval" "(sb-ext:exit)")
   :output nil :error-output nil
   :ignore-error-status t)
  (let ((ms (round (* 1000 (- (get-internal-real-time) start))
                   internal-time-units-per-second)))
    (format t "  SBCL + quicklisp load: ~Dms~%" ms)))

(let ((start (get-internal-real-time)))
  (uiop:run-program
   (list "sbcl" "--non-interactive"
         "--eval" (format nil "(load ~S)"
                          (namestring
                           (merge-pathnames "setup.lisp"
                                            ql:*quicklisp-home*)))
         "--eval" (format nil "(push ~S asdf:*central-registry*)"
                          (namestring
                           (truename
                            (asdf:system-source-directory :org.melusina.atelier))))
         "--eval" "(asdf:load-system \"org.melusina.atelier/child-worker\" :silent t)"
         "--eval" "(sb-ext:exit)")
   :output nil :error-output nil
   :ignore-error-status t)
  (let ((ms (round (* 1000 (- (get-internal-real-time) start))
                   internal-time-units-per-second)))
    (format t "  SBCL + ql + child-worker load: ~Dms~%" ms)))

(format t "~%=== All experiments complete ===~%")
(sb-ext:exit :code 0)

;;;; End of file `uiop-output-handling.lisp'
