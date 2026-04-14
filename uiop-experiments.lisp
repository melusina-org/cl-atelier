;;;; uiop-experiments.lisp — Discovery experiments for UIOP subprocess I/O

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

;;;; These experiments validate assumptions about UIOP's run-program
;;;; and launch-program output handling. Results inform slice 015
;;;; (MCP server reliability) design decisions.
;;;;
;;;; Run with:
;;;;   sbcl --non-interactive --load uiop-experiments.lisp
;;;;
;;;; Validated 2026-04-14 on SBCL 2.6.3 / Darwin ARM64.

(require :uiop)


;;;;
;;;; Helpers
;;;;

(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defmacro experiment (title &body body)
  "Run BODY as a named experiment. Print PASS/FAIL."
  `(progn
     (format t "~%=== ~A ===~%" ,title)
     (handler-case
         (progn ,@body
                (incf *pass-count*)
                (format t "  PASS~%"))
       (error (c)
         (incf *fail-count*)
         (format t "  FAIL: ~A~%" c)))))

(defun elapsed-ms (start)
  "Return milliseconds elapsed since START (internal-real-time)."
  (round (* 1000 (- (get-internal-real-time) start))
         internal-time-units-per-second))


;;;;
;;;; 1. Basic stream separation
;;;;

(experiment "run-program: :output :string, :error-output :string"
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program
       '("/bin/sh" "-c"
         "printf 'A\\n'; printf 'X\\n' 1>&2; sleep 0.1; printf 'B\\n'; printf 'Y\\n' 1>&2")
       :output :string :error-output :string
       :ignore-error-status t)
    (format t "  stdout: ~S~%" stdout)
    (format t "  stderr: ~S~%" stderr)
    (format t "  exit:   ~D~%" exit-code)
    (assert (search "A" stdout))
    (assert (search "B" stdout))
    (assert (search "X" stderr))
    (assert (search "Y" stderr))))


;;;;
;;;; 2. Function processor for stdout, string for stderr
;;;;

(experiment "run-program: :output function, :error-output :string"
  (let ((out-lines nil))
    (multiple-value-bind (result stderr exit-code)
        (uiop:run-program
         '("/bin/sh" "-c"
           "printf 'A\\n'; printf 'X\\n' 1>&2; sleep 0.1; printf 'B\\n'")
         :output (lambda (stream)
                   (loop :for line := (read-line stream nil nil)
                         :while line
                         :do (push line out-lines)))
         :error-output :string
         :ignore-error-status t)
      (declare (ignore result))
      (setf out-lines (nreverse out-lines))
      (format t "  stdout lines: ~S~%" out-lines)
      (format t "  stderr:       ~S~%" stderr)
      (format t "  exit:         ~D~%" exit-code)
      (assert (member "A" out-lines :test #'string=))
      (assert (member "B" out-lines :test #'string=))
      (assert (search "X" stderr)))))


;;;;
;;;; 3. Both streams as functions simultaneously
;;;;

(experiment "run-program: both :output and :error-output as functions"
  (let ((out-lines nil)
        (err-lines nil))
    (uiop:run-program
     '("/bin/sh" "-c"
       "printf 'A\\n'; printf 'X\\n' 1>&2; sleep 0.1; printf 'B\\n'; printf 'Y\\n' 1>&2")
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
    (assert (member "A" out-lines :test #'string=))
    (assert (member "B" out-lines :test #'string=))
    (assert (member "X" err-lines :test #'string=))
    (assert (member "Y" err-lines :test #'string=))))


;;;;
;;;; 4. Merge stderr into stdout
;;;;

(experiment "run-program: :error-output :output (merge)"
  (let ((lines nil))
    (uiop:run-program
     '("/bin/sh" "-c"
       "printf 'A\\n'; printf 'X\\n' 1>&2; sleep 0.1; printf 'B\\n'; printf 'Y\\n' 1>&2")
     :output (lambda (stream)
               (loop :for line := (read-line stream nil nil)
                     :while line
                     :do (push line lines)))
     :error-output :output
     :ignore-error-status t)
    (setf lines (nreverse lines))
    (format t "  merged lines: ~S~%" lines)
    (assert (>= (length lines) 4))))


;;;;
;;;; 5. launch-program with :stream — sequential read of both
;;;;

(experiment "launch-program: :output :stream, :error-output :stream"
  (let* ((process (uiop:launch-program
                    '("/bin/sh" "-c"
                      "printf 'A\\n'; printf 'X\\n' 1>&2; sleep 0.1; printf 'B\\n'; printf 'Y\\n' 1>&2")
                    :output :stream
                    :error-output :stream))
         (stdout (uiop:process-info-output process))
         (stderr (uiop:process-info-error-output process))
         (out-lines nil)
         (err-lines nil))
    ;; Read stdout first (blocking until EOF)
    (loop :for line := (read-line stdout nil nil)
          :while line
          :do (push line out-lines))
    ;; Then read stderr (process is done by now)
    (loop :for line := (read-line stderr nil nil)
          :while line
          :do (push line err-lines))
    (uiop:close-streams process)
    (uiop:wait-process process)
    (setf out-lines (nreverse out-lines))
    (setf err-lines (nreverse err-lines))
    (format t "  stdout: ~S~%" out-lines)
    (format t "  stderr: ~S~%" err-lines)
    (assert out-lines)
    (assert err-lines)))


;;;;
;;;; 6. launch-program: incremental stdout read (MCP child spawn pattern)
;;;;
;;;; This simulates the MCP child spawn: child prints a port number on
;;;; stdout, then keeps running. We read stdout line-by-line looking for
;;;; the port, then move on — no drain thread needed.
;;;;

(experiment "launch-program: incremental stdout read (port discovery pattern)"
  (let* ((process (uiop:launch-program
                    '("/bin/sh" "-c"
                      "echo PORT:12345; sleep 0.5; echo READY")
                    :output :stream
                    :error-output :stream))
         (stdout (uiop:process-info-output process))
         (port nil))
    ;; Read lines until we find the port
    (loop :for line := (read-line stdout nil nil)
          :while line
          :do (when (uiop:string-prefix-p "PORT:" line)
                (setf port (parse-integer line :start 5))
                (return)))
    (format t "  discovered port: ~D~%" port)
    (assert (eql port 12345))
    ;; Process is still running — we can read more later
    (let ((next-line (read-line stdout nil nil)))
      (format t "  next line: ~S~%" next-line)
      (assert (string= "READY" next-line)))
    (uiop:close-streams process)
    (uiop:wait-process process)))


;;;;
;;;; 7. launch-program: stderr to temp file (deadlock prevention)
;;;;
;;;; When child produces large stderr before printing the port to stdout,
;;;; the stderr pipe buffer fills and blocks the child. Redirecting stderr
;;;; to a temp file avoids deadlock.
;;;;

(experiment "launch-program: :error-output to pathname (deadlock prevention)"
  (uiop:with-temporary-file (:pathname errfile :keep nil)
    (let* ((process (uiop:launch-program
                      '("/bin/sh" "-c"
                        ;; Produce stderr BEFORE stdout — would deadlock with pipes
                        "for i in $(seq 1 100); do printf 'warn %d\\n' $i 1>&2; done; printf 'PORT:9999\\n'")
                      :output :stream
                      :error-output errfile))
           (stdout (uiop:process-info-output process))
           (port nil))
      ;; Read stdout for port — no deadlock because stderr goes to file
      (loop :for line := (read-line stdout nil nil)
            :while line
            :do (when (uiop:string-prefix-p "PORT:" line)
                  (setf port (parse-integer line :start 5))
                  (return)))
      (uiop:close-streams process)
      (uiop:wait-process process)
      (format t "  port: ~D~%" port)
      (assert (eql port 9999))
      ;; Read stderr from file after process is done
      (let ((stderr-content (uiop:read-file-string errfile)))
        (format t "  stderr length: ~D chars~%" (length stderr-content))
        (assert (search "warn 100" stderr-content))))))


;;;;
;;;; 8. run-program: :output :lines keyword processor
;;;;

(experiment "run-program: :output :lines (keyword processor)"
  (let ((lines (uiop:run-program
                 '("/bin/sh" "-c" "echo hello; echo world")
                 :output :lines)))
    (format t "  lines: ~S~%" lines)
    (assert (equal lines '("hello" "world")))))


;;;;
;;;; 9. run-program: :output :form (read Lisp form from child)
;;;;

(experiment "run-program: :output :form (Lisp form from child stdout)"
  (let ((form (uiop:run-program
                '("/bin/sh" "-c" "echo '(1 2 3)'")
                :output :form)))
    (format t "  form: ~S~%" form)
    (assert (equal form '(1 2 3)))))


;;;;
;;;; 10. Timing: spawn overhead
;;;;

(format t "~%=== Timing: spawn overhead ===~%")

(flet ((time-command (label command)
         (let ((start (get-internal-real-time)))
           (uiop:run-program command :output nil :error-output nil
                                     :ignore-error-status t)
           (let ((ms (elapsed-ms start)))
             (format t "  ~A: ~Dms~%" label ms)
             ms))))

  (time-command "'true'" '("true"))
  (time-command "bare SBCL" '("sbcl" "--non-interactive" "--eval" "(sb-ext:exit)"))
  (time-command "SBCL + quicklisp"
                (list "sbcl" "--non-interactive"
                      "--eval" (format nil "(load ~S)"
                                       (namestring
                                        (merge-pathnames "setup.lisp"
                                                         ql:*quicklisp-home*)))
                      "--eval" "(sb-ext:exit)"))
  (time-command "SBCL + ql + child-worker"
                (list "sbcl" "--non-interactive"
                      "--eval" (format nil "(load ~S)"
                                       (namestring
                                        (merge-pathnames "setup.lisp"
                                                         ql:*quicklisp-home*)))
                      "--eval" (format nil "(push ~S asdf:*central-registry*)"
                                       (namestring
                                        (truename
                                         (asdf:system-source-directory
                                          :org.melusina.atelier))))
                      "--eval" "(asdf:load-system \"org.melusina.atelier/child-worker\" :silent t)"
                      "--eval" "(sb-ext:exit)")))


;;;;
;;;; Summary
;;;;

(format t "~%=== Summary ===~%")
(format t "  Passed: ~D~%" *pass-count*)
(format t "  Failed: ~D~%" *fail-count*)
(sb-ext:exit :code (if (zerop *fail-count*) 0 1))

;;;; End of file `uiop-experiments.lisp'
