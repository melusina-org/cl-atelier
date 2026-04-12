;;;; fresh-sbcl-load.lisp — Verify editor loads without MCP in fresh SBCL

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/editor)

(define-testcase validate-editor-loads-without-mcp ()
  "Verify that org.melusina.atelier/editor loads in a fresh SBCL subprocess
without pulling in MCP dependencies (INV-17).
Checks that com.inuoe.jzon, bordeaux-threads, and atelier/mcp are all absent."
  #+sbcl
  (let* ((quicklisp-setup
           (namestring
            (merge-pathnames "setup.lisp"
                             (symbol-value
                              (find-symbol "*QUICKLISP-HOME*" "QL")))))
         (source-dir
           (namestring
            (truename
             (asdf:system-source-directory :org.melusina.atelier))))
         ;; Split into separate --eval forms so the reader never sees
         ;; asdf:* before ASDF is loaded (slice 009 rework #7).
         (exit-code
           (nth-value 2
             (uiop:run-program
              (list (namestring (truename sb-ext:*runtime-pathname*))
                    "--no-sysinit" "--no-userinit" "--non-interactive"
                    "--eval" (format nil "(load ~S)" quicklisp-setup)
                    "--eval" (format nil
                               "(push (truename ~S) (symbol-value (find-symbol ~S ~S)))"
                               source-dir
                               "*CENTRAL-REGISTRY*"
                               "ASDF")
                    "--eval" "(handler-bind ((warning (function muffle-warning))) (asdf:load-system \"org.melusina.atelier/editor\"))"
                    "--eval" "(if (or (find-package \"ATELIER/MCP\") (find-package \"COM.INUOE.JZON\") (find-package \"BORDEAUX-THREADS\")) (sb-ext:exit :code 1) (sb-ext:exit :code 0))")
              :output nil :error-output nil
              :ignore-error-status t))))
    (assert-equal 0 exit-code))
  #-sbcl
  nil)

;;;; End of file `fresh-sbcl-load.lisp'
