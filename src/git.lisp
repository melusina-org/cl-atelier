;;;; git.lisp — Git integration for Atelier

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)


;;;;
;;;; Pre-Commit Hook
;;;;

(define-condition pre-commit-hook-exists (warning)
  ((pathname
    :initarg :pathname
    :reader pre-commit-hook-exists-pathname
    :type pathname
    :documentation "The pathname of the existing pre-commit hook."))
  (:report (lambda (condition stream)
             (format stream "Pre-commit hook already exists at ~A."
                     (pre-commit-hook-exists-pathname condition))))
  (:documentation "Signalled when INSTALL-PRE-COMMIT-HOOK finds an existing hook.
Provides REPLACE-HOOK and KEEP-EXISTING-HOOK restarts."))

(defun find-asd-file (directory)
  "Find the first .asd file in DIRECTORY. Return its pathname or NIL."
  (declare (type pathname directory)
           (values (or null pathname)))
  (first (directory (merge-pathnames (make-pathname :name :wild :type "asd")
                                     directory))))

(defun derive-system-name (asd-pathname)
  "Derive the ASDF system name from ASD-PATHNAME by taking the file stem."
  (declare (type pathname asd-pathname)
           (values string))
  (pathname-name asd-pathname))

(defun pre-commit-hook-script (system-name asd-file)
  "Return the shell script content for a pre-commit hook.
The script loads SYSTEM-NAME from ASD-FILE, runs the linter with autofix,
and runs the test suite."
  (declare (type string system-name)
           (type string asd-file)
           (values string))
  (format nil "#!/bin/sh
# Pre-commit hook installed by Atelier.
# Runs the linter with autofix and the test suite before each commit.

set -eu

sbcl --non-interactive \\
     --load ~S \\
     --eval '(require :asdf)' \\
     --eval '(asdf:load-system ~S)' \\
     --eval '(atelier:lint ~S :action :fix)' \\
     --eval '(asdf:test-system ~S)'
"
          asd-file
          system-name
          system-name
          system-name))

(defun install-pre-commit-hook (git-repository-pathname &key system-name)
  "Install a git pre-commit hook at GIT-REPOSITORY-PATHNAME.
The hook runs the linter with autofix and the test suite before each commit.
When SYSTEM-NAME is not supplied, it is derived from the .asd file in the
repository root.
Signals an error if GIT-REPOSITORY-PATHNAME has no .git/ subdirectory.
Signals PRE-COMMIT-HOOK-EXISTS if the hook already exists, with REPLACE-HOOK
and KEEP-EXISTING-HOOK restarts."
  (declare (type pathname git-repository-pathname)
           (values (or null pathname)))
  (let* ((git-dir (merge-pathnames #p".git/" git-repository-pathname))
         (hooks-dir (merge-pathnames #p"hooks/" git-dir))
         (hook-path (merge-pathnames #p"pre-commit" hooks-dir)))
    (unless (probe-file git-dir)
      (error "Not a git repository: ~A has no .git/ directory."
             git-repository-pathname))
    (when (probe-file hook-path)
      (restart-case
          (progn
            (warn 'pre-commit-hook-exists :pathname hook-path)
            (return-from install-pre-commit-hook nil))
        (replace-hook ()
          :report "Replace the existing pre-commit hook."
          nil)
        (keep-existing-hook ()
          :report "Keep the existing pre-commit hook."
          (return-from install-pre-commit-hook nil))))
    (let* ((effective-system-name
             (or system-name
                 (let ((asd (find-asd-file git-repository-pathname)))
                   (if asd
                       (derive-system-name asd)
                       (error "No .asd file found in ~A; supply :SYSTEM-NAME."
                              git-repository-pathname)))))
           (asd-file
             (let ((asd (find-asd-file git-repository-pathname)))
               (if asd
                   (namestring asd)
                   (format nil "~A.asd" effective-system-name))))
           (script (pre-commit-hook-script effective-system-name asd-file)))
      (ensure-directories-exist hook-path)
      (with-open-file (stream hook-path :direction :output
                                        :if-exists :supersede
                                        :external-format :utf-8)
        (write-string script stream))
      ;; Set executable permission
      (uiop:run-program (list "chmod" "+x" (namestring hook-path)))
      hook-path)))

;;;; End of file `git.lisp'
