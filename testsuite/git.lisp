;;;; git.lisp — Tests for git integration

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)


;;;;
;;;; Pre-Commit Hook Tests
;;;;

(defun call-with-temporary-git-repo (function)
  "Call FUNCTION with a temporary directory set up as a git repository.
FUNCTION receives the directory pathname. The directory is cleaned up on exit."
  (let* ((base (merge-pathnames
                (make-pathname :directory (list :relative
                                               (format nil "atelier-git-test-~A"
                                                       (random 1000000))))
                (uiop:temporary-directory)))
         (git-dir (merge-pathnames #p".git/" base)))
    (unwind-protect
         (progn
           (ensure-directories-exist (merge-pathnames #p"hooks/" git-dir))
           (with-open-file (s (merge-pathnames "test-project.asd" base)
                              :direction :output :if-exists :supersede)
             (format s "(defsystem \"test-project\")~%"))
           (funcall function base))
      (uiop:delete-directory-tree base :validate t :if-does-not-exist :ignore))))

(define-testcase test-install-pre-commit-hook-no-git ()
  "Verify that install-pre-commit-hook signals an error for non-git directories."
  (let ((base (merge-pathnames
               (make-pathname :directory (list :relative
                                              (format nil "atelier-nogit-~A"
                                                      (random 1000000))))
               (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (ensure-directories-exist base)
           (assert-t
            (not (null
                  (handler-case
                      (progn
                        (atelier:install-pre-commit-hook base :system-name "test-system")
                        nil)
                    (error () t))))))
      (uiop:delete-directory-tree base :validate t :if-does-not-exist :ignore))))

(define-testcase test-install-pre-commit-hook ()
  "Verify that install-pre-commit-hook creates a working pre-commit hook."
  (call-with-temporary-git-repo
   (lambda (tmpdir)
     (let* ((hook-path (merge-pathnames #p".git/hooks/pre-commit" tmpdir))
            (result (atelier:install-pre-commit-hook tmpdir)))
       (assert-t (not (null result)))
       (assert-t (not (null (probe-file hook-path))))
       (let ((content (uiop:read-file-string hook-path)))
         (assert-t (not (null (search "sbcl" content))))
         (assert-t (not (null (search "lint-system" content))))
         (assert-t (not (null (search "test-system" content))))
         (assert-t (not (null (search "test-project" content)))))))))

(define-testcase test-install-pre-commit-hook-exists ()
  "Verify that install-pre-commit-hook signals a warning when hook exists."
  (call-with-temporary-git-repo
   (lambda (tmpdir)
     (let ((hook-path (merge-pathnames #p".git/hooks/pre-commit" tmpdir)))
       ;; Create existing hook
       (with-open-file (s hook-path :direction :output :if-exists :supersede)
         (write-string "#!/bin/sh" s))
       ;; Without handling, should return NIL (keep existing)
       (let ((warned nil))
         (handler-bind ((atelier:pre-commit-hook-exists
                          (lambda (c)
                            (declare (ignore c))
                            (setf warned t)
                            (muffle-warning c))))
           (atelier:install-pre-commit-hook tmpdir))
         (assert-t warned)
         ;; Hook should still have original content
         (assert-string= "#!/bin/sh" (uiop:read-file-string hook-path)))))))

(define-testcase test-install-pre-commit-hook-replace ()
  "Verify that the replace-hook restart overwrites the existing hook."
  (call-with-temporary-git-repo
   (lambda (tmpdir)
     (let ((hook-path (merge-pathnames #p".git/hooks/pre-commit" tmpdir)))
       ;; Create existing hook
       (with-open-file (s hook-path :direction :output :if-exists :supersede)
         (write-string "#!/bin/sh" s))
       ;; Use replace-hook restart
       (handler-bind ((atelier:pre-commit-hook-exists
                        (lambda (c)
                          (declare (ignore c))
                          (invoke-restart 'atelier::replace-hook))))
         (atelier:install-pre-commit-hook tmpdir))
       ;; Hook should now have Atelier content
       (let ((content (uiop:read-file-string hook-path)))
         (assert-t (not (null (search "sbcl" content)))))))))


;;;;
;;;; Entry Point
;;;;

(define-testcase testsuite-git ()
  "Run all git integration tests."
  (test-install-pre-commit-hook-no-git)
  (test-install-pre-commit-hook)
  (test-install-pre-commit-hook-exists)
  (test-install-pre-commit-hook-replace))

;;;; End of file `git.lisp'
