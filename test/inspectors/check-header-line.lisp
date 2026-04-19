;;;; check-header-line.lisp — Tests for the canonical header line inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/test)

(define-testcase validate-check-header-line-correct-lisp ()
  "Verify no finding for a Lisp file with a correct canonical header."
  (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (format s ";;;; ~A — Test fixture~%(in-package #:cl-user)~%"
              (file-namestring p)))
    (let* ((inspector (atelier:find-inspector 'atelier:check-header-line))
           (findings (atelier:inspect-file inspector p)))
      (assert-t (null findings)))))

(define-testcase validate-check-header-line-violation-lisp ()
  "Verify finding for a Lisp file with wrong header."
  (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (write-string ";; wrong header" s)
      (terpri s))
    (let* ((inspector (atelier:find-inspector 'atelier:check-header-line))
           (findings (atelier:inspect-file inspector p)))
      (assert-t (not (null findings)))
      (assert-t (typep (first findings) 'atelier:header-line-finding)))))

(define-testcase validate-check-header-line-correct-shell ()
  "Verify no finding for a shell script with a correct canonical header after shebang."
  (uiop:with-temporary-file (:pathname p :type "sh" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (format s "#!/bin/sh~%# ~A — Test fixture~%echo hello~%"
              (file-namestring p)))
    (let* ((inspector (atelier:find-inspector 'atelier:check-header-line))
           (findings (atelier:inspect-file inspector p)))
      (assert-t (null findings)))))

(define-testcase validate-check-header-line-violation-shell ()
  "Verify finding for a shell script with wrong header after shebang."
  (uiop:with-temporary-file (:pathname p :type "sh" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (format s "#!/bin/sh~%echo hello~%"))
    (let* ((inspector (atelier:find-inspector 'atelier:check-header-line))
           (findings (atelier:inspect-file inspector p)))
      (assert-t (not (null findings))))))

(define-testcase validate-check-header-line-correct-c ()
  "Verify no finding for a C file with a correct canonical header."
  (uiop:with-temporary-file (:pathname p :type "c" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (format s "// ~A — Test fixture~%int main() {}~%"
              (file-namestring p)))
    (let* ((inspector (atelier:find-inspector 'atelier:check-header-line))
           (findings (atelier:inspect-file inspector p)))
      (assert-t (null findings)))))

(define-testcase validate-check-header-line-unknown-type-skipped ()
  "Verify no finding for a file with unknown extension."
  (uiop:with-temporary-file (:pathname p :type "xyz" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (write-string "some random content" s))
    (let* ((inspector (atelier:find-inspector 'atelier:check-header-line))
           (findings (atelier:inspect-file inspector p)))
      (assert-t (null findings)))))

(define-testcase validate-file-comment-prefix ()
  "Verify file-comment-prefix returns the right prefix for various file types."
  (assert-string= ";;;; " (atelier:file-comment-prefix #p"foo.lisp"))
  (assert-string= "# " (atelier:file-comment-prefix #p"bar.sh"))
  (assert-string= "// " (atelier:file-comment-prefix #p"baz.c"))
  (assert-string= "// " (atelier:file-comment-prefix #p"baz.cpp"))
  (assert-string= "% " (atelier:file-comment-prefix #p"doc.tex"))
  (assert-string= "# " (atelier:file-comment-prefix #p"main.tf"))
  (assert-t (null (atelier:file-comment-prefix #p"readme.md"))))

(define-testcase testsuite-check-header-line ()
  (validate-file-comment-prefix)
  (validate-check-header-line-correct-lisp)
  (validate-check-header-line-violation-lisp)
  (validate-check-header-line-correct-shell)
  (validate-check-header-line-violation-shell)
  (validate-check-header-line-correct-c)
  (validate-check-header-line-unknown-type-skipped))

;;;; End of file `check-header-line.lisp'
