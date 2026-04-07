;;;; check-header-line.lisp — Canonical header line inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)


;;;;
;;;; Comment Style Infrastructure
;;;;

(defun file-comment-prefix (pathname)
  "Return the line comment prefix for PATHNAME based on its extension and name.
Return NIL when the file type is not recognised. Supports Lisp, Shell, Makefile,
Dockerfile, Terraform, C/C++, TeX/Metapost, and Autoconf."
  (declare (type pathname pathname)
           (values (or null string)))
  (let ((ext (pathname-type pathname))
        (name (file-namestring pathname)))
    (cond
      ;; Exact filename matches first
      ((or (string= name "Makefile")
           (string= name "Makefile.in")
           (string= name "Makefile.config.in"))
       "# ")
      ((string= name "Dockerfile")
       "# ")
      ((string= name "configure.ac")
       "dnl ")
      ;; Extension matches
      ((and ext (member ext '("lisp" "asd" "asdf") :test #'string-equal))
       ";;;; ")
      ((and ext (member ext '("sh" "bash" "zsh" "ksh"
                               "sh.in" "bash.in" "zsh.in" "ksh.in")
                        :test #'string-equal))
       "# ")
      ((and ext (member ext '("tf" "hcl" "tfvars") :test #'string-equal))
       "# ")
      ((and ext (member ext '("mk" "mk.in") :test #'string-equal))
       "# ")
      ((and ext (member ext '("dockerfile") :test #'string-equal))
       "# ")
      ((and ext (member ext '("c" "h" "cc" "cpp" "cxx" "hpp" "hxx") :test #'string-equal))
       "// ")
      ((and ext (member ext '("tex" "cls" "sty" "mac" "mp" "mpost") :test #'string-equal))
       "% ")
      ((and ext (member ext '("m4" "ac") :test #'string-equal))
       "dnl ")
      (t nil))))

(defun lisp-source-file-p (pathname)
  "Return T if PATHNAME has a Lisp source file extension."
  (declare (type pathname pathname))
  (and (member (pathname-type pathname) '("lisp" "asd" "asdf") :test #'string-equal) t))


;;;;
;;;; Inspector
;;;;

(define-file-inspector check-header-line ((pathname pathname))
  "Check that the first non-shebang line is a canonical header line.
The canonical header line has the form: <comment-prefix>filename — description.
Applies to all files with a recognised comment style."
  (let ((prefix (file-comment-prefix pathname)))
    (when prefix
      (let* ((lines (read-file-into-line-vector pathname))
             (first-line-index
               (if (and (> (length lines) 0)
                        (string-prefix-p "#!" (aref lines 0)))
                   1
                   0))
             (first-line (when (< first-line-index (length lines))
                           (aref lines first-line-index)))
             (filename (file-namestring pathname))
             (expected-prefix (format nil "~A~A —" prefix filename)))
        (when (and first-line
                   (not (string-prefix-p expected-prefix first-line)))
          (list (make-instance 'header-line-finding
                 :inspector 'check-header-line
                 :severity :style
                 :observation "First line is not a canonical header line."
                 :rationale (format nil
                                    "The first line should begin with: ~A"
                                    expected-prefix)
                 :file pathname
                 :line (1+ first-line-index)
                 :column 0
                 :end-line (1+ first-line-index)
                 :end-column (length first-line)
                 :source-text first-line)))))))

;;;; End of file `check-header-line.lisp'
