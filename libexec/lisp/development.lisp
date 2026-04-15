;;;; development.lisp — Project Development for Atelier

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(defpackage #:atelier/development
  (:use #:cl)
  (:import-from #:confidence
   #:define-testcase
   #:assert-condition
   #:assert-eq
   #:assert-nil
   #:assert-string=
   #:assert-t
   #:assert-t*
   #:assert=)
  (:export
   #:lint
   #:findings-without-autofix
   #+quicklisp
   #:reload))

(in-package #:atelier/development)

(defun project-configuration ()
  "Return the project configuration for Atelier, read from project-configuration.sexp."
  (let ((path (merge-pathnames #p"project-configuration.sexp"
                               (asdf:system-source-directory "org.melusina.atelier"))))
    (atelier:read-project-configuration path)))

(defun lint ()
  "Lint the Atelier project using the new-style linter."
  (atelier:lint-system "org.melusina.atelier"))

#+quicklisp
(defun reload ()
  (ql:quickload
   '("org.melusina.confidence"
     "org.melusina.atelier"
     "org.melusina.atelier/development"
     "org.melusina.atelier/testsuite")))


;;;;
;;;; Finding Coverage
;;;;

(defun findings-without-autofix ()
  "Report all finding subclasses that have no registered maintainer.
For each concrete finding class, instantiate a dummy finding and call
RESOLVE-FINDING. If no resolutions are returned, the finding has no
autofix. Returns a list of finding class names without autofix and
prints a summary."
  (let ((without nil)
        (with nil))
    (flet ((finding-has-maintainer-p (finding-class-name)
             ;; Check whether any maintainer has a PREPARE-RESOLUTION method
             ;; specialised on this finding class. We do this by checking the
             ;; method list rather than instantiating dummy findings.
             (let ((gf (fdefinition 'atelier:prepare-resolution)))
               (flet ((method-handles-finding-p (method)
                        (let ((specializers (sb-mop:method-specializers method)))
                          (when (= (length specializers) 2)
                            (let ((finding-specializer (second specializers)))
                              (and (typep finding-specializer 'class)
                                   (not (eq finding-specializer (find-class 'atelier:finding)))
                                   (subtypep finding-class-name
                                             (class-name finding-specializer))))))))
                 (some #'method-handles-finding-p
                       (sb-mop:generic-function-methods gf))))))
      (dolist (inspector-name (atelier:list-inspectors))
        (let* ((inspector (atelier:find-inspector inspector-name))
               (level (atelier:inspector-level inspector)))
          (declare (ignore level))
          ;; Walk the finding subclasses defined via define-findings
          ;; by searching for classes named *-FINDING in the ATELIER package.
          (do-symbols (sym (find-package :atelier))
            (when (and (find-class sym nil)
                       (subtypep sym 'atelier:finding)
                       (not (eq sym 'atelier:finding))
                       (not (eq sym 'atelier:file-finding))
                       (not (eq sym 'atelier:line-finding))
                       (not (eq sym 'atelier:region-finding))
                       (not (eq sym 'atelier:syntax-finding)))
              ;; Avoid duplicates
              (unless (or (member sym without) (member sym with))
                (if (finding-has-maintainer-p sym)
                    (push sym with)
                    (push sym without))))))))
    (format t "~&Findings with autofix (~D):~%" (length with))
    (dolist (name (sort with #'string< :key #'symbol-name))
      (format t "~&  ~A~%" name))
    (format t "~&~%Findings without autofix (~D):~%" (length without))
    (dolist (name (sort without #'string< :key #'symbol-name))
      (format t "~&  ~A~%" name))
    (values without with)))


;;;;
;;;; Command Stock
;;;;

#+nil
(org.melusina.atelier/development:reload)

;;;; End of file `development.lisp'
