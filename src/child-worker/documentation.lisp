;;;; documentation.lisp — Documentation helpers for the child worker

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/child-worker)


;;; ---- S1: apropos ----

(defun apropos-data (search-string &optional package-name)
  "Return a list of alists describing symbols matching SEARCH-STRING.
   If PACKAGE-NAME is non-nil, restrict to that package."
  (let ((result nil)
        (pkg (when (and package-name (not (equal package-name "")))
               (or (find-package package-name)
                   (error "Package ~S not found." package-name))))
        (pattern (string-upcase search-string)))
    (flet ((collect-from-package (p)
             (do-symbols (sym p)
               (when (and (search pattern (symbol-name sym))
                          (eq (symbol-package sym) p))
                 (push (list (cons "name" (symbol-name sym))
                             (cons "package" (package-name p))
                             (cons "kind" (%symbol-kind sym))
                             (cons "external"
                                   (eq :external
                                       (nth-value
                                        1 (find-symbol (symbol-name sym) p)))))
                       result)))))
      (if pkg
          (collect-from-package pkg)
          (dolist (p (list-all-packages))
            (collect-from-package p))))
    (sort result #'string<
          :key (lambda (alist)
                 (format nil "~A:~A"
                         (cdr (assoc "package" alist :test #'string=))
                         (cdr (assoc "name" alist :test #'string=)))))))


;;; ---- S6: macroexpand ----

(defun macroexpand-data (form-string &key fully)
  "Macroexpand FORM-STRING. If FULLY is true, use MACROEXPAND;
   otherwise use MACROEXPAND-1. Returns an alist with expanded form."
  (let* ((form (let ((*package* (find-package :cl-user)))
                 (read-from-string form-string)))
         (expanded (if fully
                       (macroexpand form)
                       (macroexpand-1 form))))
    (list (cons "form" form-string)
          (cons "expanded" (let ((*print-pretty* t)
                                 (*print-right-margin* 80)
                                 (*package* (find-package :cl-user)))
                             (prin1-to-string expanded)))
          (cons "fully" (if fully t nil)))))


;;; ---- S7: disassemble ----

(defun disassemble-data (designator)
  "Disassemble the function named by DESIGNATOR.
   Returns an alist with the disassembly text."
  (let* ((sym (%parse-symbol-designator designator))
         (output (make-string-output-stream)))
    (unless (fboundp sym)
      (error "~A is not a defined function." designator))
    (let ((*standard-output* output))
      (disassemble sym))
    (list (cons "symbol" designator)
          (cons "disassembly" (get-output-stream-string output)))))


;;; ---- S8: compile-form ----

(defun compile-form-data (form-string)
  "Compile FORM-STRING and capture compiler diagnostics.
   Uses (compile nil (lambda () FORM)) to avoid side effects.
   Returns an alist with the diagnostics."
  (let* ((form (let ((*package* (find-package :cl-user)))
                 (read-from-string form-string)))
         (notes nil))
    (handler-bind
        ((warning (lambda (c)
                    (push (list (cons "severity" "WARNING")
                                (cons "message" (princ-to-string c)))
                          notes)
                    (muffle-warning c)))
         #+sbcl
         (sb-ext:compiler-note (lambda (c)
                                 (push (list (cons "severity" "NOTE")
                                             (cons "message" (princ-to-string c)))
                                       notes)
                                 (muffle-warning c))))
      (handler-case
          (compile nil `(lambda () ,form))
        (error (c)
          (push (list (cons "severity" "ERROR")
                      (cons "message" (princ-to-string c)))
                notes))))
    (list (cons "form" form-string)
          (cons "diagnostics" (nreverse notes))
          (cons "diagnostic-count" (length notes)))))

;;;; End of file `documentation.lisp'
