;;;; introspection.lisp — Introspection helpers for the child worker

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/child-worker)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (require :sb-introspect))

;;; These functions run in the child SBCL image. The parent calls them
;;; via SWANK eval and reads back the printed alist results.

(defun %symbol-kind (symbol)
  "Classify SYMBOL into a kind keyword string."
  (cond
    ((special-operator-p symbol) "SPECIAL-FORM")
    ((macro-function symbol) "MACRO")
    ((and (fboundp symbol)
          (typep (fdefinition symbol) 'generic-function))
     "GENERIC-FUNCTION")
    ((fboundp symbol) "FUNCTION")
    ((find-class symbol nil)
     (if (subtypep (find-class symbol) (find-class 'condition))
         "CONDITION"
         "CLASS"))
    ((constantp symbol) "CONSTANT")
    ((boundp symbol) "VARIABLE")
    (t "UNBOUND")))

(defun %symbol-documentation (symbol)
  "Return the best available documentation string for SYMBOL."
  (or (documentation symbol 'function)
      (documentation symbol 'variable)
      (documentation symbol 'type)
      ""))

(defun %truncate-string (string max-length)
  "Truncate STRING to MAX-LENGTH characters."
  (if (> (length string) max-length)
      (concatenate 'string (subseq string 0 max-length) "...")
      string))


;;; ---- Public API ----

(defun list-packages-data ()
  "Return a list of alists describing all packages in the image."
  (let ((result nil))
    (dolist (pkg (list-all-packages) (nreverse result))
      (let ((ext-count 0)
            (int-count 0))
        (do-external-symbols (s pkg)
          (declare (ignore s))
          (incf ext-count))
        (do-symbols (s pkg)
          (when (eq (symbol-package s) pkg)
            (incf int-count)))
        ;; internal-count includes externals; subtract
        (decf int-count ext-count)
        (push (list (cons "name" (package-name pkg))
                    (cons "nicknames"
                          (mapcar #'identity (package-nicknames pkg)))
                    (cons "use-list"
                          (mapcar #'package-name (package-use-list pkg)))
                    (cons "used-by-list"
                          (mapcar #'package-name (package-used-by-list pkg)))
                    (cons "external-count" ext-count)
                    (cons "internal-count" int-count))
              result)))))

(defun list-package-symbols-data (package-name &key (status :external))
  "Return a list of alists describing symbols in PACKAGE-NAME.
   STATUS is one of :EXTERNAL, :INTERNAL, :ANY (default :EXTERNAL)."
  (let ((pkg (find-package package-name))
        (result nil))
    (unless pkg
      (error "Package ~S not found." package-name))
    (flet ((collect-symbol (sym)
             (push (list (cons "name" (symbol-name sym))
                         (cons "package" (package-name
                                         (or (symbol-package sym) pkg)))
                         (cons "status" (string status))
                         (cons "home-package"
                               (if (symbol-package sym)
                                   (package-name (symbol-package sym))
                                   ""))
                         (cons "kind" (%symbol-kind sym))
                         (cons "documentation"
                               (%truncate-string
                                (%symbol-documentation sym) 200)))
                   result)))
      (ecase status
        (:external (do-external-symbols (s pkg) (collect-symbol s)))
        (:internal (do-symbols (s pkg)
                     (when (eq (symbol-package s) pkg)
                       (collect-symbol s))))
        (:any (do-symbols (s pkg) (collect-symbol s)))))
    (sort result #'string< :key (lambda (alist) (cdr (assoc "name" alist :test #'string=))))))

(defun %parse-symbol-designator (designator)
  "Parse DESIGNATOR (a string like \"cl:car\") into a symbol.
   Signals an error if the symbol cannot be found."
  (let ((sym (let ((*package* (find-package :cl-user)))
               (read-from-string designator))))
    (unless (symbolp sym)
      (error "~S does not designate a symbol." designator))
    sym))

(defun describe-symbol-data (designator)
  "Return an alist describing the symbol named by DESIGNATOR."
  (let* ((sym (%parse-symbol-designator designator))
         (result (list (cons "name" (symbol-name sym))
                       (cons "package" (if (symbol-package sym)
                                          (package-name (symbol-package sym))
                                          ""))
                       (cons "kind" (%symbol-kind sym))
                       (cons "documentation" (%symbol-documentation sym)))))
    ;; Function-specific info
    (when (fboundp sym)
      (let ((fn (fdefinition sym)))
        (push (cons "lambda-list"
                    (princ-to-string
                     (or #+sbcl (sb-introspect:function-lambda-list fn)
                         nil)))
              result)
        #+sbcl
        (let ((type (sb-introspect:function-type fn)))
          (when type
            (push (cons "type" (princ-to-string type)) result)))))
    ;; Class-specific info
    (let ((class (find-class sym nil)))
      (when class
        (closer-mop:ensure-finalized class)
        (push (cons "slots"
                    (mapcar (lambda (slot)
                              (symbol-name (closer-mop:slot-definition-name slot)))
                            (closer-mop:class-slots class)))
              result)))
    ;; Generic-function-specific info
    (when (and (fboundp sym)
               (typep (fdefinition sym) 'generic-function))
      (let ((gf (fdefinition sym)))
        (push (cons "methods"
                    (mapcar (lambda (method)
                              (format nil "~{~A~^ ~}"
                                      (mapcar #'princ-to-string
                                              (closer-mop:method-specializers method))))
                            (closer-mop:generic-function-methods gf)))
              result)))
    (nreverse result)))

(defun find-definition-data (designator)
  "Return an alist with source location for DESIGNATOR, or NIL."
  #+sbcl
  (let* ((sym (%parse-symbol-designator designator))
         (sources (sb-introspect:find-definition-sources-by-name sym :function)))
    (when sources
      (let ((source (first sources)))
        (let ((pathname (sb-introspect:definition-source-pathname source)))
          (when pathname
            (list (cons "source-file" (namestring pathname))
                  (cons "form-path"
                        (princ-to-string
                         (sb-introspect:definition-source-form-path source)))))))))
  #-sbcl
  nil)

(defun run-testsuite-data (system-name)
  "Load SYSTEM-NAME, run its test system, and return a result alist."
  (let ((output (make-string-output-stream))
        (err-output (make-string-output-stream))
        (start (get-internal-real-time))
        (success nil))
    (handler-case
        (progn
          (let ((*standard-output* (make-broadcast-stream *standard-output* output))
                (*error-output* (make-broadcast-stream *error-output* err-output)))
            (asdf:load-system system-name)
            (asdf:test-system system-name))
          (setf success t))
      (error (c)
        (format err-output "~&Error: ~A~%" c)))
    (let ((duration-ms (round (* 1000 (- (get-internal-real-time) start))
                              internal-time-units-per-second)))
      (list (cons "system" (string system-name))
            (cons "success" success)
            (cons "duration-ms" duration-ms)
            (cons "output" (%truncate-string
                            (get-output-stream-string output) 10000))
            (cons "error-output" (%truncate-string
                                  (get-output-stream-string err-output) 5000))))))

;;;; End of file `introspection.lisp'
