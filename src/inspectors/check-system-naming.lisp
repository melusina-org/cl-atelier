;;;; check-system-naming.lisp — System and component naming inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)


;;;;
;;;; Canonical System Naming
;;;;

(defparameter *canonical-system-suffixes*
  '("test" "development" "operation" "experiment")
  "List of canonical secondary system name suffixes.
A system named MAIN/SUFFIX is canonical when SUFFIX is in this list.")

(defparameter *deprecated-system-name-map*
  '(("testsuite" . "test"))
  "Alist mapping deprecated system suffixes to their canonical replacements.")

(defparameter *deprecated-component-name-map*
  '(("entrypoint" . "entry-point")
    ("main" . "entry-point")
    ("testsuite" . "test"))
  "Alist mapping deprecated component names to their canonical replacements.")

(defun system-suffix (system-name main-system-name)
  "Extract the suffix from SYSTEM-NAME relative to MAIN-SYSTEM-NAME.
Return NIL if SYSTEM-NAME is the main system or does not start with
MAIN-SYSTEM-NAME followed by a slash."
  (declare (type string system-name main-system-name)
           (values (or null string)))
  (let ((prefix (concatenate 'string main-system-name "/")))
    (when (and (> (length system-name) (length prefix))
               (string-equal prefix (subseq system-name 0 (length prefix))))
      (subseq system-name (length prefix)))))

(defun canonical-suffix-p (suffix)
  "Return T if SUFFIX is a canonical secondary system suffix."
  (declare (type string suffix)
           (values boolean))
  (and (member suffix *canonical-system-suffixes* :test #'string-equal) t))

(defun deprecated-suffix-replacement (suffix)
  "Return the canonical replacement for deprecated SUFFIX, or NIL."
  (declare (type string suffix)
           (values (or null string)))
  (cdr (assoc suffix *deprecated-system-name-map* :test #'string-equal)))

(defun deprecated-component-replacement (name)
  "Return the canonical replacement for deprecated component NAME, or NIL."
  (declare (type string name)
           (values (or null string)))
  (cdr (assoc name *deprecated-component-name-map* :test #'string-equal)))


;;;;
;;;; Reading Defsystem Forms
;;;;

(defun read-defsystem-forms-with-positions (pathname)
  "Read all top-level forms from PATHNAME and return those that are DEFSYSTEM calls.
Each returned element is (FORM . LINE-NUMBER) where FORM is the raw sexp
(DEFSYSTEM NAME ...) and LINE-NUMBER is the 1-based line where it starts.
Reading is done with *READ-EVAL* bound to NIL for safety."
  (declare (type pathname pathname)
           (values list))
  (let ((*read-eval* nil)
        (*package* (find-package :keyword))
        (forms nil))
    (handler-case
        (with-open-file (stream pathname :direction :input
                                         :external-format :utf-8)
          (loop :for form = (read stream nil :eof)
                :for line-after = (unless (eq form :eof) (file-position stream))
                :until (eq form :eof)
                :when (and (consp form)
                           (symbolp (car form))
                           (string-equal "DEFSYSTEM" (symbol-name (car form))))
                  :do (push (cons form 1) forms)))
      (error () nil))
    ;; We cannot reliably get line numbers from standard READ.
    ;; Instead, find the line by searching the file content for each defsystem name.
    (when forms
      (let* ((content (uiop:read-file-string pathname :external-format :utf-8))
             (lines (string-lines content)))
        (flet ((find-defsystem-line (name-string)
                 (loop :for line :in lines
                       :for line-number :from 1
                       :when (search name-string line :test #'char-equal)
                         :return line-number
                       :finally (return 1))))
          (setf forms
                (loop :for (form . nil) :in (nreverse forms)
                      :for name = (defsystem-name-string form)
                      :collect (cons form (find-defsystem-line name)))))))
    forms))

(defun defsystem-name-string (defsystem-form)
  "Extract the system name from a DEFSYSTEM form as a lowercase string."
  (declare (type cons defsystem-form)
           (values string))
  (let ((name (second defsystem-form)))
    (etypecase name
      (string (string-downcase name))
      (symbol (string-downcase (symbol-name name))))))

(defun find-main-system-name (defsystem-forms)
  "Return the name of the main system from DEFSYSTEM-FORMS.
The main system is the first one whose name contains no slash."
  (declare (type list defsystem-forms)
           (values (or null string)))
  (loop :for form :in defsystem-forms
        :for name = (defsystem-name-string form)
        :unless (position #\/ name)
          :return name))


;;;;
;;;; Component Extraction
;;;;

(defun extract-component-names (defsystem-form)
  "Extract the list of Lisp source component name strings from DEFSYSTEM-FORM.
Walks the :COMPONENTS tree recursively, collecting names from
(:FILE name) entries. Module names are not included."
  (declare (type cons defsystem-form)
           (values list))
  (let ((components-plist (cddr defsystem-form))
        (result nil))
    (labels ((find-components-value (plist)
               (loop :for (key value) :on plist :by #'cddr
                     :when (eq key :components)
                       :return value))
             (walk (component-list)
               (dolist (component component-list)
                 (when (consp component)
                   (let ((kind (car component)))
                     (cond
                       ((eq kind :file)
                        (push (string-downcase
                               (etypecase (second component)
                                 (string (second component))
                                 (symbol (symbol-name (second component)))))
                              result))
                       ((eq kind :module)
                        (let ((sub-components (find-components-value
                                               (cddr component))))
                          (when sub-components
                            (walk sub-components))))))))))
      (let ((top-components (find-components-value components-plist)))
        (when top-components
          (walk top-components))))
    (nreverse result)))


;;;;
;;;; Inspector
;;;;

(defun find-component-line (content component-name)
  "Find the 1-based line number where COMPONENT-NAME appears in CONTENT."
  (declare (type string content component-name)
           (values (integer 1)))
  (let ((search-string (format nil "\"~A\"" component-name)))
    (loop :for line :in (string-lines content)
          :for line-number :from 1
          :when (search search-string line :test #'char-equal)
            :return line-number
          :finally (return 1))))

(define-file-inspector check-system-naming ((pathname pathname))
  "Check that ASDF system and component names follow canonical conventions.
Inspects .asd files only. Produces findings for:
- Non-canonical secondary system suffixes (not test/development/operation/experiment).
- Deprecated system names (e.g. /testsuite should be /test).
- Deprecated component names (entrypoint, main, testsuite)."
  (when (string-equal "asd" (pathname-type pathname))
    (let* ((forms-with-positions (read-defsystem-forms-with-positions pathname))
           (defsystem-forms (mapcar #'car forms-with-positions))
           (main-name (find-main-system-name defsystem-forms))
           (content (uiop:read-file-string pathname :external-format :utf-8))
           (content-lines (string-lines content))
           (findings nil))
      (flet ((line-length (line-number)
               (if (<= line-number (length content-lines))
                   (length (nth (1- line-number) content-lines))
                   0)))
        (when main-name
          ;; Check system names
          (dolist (entry forms-with-positions)
            (let* ((form (car entry))
                   (line-number (cdr entry))
                   (name (defsystem-name-string form))
                   (suffix (system-suffix name main-name)))
              (when suffix
                (let ((replacement (deprecated-suffix-replacement suffix))
                      (line-text (if (<= line-number (length content-lines))
                                     (nth (1- line-number) content-lines)
                                     "")))
                  (cond
                    (replacement
                     (push (make-instance 'deprecated-system-name-finding
                            :inspector 'check-system-naming
                            :severity :warning
                            :observation (format nil
                                                "System ~S uses deprecated suffix ~S."
                                                name suffix)
                            :rationale (format nil
                                              "Rename to ~A/~A for consistency."
                                              main-name replacement)
                            :file pathname
                            :line line-number
                            :column 0
                            :end-line line-number
                            :end-column (line-length line-number)
                            :source-text line-text)
                           findings))
                    ((not (canonical-suffix-p suffix))
                     (push (make-instance 'non-canonical-system-name-finding
                            :inspector 'check-system-naming
                            :severity :warning
                            :observation (format nil "System ~S uses non-canonical suffix ~S."
                                                 name suffix)
                            :rationale (format nil "Canonical suffixes are: ~{~A~^, ~}."
                                               *canonical-system-suffixes*)
                            :file pathname
                            :line line-number
                            :column 0
                            :end-line line-number
                            :end-column (line-length line-number)
                            :source-text line-text)
                           findings)))))))
          ;; Check component names in all systems
          (dolist (form defsystem-forms)
            (dolist (component-name (extract-component-names form))
              (let ((replacement (deprecated-component-replacement component-name)))
                (when replacement
                  (let* ((comp-line (find-component-line content component-name))
                         (line-text (if (<= comp-line (length content-lines))
                                        (nth (1- comp-line) content-lines)
                                        "")))
                    (push (make-instance 'deprecated-component-name-finding
                           :inspector 'check-system-naming
                           :severity :warning
                           :observation (format nil
                                               "Component ~S should be renamed to ~S."
                                               component-name replacement)
                           :rationale "File and component naming should follow project conventions."
                           :file pathname
                           :line comp-line
                           :column 0
                           :end-line comp-line
                           :end-column (line-length comp-line)
                           :source-text line-text)
                          findings))))))))
      (nreverse findings))))

;;;; End of file `check-system-naming.lisp'
