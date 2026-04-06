;;;; runner.lisp — Inspector runner for Atelier

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:atelier)


;;;;
;;;; Inspection Protocol
;;;;

(defgeneric inspect-file (inspector pathname)
  (:documentation "Run INSPECTOR on the file at PATHNAME and return a list of findings or NIL.
Used by file-level inspectors. Configuration is available via
*CURRENT-PROJECT-CONFIGURATION* and *CURRENT-LINTER-CONFIGURATION*.")
  (:method ((inspector inspector) (pathname pathname))
    "Default method returns NIL — no findings."
    nil))

(defgeneric inspect-lines (inspector pathname lines)
  (:documentation "Run INSPECTOR on LINES read from PATHNAME.
Return a list of findings or NIL. LINES is a vector of strings,
one per line. Used by line-level inspectors. The runner reads the
file into LINES once and passes it to all line inspectors.")
  (:method ((inspector inspector) (pathname pathname) (lines vector))
    "Default method returns NIL — no findings."
    nil))


;;;;
;;;; Helpers
;;;;

(defun inspector-disabled-p (inspector-name linter-configuration)
  "Return T if INSPECTOR-NAME is disabled in LINTER-CONFIGURATION."
  (declare (type symbol inspector-name))
  (when linter-configuration
    (member inspector-name
            (linter-configuration-disabled-inspectors linter-configuration)
            :test #'eq)))

(defun apply-severity-override (finding inspector-name linter-configuration)
  "Return FINDING with its severity overridden if LINTER-CONFIGURATION specifies one.
Return FINDING unchanged if no override applies."
  (declare (type finding finding)
           (type symbol inspector-name))
  (when linter-configuration
    (let ((override (assoc inspector-name
                          (linter-configuration-severity-overrides linter-configuration)
                          :test #'eq)))
      (when override
        (setf (slot-value finding 'severity) (cdr override)))))
  finding)

(defun collect-inspectors-by-level (level-class linter-configuration)
  "Return all registered inspectors of LEVEL-CLASS that are not disabled."
  (declare (type symbol level-class))
  (loop :for inspector-name :being :the :hash-key :of *inspectors*
        :for inspector-instance = (gethash inspector-name *inspectors*)
        :when (and (typep inspector-instance level-class)
                   (not (inspector-disabled-p inspector-name
                                              linter-configuration)))
        :collect inspector-instance))

(defun collect-and-override-findings (inspector-instance findings linter-configuration)
  "Apply severity overrides to FINDINGS from INSPECTOR-INSTANCE and return them."
  (declare (type inspector inspector-instance)
           (type list findings))
  (let ((inspector-name (inspector-name inspector-instance)))
    (flet ((apply-override (finding)
             (apply-severity-override finding inspector-name linter-configuration)))
      (mapcar #'apply-override findings))))

(defun read-file-into-line-vector (pathname)
  "Read PATHNAME into a vector of strings, one per line."
  (declare (type pathname pathname)
           (values vector))
  (with-open-file (stream pathname :direction :input :external-format :utf-8)
    (coerce
     (loop :for line = (read-line stream nil nil)
           :while line
           :collect line)
     'vector)))


;;;;
;;;; Runner
;;;;

(defun run-file-inspectors (pathname project-configuration linter-configuration)
  "Run all registered inspectors on PATHNAME in pipeline order.
Stage 1: run file-inspector instances on the pathname.
Stage 2: read the file into lines once, run line-inspector instances on the lines.
Bind *CURRENT-PROJECT-CONFIGURATION* and *CURRENT-LINTER-CONFIGURATION*
for inspectors to access. Return a list of findings."
  (declare (type pathname pathname)
           (values list))
  (let ((*current-project-configuration* project-configuration)
        (*current-linter-configuration* linter-configuration)
        (findings nil))
    ;; Stage 1: file inspectors
    (let ((file-inspectors
            (collect-inspectors-by-level 'file-inspector linter-configuration)))
      (loop :for inspector-instance :in file-inspectors
            :for inspector-findings = (inspect-file inspector-instance pathname)
            :when inspector-findings
            :do (setf findings
                      (nconc findings
                             (collect-and-override-findings
                              inspector-instance inspector-findings
                              linter-configuration)))))
    ;; Stage 2: line inspectors (read file once)
    (let ((line-inspectors
            (collect-inspectors-by-level 'line-inspector linter-configuration)))
      (when line-inspectors
        (let ((lines (read-file-into-line-vector pathname)))
          (loop :for inspector-instance :in line-inspectors
                :for inspector-findings =
                  (inspect-lines inspector-instance pathname lines)
                :when inspector-findings
                :do (setf findings
                          (nconc findings
                                 (collect-and-override-findings
                                  inspector-instance inspector-findings
                                  linter-configuration)))))))
    findings))

;;;; End of file `runner.lisp'
