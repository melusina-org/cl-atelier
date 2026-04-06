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
Configuration is available via *CURRENT-PROJECT-CONFIGURATION* and
*CURRENT-LINTER-CONFIGURATION*, which are bound by the runner before
this generic function is called.")
  (:method ((inspector inspector) (pathname pathname))
    "Default method returns NIL — no findings."
    nil))


;;;;
;;;; Runner
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

(defun eligible-inspector-p (inspector-instance linter-configuration)
  "Return T if INSPECTOR-INSTANCE should run given LINTER-CONFIGURATION.
An inspector is eligible if it is a file-inspector or line-inspector
and is not disabled in the linter configuration."
  (declare (type inspector inspector-instance))
  (and (or (typep inspector-instance 'file-inspector)
           (typep inspector-instance 'line-inspector))
       (not (inspector-disabled-p (inspector-name inspector-instance)
                                  linter-configuration))))

(defun run-file-inspectors (pathname project-configuration linter-configuration)
  "Run all registered file-level and line-level inspectors on PATHNAME.
Bind *CURRENT-PROJECT-CONFIGURATION* and *CURRENT-LINTER-CONFIGURATION*
for inspectors to access. Return a list of findings."
  (declare (type pathname pathname)
           (values list))
  (let ((*current-project-configuration* project-configuration)
        (*current-linter-configuration* linter-configuration)
        (findings nil))
    (loop :for inspector-name :being :the :hash-key :of *inspectors*
          :for inspector-instance = (gethash inspector-name *inspectors*)
          :when (eligible-inspector-p inspector-instance linter-configuration)
          :do (let ((inspector-findings
                      (inspect-file inspector-instance pathname)))
                (when inspector-findings
                  (flet ((apply-override (finding)
                           (apply-severity-override finding inspector-name
                                                    linter-configuration)))
                    (setf findings
                          (nconc findings
                                 (mapcar #'apply-override
                                         inspector-findings)))))))
    findings))

;;;; End of file `runner.lisp'
