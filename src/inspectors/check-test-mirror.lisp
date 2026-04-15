;;;; check-test-mirror.lisp — Test system mirror inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)


;;;;
;;;; Non-Lisp Component Filtering
;;;;

(defparameter *non-mirror-component-types*
  '(:static-file)
  "List of ASDF component type keywords to exclude from mirror comparison.
These represent non-Lisp components (configuration files, static assets)
that are not expected to have test counterparts.")

(defparameter *non-mirror-component-names*
  '("project-configuration" "linter-configuration")
  "List of component names to exclude from mirror comparison.
These are infrastructure components that do not require test mirrors.")

(defun mirror-eligible-p (component-name)
  "Return T if COMPONENT-NAME should be included in mirror comparison.
Checks the built-in exclusion list and, when *LINTER-CONFIGURATION* is bound,
the project's mirror-excluded-components list."
  (declare (type string component-name)
           (values boolean))
  (and (not (member component-name *non-mirror-component-names*
                    :test #'string-equal))
       (not (and *linter-configuration*
                 (member component-name
                         (linter-configuration-mirror-excluded-components
                          *linter-configuration*)
                         :test #'string-equal)))))


;;;;
;;;; Component Extraction for Mirror Comparison
;;;;

(defun extract-mirror-components (defsystem-form)
  "Extract the ordered list of mirror-eligible component names from DEFSYSTEM-FORM.
Walks the :COMPONENTS tree, collecting (:FILE name) entries while skipping
excluded components. Both file and module names are checked against the
exclusion list: an excluded module is not descended into, so all its
children are also excluded."
  (declare (type cons defsystem-form)
           (values list))
  (let ((components-plist (cddr defsystem-form))
        (result nil))
    (labels ((find-components-value (plist)
               (loop :for (key value) :on plist :by #'cddr
                     :when (eq key :components)
                       :return value))
             (component-name-string (component)
               (string-downcase
                (etypecase (second component)
                  (string (second component))
                  (symbol (symbol-name (second component))))))
             (walk (component-list)
               (dolist (component component-list)
                 (when (consp component)
                   (let* ((kind (car component))
                          (name (component-name-string component)))
                     (when (mirror-eligible-p name)
                       (cond
                         ((eq kind :file)
                          (push name result))
                         ((eq kind :module)
                          (let ((sub-components (find-components-value
                                                 (cddr component))))
                            (when sub-components
                              (walk sub-components)))))))))))
      (let ((top-components (find-components-value components-plist)))
        (when top-components
          (walk top-components))))
    (nreverse result)))


;;;;
;;;; Inspector
;;;;

(defun find-defsystem-by-suffix (defsystem-forms main-name suffix)
  "Find the defsystem form whose name is MAIN-NAME/SUFFIX."
  (declare (type list defsystem-forms)
           (type string main-name suffix)
           (values (or null cons)))
  (let ((target (concatenate 'string main-name "/" suffix)))
    (find target defsystem-forms
          :key #'defsystem-name-string
          :test #'string-equal)))

(define-file-inspector check-test-mirror ((pathname pathname))
  "Check that the test system's component structure mirrors the main system.
Inspects .asd files only. Produces findings for:
- Components present in the main system but missing from the test system.
- Components present in both but in different order."
  (when (string-equal "asd" (pathname-type pathname))
    (let* ((defsystem-forms (mapcar #'car (read-defsystem-forms-with-positions pathname)))
           (main-name (find-main-system-name defsystem-forms))
           (findings nil))
      (when main-name
        (let ((main-form (find main-name defsystem-forms
                               :key #'defsystem-name-string
                               :test #'string-equal))
              (test-form (or (find-defsystem-by-suffix defsystem-forms main-name "test")
                             (find-defsystem-by-suffix defsystem-forms main-name "testsuite"))))
          (when (and main-form test-form)
            (let ((main-components (extract-mirror-components main-form))
                  (test-components (extract-mirror-components test-form)))
              ;; Check for missing components
              (dolist (component main-components)
                (unless (member component test-components :test #'string-equal)
                  (push (make-instance 'missing-test-component-finding
                         :inspector 'check-test-mirror
                         :severity :warning
                         :observation (format nil
                                             "Test system is missing component ~S."
                                             component)
                         :rationale "Test structure should mirror source structure."
                         :file pathname)
                        findings)))
              ;; Check order of shared components
              (let ((shared-in-main
                      (flet ((remove-item (c)
                               (member c test-components :test #'string-equal)))
                        (remove-if-not #'remove-item main-components)))
                    (shared-in-test
                      (flet ((remove-item (c)
                               (member c main-components :test #'string-equal)))
                        (remove-if-not #'remove-item test-components))))
                (when (and shared-in-main shared-in-test
                           (not (equal shared-in-main shared-in-test)))
                  (push (make-instance 'test-component-order-finding
                         :inspector 'check-test-mirror
                         :severity :warning
                         :observation (format nil
                                             "Test components are in different order. ~
                                              Main: ~{~A~^, ~}. Test: ~{~A~^, ~}."
                                             shared-in-main shared-in-test)
                         :rationale "Test structure should mirror source structure for navigability."
                         :file pathname)
                        findings)))))))
      (nreverse findings))))

;;;; End of file `check-test-mirror.lisp'
