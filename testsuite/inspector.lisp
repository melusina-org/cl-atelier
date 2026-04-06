;;;; inspector.lisp — Tests for the inspector registry

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:atelier/testsuite)

(defmacro with-clean-inspector-registry (&body body)
  `(let ((atelier:*inspectors* (make-hash-table :test 'eq)))
     ,@body))

(define-testcase validate-define-inspector-and-list ()
  "Verify that DEFINE-INSPECTOR registers an inspector visible in LIST-INSPECTORS."
  (with-clean-inspector-registry
    (atelier:define-inspector test-encoding-inspector
      :level :file
      :description "Check that files are valid UTF-8.")
    (assert-t (not (null (member 'test-encoding-inspector (atelier:list-inspectors)))))))

(define-testcase validate-inspector-last-load-wins ()
  "Verify that redefining an inspector replaces the previous definition."
  (with-clean-inspector-registry
    (atelier:define-inspector test-redefined-inspector
      :level :file :description "Version 1")
    (atelier:define-inspector test-redefined-inspector
      :level :line :description "Version 2")
    (let ((inspector-instance (atelier:find-inspector 'test-redefined-inspector)))
      (assert-eq :line (atelier:inspector-level inspector-instance))
      (assert-string= "Version 2" (atelier:inspector-description inspector-instance)))))

(define-testcase validate-inspector-metadata ()
  "Verify that inspector metadata (level, description) is accessible."
  (with-clean-inspector-registry
    (atelier:define-inspector test-syntax-inspector
      :level :syntax
      :description "Check earmuffs on special variables.")
    (let ((inspector-instance (atelier:find-inspector 'test-syntax-inspector)))
      (assert-eq :syntax (atelier:inspector-level inspector-instance))
      (assert-string= "Check earmuffs on special variables."
                      (atelier:inspector-description inspector-instance)))))

(define-testcase validate-named-instance-identity ()
  "Verify that an inspector is identified by its symbol and retrievable by it."
  (with-clean-inspector-registry
    (atelier:define-inspector test-identity-inspector
      :level :file :description "Identity test inspector.")
    (let ((inspector-instance (atelier:find-inspector 'test-identity-inspector)))
      (assert-t (not (null inspector-instance)))
      (assert-eq 'test-identity-inspector (atelier:inspector-name inspector-instance))
      (assert-eq inspector-instance (atelier:find-inspector inspector-instance)))))

(define-testcase validate-named-instance-redefine ()
  "Verify that redefining a named instance replaces the old definition."
  (let (original-inspector redefined-inspector)
    (with-clean-inspector-registry
      (atelier:define-inspector test-redefine-inspector
        :level :file :description "Original definition.")
      (setf original-inspector (atelier:find-inspector 'test-redefine-inspector))
      (atelier:define-inspector test-redefine-inspector
        :level :line :description "Redefined definition.")
      (setf redefined-inspector (atelier:find-inspector 'test-redefine-inspector))
      (assert-t (not (eq original-inspector redefined-inspector)))
      (assert-string= "Redefined definition."
                      (atelier:inspector-description redefined-inspector)))))

(define-testcase testsuite-inspector ()
  (validate-define-inspector-and-list)
  (validate-inspector-last-load-wins)
  (validate-inspector-metadata)
  (validate-named-instance-identity)
  (validate-named-instance-redefine))

;;;; End of file `inspector.lisp'
