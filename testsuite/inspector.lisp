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

(define-testcase validate-inspector-individual-class ()
  "Verify that DEFINE-FILE-INSPECTOR creates a subclass with a singleton instance."
  (with-clean-inspector-registry
    (atelier:define-file-inspector test-encoding-inspector
        ((pathname pathname))
      "Check encoding for testing purposes."
      nil)
    (let ((instance (atelier:find-inspector 'test-encoding-inspector)))
      (assert-t (not (null instance)))
      (assert-t (typep instance 'atelier:file-inspector))
      (assert-t (typep instance 'atelier:inspector))
      (assert-eq :file (atelier:inspector-level instance))
      (assert-eq 'test-encoding-inspector (atelier:inspector-name instance)))))

(define-testcase validate-define-inspector-and-list ()
  "Verify that DEFINE-INSPECTOR registers an inspector visible in LIST-INSPECTORS."
  (with-clean-inspector-registry
    (atelier:define-file-inspector test-list-inspector
        ((pathname pathname))
      "Inspector for list test."
      nil)
    (assert-t (not (null (member 'test-list-inspector (atelier:list-inspectors)))))))

(define-testcase validate-inspector-last-load-wins ()
  "Verify that redefining an inspector replaces the previous registration."
  (with-clean-inspector-registry
    (atelier:define-file-inspector test-redefined-inspector
        ((pathname pathname))
      "Version 1."
      nil)
    (atelier:define-line-inspector test-redefined-inspector
        ((pathname pathname) (lines vector))
      "Version 2."
      nil)
    (let ((instance (atelier:find-inspector 'test-redefined-inspector)))
      (assert-eq :line (atelier:inspector-level instance))
      (assert-string= "Version 2." (atelier:inspector-description instance)))))

(define-testcase validate-inspector-metadata ()
  "Verify that inspector metadata is accessible."
  (with-clean-inspector-registry
    (atelier:define-syntax-inspector test-syntax-inspector
        ((pathname pathname))
      "Check earmuffs on special variables."
      nil)
    (let ((instance (atelier:find-inspector 'test-syntax-inspector)))
      (assert-eq :syntax (atelier:inspector-level instance))
      (assert-string= "Check earmuffs on special variables."
                      (atelier:inspector-description instance)))))

(define-testcase validate-named-instance-identity ()
  "Verify that an inspector is identified by its symbol."
  (with-clean-inspector-registry
    (atelier:define-file-inspector test-identity-inspector
        ((pathname pathname))
      "Identity test inspector."
      nil)
    (let ((instance (atelier:find-inspector 'test-identity-inspector)))
      (assert-t (not (null instance)))
      (assert-eq 'test-identity-inspector (atelier:inspector-name instance))
      (assert-eq instance (atelier:find-inspector instance)))))

(define-testcase validate-named-instance-redefine ()
  "Verify that redefining a named instance replaces the old definition."
  (let (original-inspector redefined-inspector)
    (with-clean-inspector-registry
      (atelier:define-file-inspector test-redefine-inspector
          ((pathname pathname))
        "Original definition."
        nil)
      (setf original-inspector (atelier:find-inspector 'test-redefine-inspector))
      (atelier:define-file-inspector test-redefine-inspector
          ((pathname pathname))
        "Redefined definition."
        nil)
      (setf redefined-inspector (atelier:find-inspector 'test-redefine-inspector))
      (assert-t (not (eq original-inspector redefined-inspector)))
      (assert-string= "Redefined definition."
                      (atelier:inspector-description redefined-inspector)))))

(define-testcase testsuite-inspector ()
  (validate-inspector-individual-class)
  (validate-define-inspector-and-list)
  (validate-inspector-last-load-wins)
  (validate-inspector-metadata)
  (validate-named-instance-identity)
  (validate-named-instance-redefine))

;;;; End of file `inspector.lisp'
