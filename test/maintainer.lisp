;;;; maintainer.lisp — Tests for the maintainer registry

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)

(defmacro with-clean-maintainer-registry (&body body)
  `(let ((atelier:*maintainers* (make-hash-table :test 'eq)))
     ,@body))

(defvar *test-log* nil
  "Log used by maintainer tests to track which maintainers were called.")


;;;;
;;;; Test Finding Subclass
;;;;

(defclass test-specific-finding (atelier:file-finding)
  ()
  (:documentation "A finding subclass used only in maintainer tests."))


;;;;
;;;; Test Maintainer Definitions
;;;;

(atelier:define-automatic-maintainer test-base-maintainer
    ((finding atelier:file-finding))
  "Base maintainer for testing."
  (declare (ignore maintainer finding))
  (push :base *test-log*)
  nil)

(atelier:define-automatic-maintainer test-override-maintainer
    ((finding atelier:file-finding))
  "Override maintainer for testing."
  (:supersedes '(test-base-maintainer))
  (declare (ignore maintainer finding))
  (push :override *test-log*)
  nil)

(atelier:define-automatic-maintainer test-maximal-a
    ((finding atelier:file-finding))
  "First maximal maintainer for testing."
  (declare (ignore maintainer finding))
  (push :maximal-a *test-log*)
  nil)

(atelier:define-automatic-maintainer test-maximal-b
    ((finding atelier:file-finding))
  "Second maximal maintainer for testing."
  (declare (ignore maintainer finding))
  (push :maximal-b *test-log*)
  nil)

(atelier:define-automatic-maintainer test-nil-maintainer
    ((finding atelier:file-finding))
  "Maintainer that returns NIL for testing."
  (declare (ignore maintainer finding))
  nil)

(atelier:define-automatic-maintainer test-real-maintainer
    ((finding atelier:file-finding))
  "Maintainer that provides a real resolution for testing."
  (declare (ignore maintainer))
  (atelier:make-text-resolution
    :maintainer 'test-real-maintainer :finding finding
    :kind :automatic :description "Apply encoding fix."
    :replacement "fixed content"))

(atelier:define-automatic-maintainer test-resolution-provider
    ((finding atelier:file-finding))
  "Maintainer that provides resolutions for testing prepare-resolution."
  (declare (ignore maintainer))
  (atelier:make-text-resolution
    :maintainer 'test-resolution-provider :finding finding
    :kind :automatic :description "Provide a resolution."
    :replacement "replacement text"))

(atelier:define-automatic-maintainer test-specific-maintainer
    ((finding test-specific-finding))
  "Maintainer that only handles test-specific-finding."
  (declare (ignore maintainer finding))
  (push :specific-handled *test-log*)
  nil)


;;;;
;;;; Test Cases
;;;;

(define-testcase validate-maintainer-individual-class ()
  "Verify that DEFINE-AUTOMATIC-MAINTAINER creates a subclass with a singleton."
  (with-clean-maintainer-registry
    (atelier:define-automatic-maintainer test-class-maintainer
        ((finding atelier:file-finding))
      "Maintainer for class test."
      (declare (ignore maintainer finding))
      nil)
    (let ((instance (atelier:find-maintainer 'test-class-maintainer)))
      (assert-t (not (null instance)))
      (assert-t (typep instance 'atelier:automatic-maintainer))
      (assert-t (typep instance 'atelier:maintainer))
      (assert-eq :automatic (atelier:maintainer-kind instance))
      (assert-eq 'test-class-maintainer (atelier:maintainer-name instance)))))

(define-testcase validate-define-maintainer-and-list ()
  "Verify that DEFINE-MAINTAINER registers a maintainer visible in LIST-MAINTAINERS."
  (with-clean-maintainer-registry
    (atelier:define-automatic-maintainer test-listed-maintainer
        ((finding atelier:file-finding))
      "Listed maintainer."
      (declare (ignore maintainer finding))
      nil)
    (assert-t (not (null (member 'test-listed-maintainer
                                 (atelier:list-maintainers)))))))

(defun make-test-file-finding ()
  "Create a FILE-FINDING instance for use in maintainer tests."
  (atelier:make-file-finding
    :inspector :test-inspector :severity :warning
    :observation "Test observation for maintainer dispatch."
    :rationale "Test rationale."
    :file #p"src/example.lisp"))

(defmacro with-test-maintainers (names &body body)
  "Execute BODY with a clean maintainer registry containing only the named maintainers.
The maintainer singletons are captured from the global registry before rebinding."
  (let ((instances-var (gensym "INSTANCES")))
    `(let ((,instances-var
             (loop :for name :in ',names
                   :for instance = (gethash name atelier:*maintainers*)
                   :when instance
                   :collect (cons name instance))))
       (let ((atelier:*maintainers* (make-hash-table :test 'eq)))
         (loop :for (name . instance) :in ,instances-var
               :do (setf (gethash name atelier:*maintainers*) instance))
         ,@body))))

(define-testcase validate-maintainer-superseding ()
  "Verify that a superseding maintainer is called instead of the superseded one."
  (setf *test-log* nil)
  (with-test-maintainers (test-base-maintainer test-override-maintainer)
    (atelier:resolve-finding (make-test-file-finding))
    (assert-t (not (null (member :override *test-log*))))
    (assert-t (null (member :base *test-log*)))))

(define-testcase validate-maintainer-both-maximal ()
  "Verify that two unrelated maintainers are both called."
  (setf *test-log* nil)
  (with-test-maintainers (test-maximal-a test-maximal-b)
    (atelier:resolve-finding (make-test-file-finding))
    (assert-t (not (null (member :maximal-a *test-log*))))
    (assert-t (not (null (member :maximal-b *test-log*))))))

(define-testcase validate-maintainer-nil-fallthrough ()
  "Verify that NIL results are excluded and real results collected."
  (with-test-maintainers (test-nil-maintainer test-real-maintainer)
    (let ((resolutions (atelier:resolve-finding (make-test-file-finding))))
      (assert-eq 1 (length resolutions))
      (assert-t (typep (first resolutions) 'atelier:text-resolution)))))

(define-testcase validate-prepare-resolution-returns-resolution ()
  "Verify that PREPARE-RESOLUTION returns a RESOLUTION."
  (let* ((finding (make-test-file-finding))
         (instance (atelier:find-maintainer 'test-resolution-provider))
         (result (atelier:prepare-resolution instance finding)))
    (assert-t (typep result 'atelier:resolution))))

(define-testcase validate-prepare-resolution-returns-nil ()
  "Verify that PREPARE-RESOLUTION returns NIL for an unmatched maintainer."
  (let* ((finding (make-test-file-finding))
         (result (atelier:prepare-resolution
                   (make-instance 'atelier:automatic-maintainer
                     :name :unregistered-maintainer)
                   finding)))
    (assert-t (not result))))

(define-testcase validate-maintainer-finding-class-dispatch ()
  "Verify that a maintainer only runs for finding types matching its specialiser."
  (setf *test-log* nil)
  (with-test-maintainers (test-specific-maintainer)
    ;; A plain file-finding should not trigger the specific maintainer.
    (atelier:resolve-finding (make-test-file-finding))
    (assert-t (null *test-log*))
    ;; A test-specific-finding should trigger it.
    (let ((specific-finding
            (make-instance 'test-specific-finding
              :inspector :test-inspector :severity :warning
              :observation "Specific test observation."
              :rationale "Specific test rationale."
              :file #p"src/example.lisp")))
      (atelier:resolve-finding specific-finding)
      (assert-t (not (null (member :specific-handled *test-log*)))))))

(define-testcase testsuite-maintainer ()
  (validate-maintainer-individual-class)
  (validate-define-maintainer-and-list)
  (validate-maintainer-superseding)
  (validate-maintainer-both-maximal)
  (validate-maintainer-nil-fallthrough)
  (validate-prepare-resolution-returns-resolution)
  (validate-prepare-resolution-returns-nil)
  (validate-maintainer-finding-class-dispatch))

;;;; End of file `maintainer.lisp'
