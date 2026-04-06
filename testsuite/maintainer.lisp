;;;; maintainer.lisp — Tests for the maintainer registry

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

(defmacro with-clean-maintainer-registry (&body body)
  `(let ((atelier:*maintainers* (make-hash-table :test 'eq)))
     ,@body))

(defvar *test-log* nil
  "Log used by maintainer tests to track which maintainers were called.")

(define-testcase validate-define-maintainer-and-list ()
  "Verify that DEFINE-MAINTAINER registers a maintainer visible in LIST-MAINTAINERS."
  (with-clean-maintainer-registry
    (atelier:define-maintainer test-encoding-maintainer
        ((finding atelier:file-finding))
      "Fix encoding issues."
      (declare (ignore finding))
      nil)
    (assert-t (not (null (member 'test-encoding-maintainer
                                 (atelier:list-maintainers)))))))

(define-testcase validate-maintainer-superseding ()
  "Verify that a superseding maintainer is called instead of the superseded one."
  (with-clean-maintainer-registry
    (setf *test-log* nil)
    (atelier:define-maintainer test-base-maintainer
        ((finding atelier:file-finding))
      "Base maintainer."
      (declare (ignore finding))
      (push :base *test-log*)
      nil)
    (atelier:define-maintainer test-override-maintainer
        ((finding atelier:file-finding))
      "Override maintainer."
      (:supersedes '(test-base-maintainer))
      (declare (ignore finding))
      (push :override *test-log*)
      nil)
    (let ((finding (atelier:make-file-finding
                     :inspector :encoding-check :severity :warning
                     :observation "File is not UTF-8."
                     :rationale "Encoding consistency."
                     :file #p"src/example.lisp")))
      (atelier:resolve-finding finding)
      (assert-t (not (null (member :override *test-log*))))
      (assert-t (null (member :base *test-log*))))))

(define-testcase validate-maintainer-both-maximal ()
  "Verify that two unrelated maintainers are both called when both are maximal."
  (with-clean-maintainer-registry
    (setf *test-log* nil)
    (atelier:define-maintainer test-maximal-a
        ((finding atelier:file-finding))
      "First maximal maintainer."
      (declare (ignore finding))
      (push :maximal-a *test-log*)
      nil)
    (atelier:define-maintainer test-maximal-b
        ((finding atelier:file-finding))
      "Second maximal maintainer."
      (declare (ignore finding))
      (push :maximal-b *test-log*)
      nil)
    (let ((finding (atelier:make-file-finding
                     :inspector :encoding-check :severity :warning
                     :observation "File is not UTF-8."
                     :rationale "Encoding consistency."
                     :file #p"src/example.lisp")))
      (atelier:resolve-finding finding)
      (assert-t (not (null (member :maximal-a *test-log*))))
      (assert-t (not (null (member :maximal-b *test-log*)))))))

(define-testcase validate-maintainer-nil-fallthrough ()
  "Verify that a maintainer returning NIL does not produce a resolution,
and other maximal maintainers still produce theirs."
  (with-clean-maintainer-registry
    (atelier:define-maintainer test-nil-maintainer
        ((finding atelier:file-finding))
      "Returns NIL for all findings."
      (declare (ignore finding))
      nil)
    (atelier:define-maintainer test-real-maintainer
        ((finding atelier:file-finding))
      "Provides a real resolution."
      (atelier:make-text-resolution
        :maintainer 'test-real-maintainer :finding finding
        :kind :automatic :description "Apply encoding fix."
        :replacement "fixed content"))
    (let* ((finding (atelier:make-file-finding
                      :inspector :encoding-check :severity :warning
                      :observation "File is not UTF-8."
                      :rationale "Encoding consistency."
                      :file #p"src/example.lisp"))
           (resolutions (atelier:resolve-finding finding)))
      (assert-eq 1 (length resolutions))
      (assert-t (typep (first resolutions) 'atelier:text-resolution)))))

(define-testcase validate-prepare-resolution-returns-resolution ()
  "Verify that PREPARE-RESOLUTION returns a RESOLUTION when the method provides one."
  (with-clean-maintainer-registry
    (atelier:define-maintainer test-resolution-provider
        ((finding atelier:file-finding))
      "Provides resolutions for file findings."
      (atelier:make-text-resolution
        :maintainer 'test-resolution-provider :finding finding
        :kind :automatic :description "Provide a resolution."
        :replacement "replacement text"))
    (let* ((finding (atelier:make-file-finding
                      :inspector :encoding-check :severity :warning
                      :observation "File is not UTF-8."
                      :rationale "Encoding consistency."
                      :file #p"src/example.lisp"))
           (result (atelier:prepare-resolution 'test-resolution-provider finding)))
      (assert-t (typep result 'atelier:resolution)))))

(define-testcase validate-prepare-resolution-returns-nil ()
  "Verify that PREPARE-RESOLUTION returns NIL when no method is specialised."
  (let* ((finding (atelier:make-file-finding
                    :inspector :encoding-check :severity :warning
                    :observation "File is not UTF-8."
                    :rationale "Encoding consistency."
                    :file #p"src/example.lisp"))
         (result (atelier:prepare-resolution :nonexistent-maintainer finding)))
    (assert-t (not result))))

(define-testcase validate-maintainer-finding-type-dispatch ()
  "Verify that a maintainer only runs for finding types matching its specialiser."
  (with-clean-maintainer-registry
    (setf *test-log* nil)
    (atelier:define-maintainer test-line-only-maintainer
        ((finding atelier:line-finding))
      "Handles only line findings."
      (declare (ignore finding))
      (push :line-handled *test-log*)
      nil)
    ;; A file-finding should not trigger the line-finding method.
    (let ((file-finding (atelier:make-file-finding
                          :inspector :encoding-check :severity :warning
                          :observation "File is not UTF-8."
                          :rationale "Encoding consistency."
                          :file #p"src/example.lisp")))
      (atelier:resolve-finding file-finding)
      (assert-t (null *test-log*)))
    ;; A line-finding should trigger it.
    (let ((line-finding (atelier:make-line-finding
                          :inspector :long-line-check :severity :style
                          :observation "Line is 142 characters long."
                          :rationale "Long lines reduce readability."
                          :file #p"src/example.lisp"
                          :line 42 :column 0 :end-line 42 :end-column 142
                          :source-text "(very long line)")))
      (atelier:resolve-finding line-finding)
      (assert-t (not (null (member :line-handled *test-log*)))))))

(define-testcase testsuite-maintainer ()
  (validate-define-maintainer-and-list)
  (validate-maintainer-superseding)
  (validate-maintainer-both-maximal)
  (validate-maintainer-nil-fallthrough)
  (validate-prepare-resolution-returns-resolution)
  (validate-prepare-resolution-returns-nil)
  (validate-maintainer-finding-type-dispatch))

;;;; End of file `maintainer.lisp'
