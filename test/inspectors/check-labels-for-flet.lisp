;;;; check-labels-for-flet.lisp — Tests for the spurious LABELS inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/test)


;;;;
;;;; Unit Tests for Call-Graph Helpers
;;;;

(define-testcase validate-collect-calls-no-locals ()
  "Verify that COLLECT-CALLS-IN-FORMS returns NIL when no local names are present."
  (let ((local-set (make-hash-table :test #'eq)))
    (assert-t (null (atelier:collect-calls-in-forms
                     '((mapcar #'double items)) local-set)))))

(define-testcase validate-collect-calls-direct-call ()
  "Verify that a direct call to a local name is detected."
  (let ((local-set (make-hash-table :test #'eq)))
    (setf (gethash 'leaf local-set) t)
    (let ((result (atelier:collect-calls-in-forms
                   '((leaf x)) local-set)))
      (assert-t (not (null (member 'leaf result :test #'eq)))))))

(define-testcase validate-collect-calls-function-reference ()
  "Verify that a #'name reference to a local name is detected."
  (let ((local-set (make-hash-table :test #'eq)))
    (setf (gethash 'leaf local-set) t)
    (let ((result (atelier:collect-calls-in-forms
                   '((mapcar #'leaf items)) local-set)))
      (assert-t (not (null (member 'leaf result :test #'eq)))))))

(define-testcase validate-collect-calls-skips-quote ()
  "Verify that names in QUOTE forms are not treated as calls."
  (let ((local-set (make-hash-table :test #'eq)))
    (setf (gethash 'leaf local-set) t)
    (assert-t (null (atelier:collect-calls-in-forms
                     '((quote (leaf x))) local-set)))))

(define-testcase validate-labels-no-cycle-dag ()
  "Verify that a DAG call graph is correctly identified as cycle-free."
  ;; (labels ((a () (b)) (b () 42)) (a)) — a calls b, no cycle
  (assert-t (null (atelier:labels-call-graph-has-cycles-p
                   '(flet ((b ()
                             42))
                      (flet ((a ()
                               (b)))
                        (a)))))))

(define-testcase validate-labels-self-recursion-detected ()
  "Verify that a self-recursive function is detected as a cycle."
  ;; (labels ((f (n) (f (1- n)))) (f 5)) — f calls itself
  (assert-t (atelier:labels-call-graph-has-cycles-p
             '(labels ((f (n) (if (zerop n) 0 (f (1- n))))) (f 5)))))

(define-testcase validate-labels-mutual-recursion-detected ()
  "Verify that mutually recursive functions are detected as a cycle."
  ;; (labels ((even-p (n) (odd-p n)) (odd-p (n) (even-p n))) ...)
  (assert-t (atelier:labels-call-graph-has-cycles-p
             '(labels ((even-p (n) (if (zerop n) t (odd-p (1- n))))
                       (odd-p (n) (unless (zerop n) (even-p (1- n)))))
               (even-p 4)))))


(define-testcase testsuite-check-labels-for-flet ()
  "Unit tests for the call-graph helpers used by check-labels-for-flet.
The inspector itself is exercised by fixture auto-discovery
(testsuite/fixtures/inspector/check-labels-for-flet/{spurious,necessary}.lisp)."
  (validate-collect-calls-no-locals)
  (validate-collect-calls-direct-call)
  (validate-collect-calls-function-reference)
  (validate-collect-calls-skips-quote)
  (validate-labels-no-cycle-dag)
  (validate-labels-self-recursion-detected)
  (validate-labels-mutual-recursion-detected))

;;;; End of file `check-labels-for-flet.lisp'
