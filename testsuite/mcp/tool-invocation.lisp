;;;; tool-invocation.lisp — Per-tool tests for the six shipped tools

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

(defun %alist-has-key-p (alist key)
  "Return T if ALIST contains KEY under the string-equal test."
  (not (null (assoc key alist :test #'equal))))

(define-testcase validate-probe-environment-returns-env-alist ()
  "atelier:probe-environment returns at minimum lisp-implementation-type."
  (let* ((tool (find-tool-by-name "atelier:probe-environment"))
         (result (handle-tool-call tool nil)))
    (assert-t (not (null tool)))
    (assert-t (%alist-has-key-p result "lisp-implementation-type"))
    (assert-t (%alist-has-key-p result "lisp-implementation-version"))
    (assert-t (%alist-has-key-p result "machine-type"))))

(define-testcase validate-list-inspectors-returns-list ()
  "atelier:list-inspectors returns a non-empty list of alists, each with name, level, description."
  (let* ((tool (find-tool-by-name "atelier:list-inspectors"))
         (result (handle-tool-call tool nil)))
    (assert-t (not (null tool)))
    (assert-t (listp result))
    (assert-t (plusp (length result)))
    (let ((first (first result)))
      (assert-t (%alist-has-key-p first "name"))
      (assert-t (%alist-has-key-p first "level"))
      (assert-t (%alist-has-key-p first "description")))))

(define-testcase validate-list-maintainers-returns-list ()
  "atelier:list-maintainers returns a non-empty list with kind and maturity."
  (let* ((tool (find-tool-by-name "atelier:list-maintainers"))
         (result (handle-tool-call tool nil)))
    (assert-t (plusp (length result)))
    (let ((first (first result)))
      (assert-t (%alist-has-key-p first "kind"))
      (assert-t (%alist-has-key-p first "maturity")))))

(define-testcase validate-list-systems-does-not-load-any-system ()
  "atelier:list-systems returns system names from the source registry
   without calling asdf:load-system."
  (let* ((before (length (asdf:already-loaded-systems)))
         (tool (find-tool-by-name "atelier:list-systems"))
         (result (handle-tool-call tool nil))
         (after  (length (asdf:already-loaded-systems))))
    (assert-eql before after)
    (assert-t (listp result))))

(define-testcase validate-tool-dispatch-is-specialized ()
  "The list-inspectors tool dispatches via a method specialized on its
   own generated class, not the base TOOL class."
  (let* ((tool (find-tool-by-name "atelier:list-inspectors"))
         (method (find-method #'handle-tool-call '()
                              (list (class-of tool) (find-class t))
                              nil)))
    (assert-t (not (null method)))))

(define-testcase validate-tool-invocation-tests ()
  (validate-probe-environment-returns-env-alist)
  (validate-list-inspectors-returns-list)
  (validate-list-maintainers-returns-list)
  (validate-list-systems-does-not-load-any-system)
  (validate-tool-dispatch-is-specialized))

;;;; End of file `tool-invocation.lisp'
