;;;; resource-read.lisp — Per-resource tests for resource reads

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

(define-testcase validate-read-atelier-inspectors ()
  "atelier://inspectors matches a concrete resource."
  (multiple-value-bind (tool bindings)
      (match-resource-uri "atelier://inspectors")
    (assert-t (not (null tool)))
    (assert-nil bindings)
    (assert-string= "application/json" (resource-mime-type tool))))

(define-testcase validate-read-atelier-maintainers ()
  "atelier://maintainers matches a concrete resource."
  (multiple-value-bind (tool bindings)
      (match-resource-uri "atelier://maintainers")
    (assert-t (not (null tool)))
    (assert-nil bindings)))

(define-testcase validate-read-lisp-systems ()
  "lisp://systems matches a concrete resource and the handler returns a list."
  (multiple-value-bind (tool bindings)
      (match-resource-uri "lisp://systems")
    (assert-t (not (null tool)))
    (assert-nil bindings)
    (assert-t (listp (handle-tool-call tool nil)))))

(define-testcase validate-read-inspector-detail-template ()
  "atelier://inspectors/check-earmuffs matches the inspector-detail template."
  (multiple-value-bind (tool bindings)
      (match-resource-uri "atelier://inspectors/check-earmuffs")
    (assert-t (not (null tool)))
    (assert-equal '(("name" . "check-earmuffs")) bindings)))

(define-testcase validate-read-maintainer-detail-template ()
  "atelier://maintainers/fix-trailing-whitespace matches the maintainer-detail template."
  (multiple-value-bind (tool bindings)
      (match-resource-uri "atelier://maintainers/fix-trailing-whitespace")
    (assert-t (not (null tool)))
    (assert-equal '(("name" . "fix-trailing-whitespace")) bindings)))

(define-testcase validate-read-unknown-inspector-raises-resource-not-found ()
  "Calling the inspector-detail handler with an unknown name raises
   a RESOURCE-NOT-FOUND condition."
  (let* ((tool (match-resource-uri "atelier://inspectors/totally-not-real")))
    (assert-condition
     (handle-tool-call tool '(("name" . "totally-not-real")))
     resource-not-found)))

(define-testcase validate-transcript-sexp-template-matches ()
  "lisp://transcript/{session-id}.sexp matches its template."
  (multiple-value-bind (tool bindings)
      (match-resource-uri "lisp://transcript/abc123.sexp")
    (assert-t (not (null tool)))
    (assert-equal '(("session-id" . "abc123")) bindings)))

(define-testcase validate-transcript-json-template-matches ()
  "lisp://transcript/{session-id}.json matches its template."
  (multiple-value-bind (tool bindings)
      (match-resource-uri "lisp://transcript/abc123.json")
    (assert-t (not (null tool)))
    (assert-equal '(("session-id" . "abc123")) bindings)))

(define-testcase validate-transcript-markdown-template-matches ()
  "lisp://transcript/{session-id}.md matches its template."
  (multiple-value-bind (tool bindings)
      (match-resource-uri "lisp://transcript/abc123.md")
    (assert-t (not (null tool)))
    (assert-equal '(("session-id" . "abc123")) bindings)))

(define-testcase validate-resource-read-tests ()
  (validate-read-atelier-inspectors)
  (validate-read-atelier-maintainers)
  (validate-read-lisp-systems)
  (validate-read-inspector-detail-template)
  (validate-read-maintainer-detail-template)
  (validate-read-unknown-inspector-raises-resource-not-found)
  (validate-transcript-sexp-template-matches)
  (validate-transcript-json-template-matches)
  (validate-transcript-markdown-template-matches))

;;;; End of file `resource-read.lisp'
