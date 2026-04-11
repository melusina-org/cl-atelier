;;;; uri-template.lisp — Tests for parse/match/validate-uri-template

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

(define-testcase validate-uri-template-parses-static ()
  "A URI with no placeholders parses to a single literal segment."
  (let ((segments (parse-uri-template "atelier://inspectors")))
    (assert-equal '((:literal "atelier://inspectors")) segments)
    (assert-t (uri-template-static-p "atelier://inspectors"))))

(define-testcase validate-uri-template-parses-one-placeholder ()
  "A URI with one {name} placeholder parses to literal + placeholder."
  (let ((segments (parse-uri-template "atelier://inspectors/{name}")))
    (assert-equal '((:literal "atelier://inspectors/")
                    (:placeholder "name"))
                  segments)
    (assert-nil (uri-template-static-p "atelier://inspectors/{name}"))))

(define-testcase validate-uri-template-parses-two-placeholders ()
  "Two placeholders separated by literal segments parse correctly."
  (let ((segments (parse-uri-template "lisp://things/{x}/bits/{y}.txt")))
    (assert-equal '((:literal "lisp://things/")
                    (:placeholder "x")
                    (:literal "/bits/")
                    (:placeholder "y")
                    (:literal ".txt"))
                  segments)))

(define-testcase validate-uri-template-matches-static ()
  "A static URI matches itself and returns empty bindings + MATCHED T."
  (multiple-value-bind (bindings matched-p)
      (match-uri-against-template "atelier://inspectors" "atelier://inspectors")
    (assert-t matched-p)
    (assert-nil bindings)))

(define-testcase validate-uri-template-fails-static-mismatch ()
  "A non-matching static URI returns MATCHED-P NIL."
  (multiple-value-bind (bindings matched-p)
      (match-uri-against-template "atelier://maintainers" "atelier://inspectors")
    (assert-nil matched-p)
    (assert-nil bindings)))

(define-testcase validate-uri-template-matches-templated ()
  "A templated URI extracts the placeholder binding."
  (multiple-value-bind (bindings matched-p)
      (match-uri-against-template "atelier://inspectors/check-earmuffs"
                                  "atelier://inspectors/{name}")
    (assert-t matched-p)
    (assert-equal '(("name" . "check-earmuffs")) bindings)))

(define-testcase validate-uri-template-matches-templated-with-suffix ()
  "A templated URI with a literal suffix matches up to that suffix."
  (multiple-value-bind (bindings matched-p)
      (match-uri-against-template "lisp://transcript/abc123.sexp"
                                  "lisp://transcript/{session-id}.sexp")
    (assert-t matched-p)
    (assert-equal '(("session-id" . "abc123")) bindings)))

(define-testcase validate-uri-template-matches-two-placeholders ()
  "Two placeholders with a literal between them extract both bindings."
  (multiple-value-bind (bindings matched-p)
      (match-uri-against-template "lisp://things/foo/bits/bar.txt"
                                  "lisp://things/{x}/bits/{y}.txt")
    (assert-t matched-p)
    (assert-equal '(("x" . "foo") ("y" . "bar")) bindings)))

(define-testcase validate-uri-template-validation-rejects-mismatch ()
  "validate-uri-template-against-lambda-list signals INVALID-URI-TEMPLATE."
  (assert-condition
   (validate-uri-template-against-lambda-list
    "atelier://inspectors/{name}" '(&key other))
   invalid-uri-template))

(define-testcase validate-uri-template-validation-accepts-match ()
  "A matching template + lambda list returns T."
  (assert-t
   (validate-uri-template-against-lambda-list
    "atelier://inspectors/{name}" '(&key name))))

(define-testcase validate-uri-template-tests ()
  (validate-uri-template-parses-static)
  (validate-uri-template-parses-one-placeholder)
  (validate-uri-template-parses-two-placeholders)
  (validate-uri-template-matches-static)
  (validate-uri-template-fails-static-mismatch)
  (validate-uri-template-matches-templated)
  (validate-uri-template-matches-templated-with-suffix)
  (validate-uri-template-matches-two-placeholders)
  (validate-uri-template-validation-rejects-mismatch)
  (validate-uri-template-validation-accepts-match))

;;;; End of file `uri-template.lisp'
