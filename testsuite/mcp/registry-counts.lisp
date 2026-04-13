;;;; registry-counts.lisp — Drift detector for the tool/resource registries

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

;;; Slice 009 ships exactly 6 tools, 3 concrete resources, and 5
;;; URI-templated resources. These counts are asserted here as a
;;; drift detector — any change to the registry is caught on every
;;; test run, catching documentation drift patterns (see
;;; product/knowledge/patterns.md).

(define-testcase validate-tool-count-is-six ()
  "Exactly 18 tools are registered (6 slice-009 + 8 slice-010 + 4 slice-011)."
  (assert-eql 18 (length (list-tools))))

(define-testcase validate-concrete-resource-count-is-three ()
  "Exactly three concrete resources are registered."
  (assert-eql 3 (length (list-concrete-resources))))

(define-testcase validate-template-resource-count-is-five ()
  "Exactly five URI-templated resources are registered."
  (assert-eql 5 (length (list-template-resources))))

(define-testcase validate-expected-tool-names ()
  "The expected six tool names are all present."
  (dolist (name '("atelier:probe-environment"
                  "atelier:list-inspectors"
                  "atelier:list-maintainers"
                  "atelier:list-systems"
                  "atelier:inspector-detail"
                  "atelier:maintainer-detail"))
    (assert-t (not (null (find-tool-by-name name))))))

(define-testcase validate-registry-counts ()
  (validate-tool-count-is-six)
  (validate-concrete-resource-count-is-three)
  (validate-template-resource-count-is-five)
  (validate-expected-tool-names))

;;;; End of file `registry-counts.lisp'
