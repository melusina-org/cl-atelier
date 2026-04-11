;;;; tool-name-derivation.lisp — Tests for derive-tool-name-from-symbol

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

(define-testcase validate-derive-tool-name-from-atelier-mcp ()
  "A symbol in the atelier/mcp package strips the /mcp suffix."
  (assert-string= "atelier:list-inspectors"
                  (derive-tool-name-from-symbol
                   (intern "LIST-INSPECTORS" :atelier/mcp)))
  (assert-string= "atelier:probe-environment"
                  (derive-tool-name-from-symbol
                   (intern "PROBE-ENVIRONMENT" :atelier/mcp))))

(define-testcase validate-derive-tool-name-from-cl ()
  "A symbol from the COMMON-LISP package uses the shortest nickname."
  (assert-string= "cl:car"
                  (derive-tool-name-from-symbol 'common-lisp:car)))

(define-testcase validate-derive-tool-name-from-keyword ()
  "A keyword symbol becomes keyword:name."
  (assert-string= "keyword:foo"
                  (derive-tool-name-from-symbol :foo)))

(define-testcase validate-derive-tool-name-from-symbol ()
  "Combined entry point for tool-name derivation tests."
  (validate-derive-tool-name-from-atelier-mcp)
  (validate-derive-tool-name-from-cl)
  (validate-derive-tool-name-from-keyword))

;;;; End of file `tool-name-derivation.lisp'
