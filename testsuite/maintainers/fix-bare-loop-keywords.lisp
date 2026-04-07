;;;; fix-bare-loop-keywords.lisp — Testsuite for the bare loop keywords maintainer

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

(define-testcase validate-fix-bare-loop-keywords ()
  "Verify fix-bare-loop-keywords replaces a bare LOOP keyword with its keyword form."
  ;; Parse "(loop for item in items collect item)".
  ;; The 'for' symbol is (first (rest form)).
  (let* ((form (parse-cst-from-string "(loop for item in items collect item)"))
         (loop-body (concrete-syntax-tree:rest form))
         ;; First element of loop body is the bare keyword 'for'.
         (for-cst (concrete-syntax-tree:first loop-body))
         (finding (make-test-syntax-finding 'atelier:bare-loop-keyword-finding for-cst form))
         (maintainer (atelier:find-maintainer 'atelier:fix-bare-loop-keywords))
         (resolution (atelier:prepare-resolution maintainer finding)))
    (assert-type resolution 'atelier:text-resolution)
    (assert-string= ":for" (atelier:resolution-replacement resolution))
    (assert-eq 'atelier:fix-bare-loop-keywords (atelier:resolution-maintainer resolution))))

(define-testcase testsuite-fix-bare-loop-keywords ()
  "Run all fix-bare-loop-keywords tests."
  (validate-fix-bare-loop-keywords))

;;;; End of file 'fix-bare-loop-keywords.lisp'
