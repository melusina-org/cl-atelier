;;;; fix-constant-naming.lisp — Testsuite for the constant naming maintainer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)

(define-testcase validate-fix-constant-naming ()
  "Verify fix-constant-naming produces a text-resolution adding +plus-surrounded+ markers."
  ;; Parse "(defconstant wrong-name 42)": name CST node is (first (rest form)).
  (let* ((form (parse-cst-from-string "(defconstant wrong-name 42)"))
         (name-cst (concrete-syntax-tree:first
                    (concrete-syntax-tree:rest form)))
         (finding (make-test-syntax-finding 'atelier:constant-naming-finding name-cst form))
         (maintainer (atelier:find-maintainer 'atelier:fix-constant-naming))
         (resolution (atelier:prepare-resolution maintainer finding)))
    (assert-type resolution 'atelier:text-resolution)
    (assert-string= "+wrong-name+" (atelier:resolution-replacement resolution))
    (assert-eq 'atelier:fix-constant-naming (atelier:resolution-maintainer resolution))))

(define-testcase testsuite-fix-constant-naming ()
  "Run all fix-constant-naming tests."
  (validate-fix-constant-naming))

;;;; End of file `fix-constant-naming.lisp'
