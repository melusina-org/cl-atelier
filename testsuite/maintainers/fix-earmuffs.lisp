;;;; fix-earmuffs.lisp — Testsuite for the earmuffs maintainer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)


;;;;
;;;; Helpers
;;;;

(defun parse-cst-from-string (source)
  "Parse the first top-level CST form from SOURCE string using Eclector."
  (with-input-from-string (stream source)
    (eclector.concrete-syntax-tree:read stream nil nil)))

(defun make-test-syntax-finding (finding-class cst-node cst-root)
  "Construct a FINDING-CLASS instance pointing to CST-NODE in a dummy file."
  (make-instance finding-class
    :inspector :test :severity :style
    :observation "" :rationale ""
    :file #p"/tmp/test.lisp"
    :line 1 :column 0 :end-line 1 :end-column 1
    :source-text ""
    :cst-node cst-node
    :cst-root cst-root))


;;;;
;;;; Testcases
;;;;

(define-testcase validate-fix-earmuffs ()
  "Verify fix-earmuffs produces a text-resolution adding *earmuffs* to the name."
  ;; Parse "(defvar wrong-name 42)": name CST node is (first (rest form)).
  (let* ((form (parse-cst-from-string "(defvar wrong-name 42)"))
         (name-cst (concrete-syntax-tree:first
                    (concrete-syntax-tree:rest form)))
         (finding (make-test-syntax-finding 'atelier:earmuffs-finding name-cst form))
         (maintainer (atelier:find-maintainer 'atelier:fix-earmuffs))
         (resolution (atelier:prepare-resolution maintainer finding)))
    (assert-type resolution 'atelier:text-resolution)
    (assert-string= "*wrong-name*" (atelier:resolution-replacement resolution))
    (assert-eq 'atelier:fix-earmuffs (atelier:resolution-maintainer resolution))))

(define-testcase validate-syntax-text-maintainers-registered ()
  "Verify fix-earmuffs, fix-constant-naming, and fix-bare-loop-keywords are registered."
  (assert-t (not (null (atelier:find-maintainer 'atelier:fix-earmuffs))))
  (assert-t (not (null (atelier:find-maintainer 'atelier:fix-constant-naming))))
  (assert-t (not (null (atelier:find-maintainer 'atelier:fix-bare-loop-keywords)))))

(define-testcase testsuite-fix-earmuffs ()
  "Run all fix-earmuffs tests."
  (validate-fix-earmuffs)
  (validate-syntax-text-maintainers-registered))

;;;; End of file 'fix-earmuffs.lisp'
