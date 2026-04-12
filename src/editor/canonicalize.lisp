;;;; canonicalize.lisp — Normalize a toplevel form via the lint pipeline

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/editor)


;;;;
;;;; Entry Point
;;;;

(defun normalize-toplevel-form (form)
  "Run the Atelier lint + maintainer pipeline on FORM and return
\(VALUES normalized-form findings).

NORMALIZED-FORM is a new TOPLEVEL-FORM with maintainers applied
\(earmuffs, loop keywords, bare-lambda, etc). FINDINGS is a list
of FINDING instances produced during linting. The operation is
idempotent: (NORMALIZE (NORMALIZE f)) yields the same text as
\(NORMALIZE f).

Composition: write-to-string → lint-string → read-from-string."
  (declare (type toplevel-form form)
           (values toplevel-form list))
  (let* ((input-string (write-toplevel-form-to-string form))
         ;; Run the lint pipeline at syntax level only.
         ;; Line-level is excluded because canonicalize operates on
         ;; single forms, not files — trailing whitespace and mixed
         ;; indentation are file-level concerns.
         (findings nil)
         (fixed-string
           (multiple-value-bind (fixed found)
               (atelier:lint-string input-string :levels '(:syntax))
             (setf findings found)
             fixed))
         ;; Re-read the fixed string to produce a new toplevel-form
         ;; with the corrected CST.
         (normalized (read-toplevel-form-from-string fixed-string)))
    (values normalized findings)))

;;;; End of file `canonicalize.lisp'
