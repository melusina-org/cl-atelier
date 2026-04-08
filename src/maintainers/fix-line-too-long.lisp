;;;; fix-line-too-long.lisp — Line-too-long automatic maintainer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)

(define-automatic-maintainer fix-line-too-long
    ((finding line-too-long-finding))
  "Shorten a line that exceeds the maximum length.
Strategies depend on line content: long strings are broken with
compile-time concatenation, long parameter lists and conditions are
reflowed, long code lines are restructured with LET bindings."
  (:maturity :experimental)
  ;; No-op for now — returns NIL (no resolution produced).
  nil)

;;;; End of file `fix-line-too-long.lisp'
