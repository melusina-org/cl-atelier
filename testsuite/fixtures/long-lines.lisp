;;;; long-lines.lisp — Fixture with various line lengths

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

;;;; SPDX-License-Identifier: MIT

(in-package #:cl-user)

;; This is a normal short line.

;; This line is intentionally very long to trigger the line length inspector because it contains many words that push it well beyond the one hundred character limit that is configured by default.

(defun this-is-a-very-long-definition-name-that-exceeds-one-hundred-characters-but-should-be-skipped-by-the-inspector ()
  "A function whose definition line is long but should be skipped."
  nil)

;; Supercalifragilisticexpialidociousantidisestablishmentarianismpneumonoultramicroscopicsilicovolcanoconiosisfloccinaucinihilipilification

;;;; End of file `long-lines.lisp'
