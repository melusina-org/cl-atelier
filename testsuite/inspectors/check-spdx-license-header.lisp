;;;; check-spdx-license-header.lisp — Tests for the SPDX license header inspector

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

(define-testcase validate-check-spdx-header-present ()
  "Verify that a file with the correct SPDX header produces no findings."
  (let* ((fixture-path (merge-pathnames "valid-with-spdx.lisp"
                                        (testsuite-fixtures-directory)))
         (project-config (atelier:make-project-configuration :license "MIT"))
         (inspector-instance (atelier:find-inspector 'atelier:check-spdx-license-header))
         (findings (atelier:inspect-file inspector-instance fixture-path project-config)))
    (assert-t (null findings))))

(define-testcase validate-check-spdx-header-missing ()
  "Verify that a file without an SPDX header produces a finding."
  (let* ((fixture-path (merge-pathnames "missing-spdx.lisp"
                                        (testsuite-fixtures-directory)))
         (project-config (atelier:make-project-configuration :license "MIT"))
         (inspector-instance (atelier:find-inspector 'atelier:check-spdx-license-header))
         (findings (atelier:inspect-file inspector-instance fixture-path project-config)))
    (assert-eq 1 (length findings))
    (assert-t (typep (first findings) 'atelier:spdx-license-header-finding))
    (assert-eq :warning (atelier:finding-severity (first findings)))))

(define-testcase validate-check-spdx-header-mismatch ()
  "Verify that a file with the wrong SPDX identifier produces a finding."
  (let* ((fixture-path (merge-pathnames "wrong-spdx.lisp"
                                        (testsuite-fixtures-directory)))
         (project-config (atelier:make-project-configuration :license "MIT"))
         (inspector-instance (atelier:find-inspector 'atelier:check-spdx-license-header))
         (findings (atelier:inspect-file inspector-instance fixture-path project-config)))
    (assert-eq 1 (length findings))
    (assert-eq :warning (atelier:finding-severity (first findings)))))

(define-testcase validate-check-spdx-header-shell-script ()
  "Verify that a shell script with correct SPDX header produces no findings."
  (let* ((fixture-path (merge-pathnames "valid-shell-spdx.sh"
                                        (testsuite-fixtures-directory)))
         (project-config (atelier:make-project-configuration :license "MIT"))
         (inspector-instance (atelier:find-inspector 'atelier:check-spdx-license-header))
         (findings (atelier:inspect-file inspector-instance fixture-path project-config)))
    (assert-t (null findings))))

(define-testcase testsuite-check-spdx-license-header ()
  (validate-check-spdx-header-present)
  (validate-check-spdx-header-missing)
  (validate-check-spdx-header-mismatch)
  (validate-check-spdx-header-shell-script))

;;;; End of file `check-spdx-license-header.lisp'
