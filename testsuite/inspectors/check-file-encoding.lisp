;;;; check-file-encoding.lisp — Tests for the file encoding inspector

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

(define-testcase validate-check-file-encoding-registered ()
  "Verify that CHECK-FILE-ENCODING is registered in the inspector registry."
  (assert-t (not (null (member 'atelier:check-file-encoding
                               (atelier:list-inspectors))))))

(define-testcase validate-check-file-encoding-valid ()
  "Verify that a valid UTF-8 file produces no findings."
  (let* ((fixture-path (merge-pathnames "valid-with-spdx.lisp"
                                        (testsuite-fixtures-directory)))
         (inspector-instance (atelier:find-inspector 'atelier:check-file-encoding))
         (findings (let ((atelier:*project-configuration* nil)
                    (atelier:*linter-configuration* nil))
               (atelier:inspect-file inspector-instance fixture-path))))
    (assert-t (null findings))))

(define-testcase validate-check-file-encoding-invalid ()
  "Verify that a non-UTF-8 file produces an encoding finding."
  (let ((temporary-path (merge-pathnames "non-utf8-test.bin"
                                         (uiop:temporary-directory))))
    (unwind-protect
        (progn
          ;; Write invalid UTF-8 bytes
          (with-open-file (stream temporary-path
                                  :direction :output
                                  :if-exists :supersede
                                  :element-type '(unsigned-byte 8))
            (write-byte #xFF stream)
            (write-byte #xFE stream)
            (write-byte #x00 stream))
          (let* ((inspector-instance (atelier:find-inspector 'atelier:check-file-encoding))
                 (findings (let ((atelier:*project-configuration* nil)
                      (atelier:*linter-configuration* nil))
                 (atelier:inspect-file inspector-instance temporary-path))))
            (assert-eq 1 (length findings))
            (assert-t (typep (first findings) 'atelier:encoding-finding))
            (assert-eq :error (atelier:finding-severity (first findings)))))
      (when (probe-file temporary-path)
        (delete-file temporary-path)))))

(define-testcase testsuite-check-file-encoding ()
  (validate-check-file-encoding-registered)
  (validate-check-file-encoding-valid)
  (validate-check-file-encoding-invalid))

;;;; End of file `check-file-encoding.lisp'
