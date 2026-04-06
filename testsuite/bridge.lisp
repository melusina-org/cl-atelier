;;;; bridge.lisp — Tests for the legacy-to-finding bridge

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

(define-testcase validate-hint-at-file-to-finding ()
  "Verify that HINT-TO-FINDING converts HINT-AT-FILE to FILE-FINDING."
  (let* ((legacy-hint
           (make-instance 'atelier/legacy:hint-at-file
             :code :codestyle-0001
             :pathname #p"src/example.lisp"
             :description "File is not valid UTF-8."
             :explanation "Encoding error details."))
         (converted-finding
           (atelier/legacy:hint-to-finding legacy-hint)))
    (assert-t (typep converted-finding 'atelier:file-finding))
    (assert-eq :codestyle-0001 (atelier:finding-inspector converted-finding))
    (assert-eq :warning (atelier:finding-severity converted-finding))
    (assert-string= "File is not valid UTF-8."
                    (atelier:finding-observation converted-finding))
    (assert-string= "Encoding error details."
                    (atelier:finding-rationale converted-finding))))

(define-testcase validate-hint-at-file-line-to-finding ()
  "Verify that HINT-TO-FINDING converts HINT-AT-FILE-LINE to LINE-FINDING."
  (let* ((legacy-hint
           (make-instance 'atelier/legacy:hint-at-file-line
             :code :codestyle-0002
             :pathname #p"src/example.lisp"
             :line 42
             :description "Line is very long."
             :explanation "Over 100 characters."))
         (converted-finding
           (atelier/legacy:hint-to-finding legacy-hint)))
    (assert-t (typep converted-finding 'atelier:line-finding))
    (assert-eq :codestyle-0002 (atelier:finding-inspector converted-finding))
    (assert-eq 42 (atelier:finding-line converted-finding))
    (assert-string= "Line is very long."
                    (atelier:finding-observation converted-finding))))

(define-testcase testsuite-bridge ()
  (validate-hint-at-file-to-finding)
  (validate-hint-at-file-line-to-finding))

;;;; End of file `bridge.lisp'
