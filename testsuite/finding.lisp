;;;; finding.lisp — Tests for the finding class hierarchy

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)

(define-testcase validate-make-file-finding ()
  "Verify that MAKE-FILE-FINDING creates a FILE-FINDING with the expected slots."
  (let ((file-finding
          (atelier:make-file-finding
            :inspector :encoding-check
            :severity :warning
            :observation "File is not valid UTF-8."
            :rationale "Non-UTF-8 files cause encoding errors in CI."
            :file #p"src/example.lisp")))
    (assert-eq :encoding-check (atelier:finding-inspector file-finding))
    (assert-eq :warning (atelier:finding-severity file-finding))
    (assert-string= "File is not valid UTF-8." (atelier:finding-observation file-finding))
    (assert-string= "Non-UTF-8 files cause encoding errors in CI."
                    (atelier:finding-rationale file-finding))
    (assert-string= "src/example.lisp" (namestring (atelier:finding-file file-finding)))))

(define-testcase validate-make-line-finding ()
  "Verify that MAKE-LINE-FINDING creates a LINE-FINDING with the expected slots."
  (let ((line-finding
          (atelier:make-line-finding
            :inspector :long-line-check
            :severity :style
            :observation "Line is 142 characters long."
            :rationale "Lines longer than 100 characters reduce readability."
            :file #p"src/example.lisp"
            :line 42 :column 0 :end-line 42 :end-column 142
            :source-text "(defun very-long-function-name ...)")))
    (assert-eq 42 (atelier:finding-line line-finding))
    (assert-eq 0 (atelier:finding-column line-finding))
    (assert-eq 142 (atelier:finding-end-column line-finding))
    (assert-string= "(defun very-long-function-name ...)"
                    (atelier:finding-source-text line-finding))))

(define-testcase validate-finding-hierarchy ()
  "Verify the finding class inheritance structure."
  (let ((file-finding
          (atelier:make-file-finding
            :inspector :placeholder :severity :warning
            :observation "Observation placeholder" :rationale "Rationale placeholder"
            :file #p"example.lisp"))
        (line-finding
          (atelier:make-line-finding
            :inspector :placeholder :severity :info
            :observation "Observation placeholder" :rationale "Rationale placeholder"
            :file #p"example.lisp"
            :line 1 :column 0 :end-line 1 :end-column 10
            :source-text "source placeholder"))
        (region-finding
          (atelier:make-region-finding
            :inspector :placeholder :severity :style
            :observation "Observation placeholder" :rationale "Rationale placeholder"
            :file #p"example.lisp"
            :start-line 1 :end-line 5 :source-text "region placeholder"))
        (syntax-finding
          (atelier:make-syntax-finding
            :inspector :placeholder :severity :error
            :observation "Observation placeholder" :rationale "Rationale placeholder"
            :file #p"example.lisp"
            :line 1 :column 0 :end-line 1 :end-column 10
            :source-text "(defvar x)"
            :cst-node (make-instance 'concrete-syntax-tree:atom-cst
                         :raw 42 :source nil)
            :cst-root (make-instance 'concrete-syntax-tree:atom-cst
                         :raw 42 :source nil))))
    (assert-t (typep file-finding 'atelier:finding))
    (assert-t (typep file-finding 'atelier:file-finding))
    (assert-t (typep line-finding 'atelier:line-finding))
    (assert-t (typep line-finding 'atelier:file-finding))
    (assert-t (typep region-finding 'atelier:region-finding))
    (assert-t (typep region-finding 'atelier:file-finding))
    (assert-t (typep syntax-finding 'atelier:syntax-finding))
    (assert-t (typep syntax-finding 'atelier:line-finding))
    (assert-t (not (typep file-finding 'atelier:line-finding)))))

(define-testcase validate-syntax-finding-cst-types ()
  "Verify that SYNTAX-FINDING slots accept Eclector CST types."
  (let* ((cst-node (make-instance 'concrete-syntax-tree:atom-cst
                     :raw 42 :source nil))
         (syntax-finding
           (atelier:make-syntax-finding
             :inspector :earmuffs-check :severity :warning
             :observation "Special variable lacks earmuffs."
             :rationale "CL convention requires *name* for special variables."
             :file #p"src/example.lisp"
             :line 10 :column 0 :end-line 10 :end-column 15
             :source-text "(defvar x 42)"
             :cst-node cst-node :cst-root cst-node)))
    (assert-t (typep (atelier:finding-cst-node syntax-finding)
                     'concrete-syntax-tree:cst))
    (assert-t (typep (atelier:finding-cst-root syntax-finding)
                     'concrete-syntax-tree:cst))))

(define-testcase testsuite-finding ()
  (validate-make-file-finding)
  (validate-make-line-finding)
  (validate-finding-hierarchy)
  (validate-syntax-finding-cst-types))

;;;; End of file `finding.lisp'
