;;;; resolution.lisp — Tests for the resolution class hierarchy

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)

(define-testcase validate-make-text-resolution ()
  "Verify that MAKE-TEXT-RESOLUTION creates a TEXT-RESOLUTION with the expected slots."
  (let* ((source-finding
           (atelier:make-file-finding
             :inspector :encoding-check :severity :warning
             :observation "File is not UTF-8."
             :rationale "Encoding consistency."
             :file #p"src/example.lisp"))
         (text-resolution
           (atelier:make-text-resolution
             :maintainer :encoding-fixer
             :finding source-finding
             :kind :automatic
             :description "Replace invalid bytes with UTF-8."
             :replacement "corrected content")))
    (assert-eq :encoding-fixer (atelier:resolution-maintainer text-resolution))
    (assert-eq source-finding (atelier:resolution-finding text-resolution))
    (assert-eq :automatic (atelier:resolution-kind text-resolution))
    (assert-string= "corrected content" (atelier:resolution-replacement text-resolution))))

(define-testcase validate-make-syntax-resolution ()
  "Verify that MAKE-SYNTAX-RESOLUTION creates a SYNTAX-RESOLUTION with a transform function."
  (let* ((source-finding
           (atelier:make-file-finding
             :inspector :placeholder :severity :warning
             :observation "Observation placeholder"
             :rationale "Rationale placeholder"
             :file #p"example.lisp"))
         (syntax-resolution
           (atelier:make-syntax-resolution
             :maintainer :syntax-fixer
             :finding source-finding
             :kind :automatic
             :description "Transform CST node."
             :transform #'identity)))
    (assert-t (functionp (atelier:resolution-transform syntax-resolution)))))

(define-testcase validate-make-agent-resolution ()
  "Verify that MAKE-AGENT-RESOLUTION creates an AGENT-RESOLUTION with a prompt."
  (let* ((source-finding
           (atelier:make-file-finding
             :inspector :placeholder :severity :warning
             :observation "Observation placeholder"
             :rationale "Rationale placeholder"
             :file #p"example.lisp"))
         (agent-resolution
           (atelier:make-agent-resolution
             :maintainer :llm-fixer
             :finding source-finding
             :kind :agent
             :description "Ask LLM to fix encoding."
             :prompt "Fix the UTF-8 encoding in this file.")))
    (assert-eq :agent (atelier:resolution-kind agent-resolution))
    (assert-string= "Fix the UTF-8 encoding in this file."
                    (atelier:resolution-prompt agent-resolution))))

(define-testcase validate-make-composite-resolution ()
  "Verify that MAKE-COMPOSITE-RESOLUTION carries an ordered list of transforms."
  (let* ((source-finding
           (atelier:make-file-finding
             :inspector :placeholder :severity :warning
             :observation "Observation placeholder"
             :rationale "Rationale placeholder"
             :file #p"example.lisp"))
         (first-transform
           (atelier:make-syntax-resolution
             :maintainer :inner-fixer
             :finding source-finding
             :kind :automatic
             :description "Transform inner node."
             :transform #'identity))
         (second-transform
           (atelier:make-syntax-resolution
             :maintainer :outer-fixer
             :finding source-finding
             :kind :automatic
             :description "Transform outer node."
             :transform #'identity))
         (composite-resolution
           (atelier:make-composite-resolution
             :maintainer :composite-fixer
             :finding source-finding
             :kind :automatic
             :description "Multi-node transform."
             :transforms (list first-transform second-transform))))
    (assert-eq 2 (length (atelier:resolution-transforms composite-resolution)))
    (assert-eq first-transform (first (atelier:resolution-transforms composite-resolution)))
    (assert-eq second-transform (second (atelier:resolution-transforms composite-resolution)))))

(define-testcase testsuite-resolution ()
  (validate-make-text-resolution)
  (validate-make-syntax-resolution)
  (validate-make-agent-resolution)
  (validate-make-composite-resolution))

;;;; End of file `resolution.lisp'
