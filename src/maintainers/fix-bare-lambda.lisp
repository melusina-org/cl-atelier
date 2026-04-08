;;;; fix-bare-lambda.lisp — Bare lambda automatic maintainer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)


;;;;
;;;; FLET Name Generation
;;;;

(defun generate-flet-name (operator lambda-list body)
  "Generate a descriptive FLET function name for a lambda extracted from
a call to OPERATOR. The name is derived from the OPERATOR name and
LAMBDA-LIST structure. BODY is included for future heuristics.
Returns a symbol in the current package."
  (declare (type symbol operator)
           (type list lambda-list body)
           (values symbol))
  (declare (ignore lambda-list body))
  (flet ((operator-verb (op)
           ;; Extract the primary verb from the operator name.
           ;; map* → map, remove-* → remove, find-* → find, sort/stable-sort → compare
           (let ((name (symbol-name op)))
             (cond ((string= (subseq name 0 (min 3 (length name))) "MAP") "map")
                   ((string-prefix-p "REMOVE" name)                       "remove")
                   ((string-prefix-p "FIND"   name)                       "find")
                   ((string-prefix-p "COUNT"  name)                       "count")
                   ((string-prefix-p "SORT"   name)                       "compare")
                   ((string= name "STABLE-SORT")                          "compare")
                   ((string-prefix-p "EVERY"  name)                       "check")
                   ((string-prefix-p "SOME"   name)                       "check")
                   ((string-prefix-p "NOTANY" name)                       "check")
                   ((string-prefix-p "NOTEVERY" name)                     "check")
                   (t (string-downcase name))))))
    (intern (string-upcase (format nil "~A-ITEM" (operator-verb operator))))))


;;;;
;;;; Enclosing-Call Finder
;;;;

(defun find-enclosing-call (cst-root lambda-source)
  "Walk CST-ROOT to find the innermost cons node whose first argument
has source position LAMBDA-SOURCE. Returns the call node or NIL."
  (declare (type cons lambda-source)
           (values (or concrete-syntax-tree:cst null)))
  (let ((result nil))
    (labels ((walk (node)
               (when (and (not result) (concrete-syntax-tree:consp node))
                 ;; Check whether this node's first argument is the lambda.
                 (let ((args (concrete-syntax-tree:rest node)))
                   (when (concrete-syntax-tree:consp args)
                     (let ((first-arg (concrete-syntax-tree:first args)))
                       (when (equal (concrete-syntax-tree:source first-arg) lambda-source)
                         (setf result node)))))
                 ;; Recurse into both branches unless already found.
                 (unless result
                   (walk (concrete-syntax-tree:first node)))
                 (unless result
                   (walk (concrete-syntax-tree:rest node))))))
      ;; *current-cst-root* holds a plain list of all top-level CST forms;
      ;; handle both that case and a single CST node (as in tests).
      (if (listp cst-root)
          (dolist (form cst-root)
            (unless result (walk form)))
          (walk cst-root)))
    result))


;;;;
;;;; Maintainer
;;;;

(define-automatic-maintainer fix-bare-lambda
    ((finding bare-lambda-finding))
  "Extract a bare LAMBDA in a higher-order function call to a named FLET function.
Walks the CST root to find the enclosing call and returns a SYNTAX-RESOLUTION
whose CST-NODE is the enclosing call (not the lambda itself). The transform
builds a FLET-wrapped form from the call's raw form."
  (let* ((lambda-cst (finding-cst-node finding))
         (lambda-source (concrete-syntax-tree:source lambda-cst))
         (call-cst (find-enclosing-call (finding-cst-root finding) lambda-source)))
    (when call-cst
      (let* ((call-form (concrete-syntax-tree:raw call-cst))
             (operator (first call-form))
             (lambda-form (second call-form))
             (flet-name (generate-flet-name operator (second lambda-form) (cddr lambda-form))))
        (make-syntax-resolution
         :maintainer 'fix-bare-lambda
         :finding finding
         :kind :automatic
         :description (format nil "Extract lambda to FLET ~S." flet-name)
         :cst-node call-cst
         :transform (flet ((build-flet-form (raw-call)
                             (let* ((operator (first raw-call))
                                    (lambda-form (second raw-call))
                                    (rest-args (cddr raw-call))
                                    (params (second lambda-form))
                                    (body (cddr lambda-form))
                                    (name (generate-flet-name operator params body)))
                               `(flet ((,name ,params ,@body))
                                  (,operator (function ,name) ,@rest-args)))))
                     #'build-flet-form))))))

;;;; End of file `fix-bare-lambda.lisp'
