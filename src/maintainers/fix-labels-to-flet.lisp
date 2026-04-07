;;;; fix-labels-to-flet.lisp — Spurious LABELS automatic maintainer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)


;;;;
;;;; Depth Computation
;;;;

(defun compute-flet-depth-table (bindings)
  "Return a hash table mapping each local function name to its depth level.
Depth 0: calls no other local function. Depth k: calls at least one local
of depth k-1 and none of depth k or above. BINDINGS is the raw binding list
from a LABELS form. The call graph must be a DAG (no cycles)."
  (declare (type list bindings)
           (values hash-table))
  (let* ((local-names (mapcar #'first bindings))
         (local-set (make-hash-table :test #'eq))
         (call-graph (make-hash-table :test #'eq))
         (depth (make-hash-table :test #'eq)))
    (dolist (name local-names)
      (setf (gethash name local-set) t)
      (setf (gethash name depth) 0))
    (dolist (binding bindings)
      (setf (gethash (first binding) call-graph)
            (collect-calls-in-forms (cddr binding) local-set)))
    ;; Bellman-Ford relaxation: N passes ensure convergence on a DAG.
    (dotimes (_ (length local-names))
      (dolist (caller local-names)
        (dolist (callee (gethash caller call-graph nil))
          ;; caller depends on callee, so depth(caller) >= depth(callee) + 1
          (let ((new-depth (1+ (gethash callee depth 0))))
            (when (> new-depth (gethash caller depth 0))
              (setf (gethash caller depth) new-depth))))))
    depth))


;;;;
;;;; Transform
;;;;

(defun labels-transform-to-flet (raw-labels-form)
  "Transform RAW-LABELS-FORM from LABELS to nested FLET groups.
Groups local functions by depth level, with depth-0 (no local calls)
outermost and higher depths nested inward. The outer body is placed at
the innermost level. Returns the transformed form."
  (declare (type cons raw-labels-form)
           (values cons))
  (let* ((bindings (second raw-labels-form))
         (outer-body (cddr raw-labels-form))
         (depth-table (compute-flet-depth-table bindings))
         (max-depth (loop :for name :being :the :hash-key :of depth-table
                          :maximize (gethash name depth-table 0)))
         ;; groups is an array indexed by depth; each slot is a list of bindings.
         (groups (make-array (1+ max-depth) :initial-element nil)))
    ;; Distribute bindings into groups, preserving original order within each level.
    (dolist (binding (reverse bindings))
      (let ((d (gethash (first binding) depth-table 0)))
        (push binding (aref groups d))))
    ;; Build nested FLET from the inside out: start with the outer body,
    ;; wrap with the deepest group first, then progressively shallower.
    (let ((current-body outer-body))
      (loop :for d :from max-depth :downto 0
            :for group = (aref groups d)
            :when group
            :do (setf current-body (list `(flet ,group ,@current-body))))
      (if (= (length current-body) 1)
          (first current-body)
          `(progn ,@current-body)))))


;;;;
;;;; Maintainer
;;;;

(define-automatic-maintainer fix-labels-to-flet
    ((finding spurious-labels-finding))
  "Rewrite a spurious LABELS form as nested FLET groups.
Groups local functions by call-graph depth: depth-0 (no calls to other locals)
forms the outermost FLET; each deeper group nests inward. Returns a
SYNTAX-RESOLUTION using LABELS-TRANSFORM-TO-FLET."
  (make-syntax-resolution
   :maintainer 'fix-labels-to-flet
   :finding finding
   :kind :automatic
   :description "Rewrite LABELS with acyclic call graph as nested FLET."
   :transform #'labels-transform-to-flet))

;;;; End of file `fix-labels-to-flet.lisp'
