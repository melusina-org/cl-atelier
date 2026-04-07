;;;; check-labels-for-flet.lisp — Spurious LABELS inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)


;;;;
;;;; Call-Graph Helpers
;;;;

(defun collect-calls-in-forms (forms local-set)
  "Walk a list of raw forms collecting local function names in call position.
LOCAL-SET is a hash table mapping local names to T.
A name appears in call position when it is the car of a list form or when
it is the argument to FUNCTION (i.e. the #'name syntax). QUOTE forms are
skipped. Returns a list of local names found."
  (declare (type list forms)
           (type hash-table local-set)
           (values list))
  (let ((result nil))
    (labels ((walk (form)
               (cond
                 ((atom form) nil)
                 ;; (function name) — a #'name reference; check if local
                 ((and (eq (first form) 'function)
                       (consp (rest form))
                       (symbolp (second form))
                       (gethash (second form) local-set))
                  (pushnew (second form) result :test #'eq))
                 ;; (quote ...) — do not interpret contents as calls
                 ((eq (first form) 'quote) nil)
                 ;; (name args...) — possible direct call to a local
                 (t
                  (when (and (symbolp (first form))
                             (gethash (first form) local-set))
                    (pushnew (first form) result :test #'eq))
                  ;; Walk the cons structure; handle improper lists
                  ;; (e.g. destructuring patterns like (KEY . VALUE)).
                  (loop :for tail :on form
                        :do (walk (car tail))
                        :when (and (cdr tail) (atom (cdr tail)))
                        :do (walk (cdr tail)))))))
      (dolist (form forms)
        (walk form)))
    result))

(defun labels-call-graph-has-cycles-p (raw-labels-form)
  "Return T if RAW-LABELS-FORM's local call graph contains a cycle.
RAW-LABELS-FORM is a raw (LABELS bindings . body) list.
Returns NIL when the call graph is a DAG — meaning nested FLET can express
the same structure."
  (declare (type cons raw-labels-form)
           (values boolean))
  (let* ((bindings (second raw-labels-form))
         (local-names (mapcar #'first bindings))
         (local-set (make-hash-table :test #'eq))
         (call-graph (make-hash-table :test #'eq)))
    (dolist (name local-names)
      (setf (gethash name local-set) t))
    ;; Build call graph: for each binding, record which locals its body calls.
    (dolist (binding bindings)
      (let* ((name (first binding))
             (body (cddr binding)))
        (setf (gethash name call-graph)
              (collect-calls-in-forms body local-set))))
    ;; DFS cycle detection using three-colour marking.
    (let ((color (make-hash-table :test #'eq))
          (cycle-found nil))
      (dolist (name local-names)
        (setf (gethash name color) :white))
      (labels ((dfs (node)
                 (unless cycle-found
                   (setf (gethash node color) :gray)
                   (dolist (neighbor (gethash node call-graph nil))
                     (let ((c (gethash neighbor color :white)))
                       (cond
                         ((eq c :gray) (setf cycle-found t))
                         ((eq c :white) (dfs neighbor)))))
                   (unless cycle-found
                     (setf (gethash node color) :black)))))
        (dolist (name local-names)
          (when (eq (gethash name color) :white)
            (dfs name))))
      cycle-found)))


;;;;
;;;; Inspector
;;;;

(define-syntax-inspector check-labels-for-flet (form)
  "Check that LABELS forms contain mutual or self-recursive function references.
Walk the entire top-level FORM recursively. For each LABELS node whose local
call graph has no cycles, report a SPURIOUS-LABELS-FINDING indicating that
nested FLET is the correct construct."
  (let ((findings nil))
    (labels ((walk (node)
               (when (cst:consp node)
                 (when (eq 'labels (cst:raw (cst:first node)))
                   (let ((raw (cst:raw node)))
                     (when (and (consp (cdr raw))
                                (listp (second raw))
                                ;; Guard: require at least one binding.
                                (consp (second raw))
                                (not (labels-call-graph-has-cycles-p raw)))
                       (push (make-syntax-finding-from-form
                              node 'spurious-labels-finding
                              :inspector 'check-labels-for-flet
                              :severity :style
                              :observation "LABELS form has no mutual or self-recursive calls."
                              :rationale "LABELS introduces mutual visibility among all bindings; when the local call graph is a DAG, nested FLET expresses the structure more precisely.")
                             findings))))
                 (walk (cst:first node))
                 (walk (cst:rest node)))))
      (walk form))
    (nreverse findings)))

;;;; End of file `check-labels-for-flet.lisp'
