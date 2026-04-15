# Bug 1

The macro definition triggers a `SINGLE-FORM-PROGN-FINDING` but the
form in the `PROGN` has a *splat* unquote and should not be reported
by the inspector.

```lisp
(defmacro define-findings (&rest specs)
  "Define multiple concrete finding subclasses.
Each SPEC is (PARENT NAME DOCUMENTATION), ordered so that lines sort
meaningfully by parent class."
  `(progn
     ,@(flet ((map-item (spec)
                (destructuring-bind
                    (parent name documentation)
                    spec
                  (eclector.reader:quasiquote
                   (define-finding (eclector.reader:unquote name)
                    (eclector.reader:unquote parent)
                    (eclector.reader:unquote documentation))))))
         (mapcar #'map-item specs))))
```

Another manifestation of this bug is

```lisp
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
```

# Bug 2

The `WHEN-NOT-FINDING` seems to be autofix-able by replacing the
`(WHEN (NOT EXPR))` by `(UNLESS EXPR)`.


# Bug 3

The `SINGLE-BRANCH-IF-FINDING` seems to be autofix-able by replacing
the `IF` with a `WHEN` or a `UNLESS` depending on the branch.


# Bug 4

When we describe a finding instance, we shoud see if the finding is
autofix-able or not.  Work with me to decide how to materialize the
relationship between findings and maintainers --- or remind me how it
is materialized currently.
