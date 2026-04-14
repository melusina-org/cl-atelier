;;;; introspection.lisp — Introspection helpers for the child worker

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/child-worker)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (require :sb-introspect))

;;; These functions run in the child SBCL image. The parent calls them
;;; via SWANK eval and reads back the printed alist results.

(defun %symbol-kind (symbol)
  "Classify SYMBOL into a kind keyword string."
  (cond
    ((special-operator-p symbol) "SPECIAL-FORM")
    ((macro-function symbol) "MACRO")
    ((and (fboundp symbol)
          (typep (fdefinition symbol) 'generic-function))
     "GENERIC-FUNCTION")
    ((fboundp symbol) "FUNCTION")
    ((find-class symbol nil)
     (if (subtypep (find-class symbol) (find-class 'condition))
         "CONDITION"
         "CLASS"))
    ((constantp symbol) "CONSTANT")
    ((boundp symbol) "VARIABLE")
    (t "UNBOUND")))

(defun %symbol-documentation (symbol)
  "Return the best available documentation string for SYMBOL."
  (or (documentation symbol 'function)
      (documentation symbol 'variable)
      (documentation symbol 'type)
      ""))

(defun %truncate-string (string max-length)
  "Truncate STRING to MAX-LENGTH characters."
  (if (> (length string) max-length)
      (concatenate 'string (subseq string 0 max-length) "...")
      string))


;;; ---- Public API ----

(defun list-packages-data ()
  "Return a list of alists describing all packages in the image."
  (let ((result nil))
    (dolist (pkg (list-all-packages) (nreverse result))
      (let ((ext-count 0)
            (int-count 0))
        (do-external-symbols (s pkg)
          (declare (ignore s))
          (incf ext-count))
        (do-symbols (s pkg)
          (when (eq (symbol-package s) pkg)
            (incf int-count)))
        ;; internal-count includes externals; subtract
        (decf int-count ext-count)
        (push (list (cons "name" (package-name pkg))
                    (cons "nicknames"
                          (mapcar #'identity (package-nicknames pkg)))
                    (cons "use-list"
                          (mapcar #'package-name (package-use-list pkg)))
                    (cons "used-by-list"
                          (mapcar #'package-name (package-used-by-list pkg)))
                    (cons "external-count" ext-count)
                    (cons "internal-count" int-count))
              result)))))

(defun list-package-symbols-data (package-name &key (status :external))
  "Return a list of alists describing symbols in PACKAGE-NAME.
   STATUS is one of :EXTERNAL, :INTERNAL, :ANY (default :EXTERNAL)."
  (let ((pkg (find-package package-name))
        (result nil))
    (unless pkg
      (error "Package ~S not found." package-name))
    (flet ((collect-symbol (sym)
             (push (list (cons "name" (symbol-name sym))
                         (cons "package" (package-name
                                         (or (symbol-package sym) pkg)))
                         (cons "status" (string status))
                         (cons "home-package"
                               (if (symbol-package sym)
                                   (package-name (symbol-package sym))
                                   ""))
                         (cons "kind" (%symbol-kind sym))
                         (cons "documentation"
                               (%truncate-string
                                (%symbol-documentation sym) 200)))
                   result)))
      (ecase status
        (:external (do-external-symbols (s pkg) (collect-symbol s)))
        (:internal (do-symbols (s pkg)
                     (when (eq (symbol-package s) pkg)
                       (collect-symbol s))))
        (:any (do-symbols (s pkg) (collect-symbol s)))))
    (sort result #'string< :key (lambda (alist) (cdr (assoc "name" alist :test #'string=))))))

(defun %parse-symbol-designator (designator)
  "Parse DESIGNATOR (a string like \"cl:car\") into a symbol.
   Signals an error if the symbol cannot be found."
  (let ((sym (let ((*package* (find-package :cl-user)))
               (read-from-string designator))))
    (unless (symbolp sym)
      (error "~S does not designate a symbol." designator))
    sym))

(defun describe-symbol-data (designator)
  "Return an alist describing the symbol named by DESIGNATOR."
  (let* ((sym (%parse-symbol-designator designator))
         (result (list (cons "name" (symbol-name sym))
                       (cons "package" (if (symbol-package sym)
                                          (package-name (symbol-package sym))
                                          ""))
                       (cons "kind" (%symbol-kind sym))
                       (cons "documentation" (%symbol-documentation sym)))))
    ;; Function-specific info
    (when (fboundp sym)
      (let ((fn (fdefinition sym)))
        (push (cons "lambda-list"
                    (princ-to-string
                     (or #+sbcl (sb-introspect:function-lambda-list fn)
                         nil)))
              result)
        #+sbcl
        (let ((type (sb-introspect:function-type fn)))
          (when type
            (push (cons "type" (princ-to-string type)) result)))))
    ;; Class-specific info
    (let ((class (find-class sym nil)))
      (when class
        (closer-mop:ensure-finalized class)
        (push (cons "slots"
                    (mapcar (lambda (slot)
                              (symbol-name (closer-mop:slot-definition-name slot)))
                            (closer-mop:class-slots class)))
              result)))
    ;; Generic-function-specific info
    (when (and (fboundp sym)
               (typep (fdefinition sym) 'generic-function))
      (let ((gf (fdefinition sym)))
        (push (cons "methods"
                    (mapcar (lambda (method)
                              (format nil "~{~A~^ ~}"
                                      (mapcar #'princ-to-string
                                              (closer-mop:method-specializers method))))
                            (closer-mop:generic-function-methods gf)))
              result)))
    (nreverse result)))

(defun find-definition-data (designator)
  "Return an alist with source location for DESIGNATOR, or NIL."
  #+sbcl
  (let* ((sym (%parse-symbol-designator designator))
         (sources (sb-introspect:find-definition-sources-by-name sym :function)))
    (when sources
      (let ((source (first sources)))
        (let ((pathname (sb-introspect:definition-source-pathname source)))
          (when pathname
            (list (cons "source-file" (namestring pathname))
                  (cons "form-path"
                        (princ-to-string
                         (sb-introspect:definition-source-form-path source)))))))))
  #-sbcl
  nil)

(defun run-testsuite-data (system-name)
  "Load SYSTEM-NAME, run its test system, and return a result alist."
  (let ((output (make-string-output-stream))
        (err-output (make-string-output-stream))
        (start (get-internal-real-time))
        (success nil))
    (handler-case
        (progn
          (let ((*standard-output* (make-broadcast-stream *standard-output* output))
                (*error-output* (make-broadcast-stream *error-output* err-output)))
            (asdf:load-system system-name)
            (asdf:test-system system-name))
          (setf success t))
      (error (c)
        (format err-output "~&Error: ~A~%" c)))
    (let ((duration-ms (round (* 1000 (- (get-internal-real-time) start))
                              internal-time-units-per-second)))
      (list (cons "system" (string system-name))
            (cons "success" success)
            (cons "duration-ms" duration-ms)
            (cons "output" (%truncate-string
                            (get-output-stream-string output) 10000))
            (cons "error-output" (%truncate-string
                                  (get-output-stream-string err-output) 5000))))))


;;; ---- Slice 012: ASDF, Quicklisp, Confidence ----

(defun quickload-data (system-name)
  "Load SYSTEM-NAME via Quicklisp (or ASDF if QL unavailable).
   Returns an alist with system, success, output, duration-ms."
  (let ((output (make-string-output-stream))
        (start (get-internal-real-time))
        (success nil))
    (handler-case
        (let ((*standard-output* (make-broadcast-stream *standard-output* output))
              (*error-output* (make-broadcast-stream *error-output* output)))
          (if (find-package :ql)
              (funcall (find-symbol "QUICKLOAD" :ql) system-name :silent t)
              (asdf:load-system system-name))
          (setf success t))
      (error (c)
        (format output "~&Error: ~A~%" c)))
    (let ((duration-ms (round (* 1000 (- (get-internal-real-time) start))
                              internal-time-units-per-second)))
      (list (cons "system" (string system-name))
            (cons "success" success)
            (cons "output" (%truncate-string
                            (get-output-stream-string output) 10000))
            (cons "duration-ms" duration-ms)))))

(defun system-info-data (system-name)
  "Return an alist describing ASDF system SYSTEM-NAME."
  (let ((system (asdf:find-system system-name nil)))
    (unless system
      (error "System ~S not found." system-name))
    (list (cons "name" (asdf:component-name system))
          (cons "version" (or (asdf:component-version system) ""))
          (cons "author" (or (asdf:system-author system) ""))
          (cons "license" (or (asdf:system-license system) ""))
          (cons "description" (or (asdf:system-description system) ""))
          (cons "source-directory"
                (let ((dir (asdf:system-source-directory system)))
                  (if dir (namestring dir) "")))
          (cons "depends-on"
                (mapcar (lambda (dep)
                          (if (stringp dep) dep
                              (princ-to-string dep)))
                        (asdf:system-depends-on system)))
          (cons "components"
                (mapcar (lambda (comp)
                          (list (cons "name" (asdf:component-name comp))
                                (cons "type" (string-downcase
                                              (symbol-name
                                               (type-of comp))))))
                        (asdf:component-children system))))))

(defun system-apropos-data (search-string)
  "Return system names from the source registry matching SEARCH-STRING."
  (asdf/source-registry:ensure-source-registry)
  (let ((registry (symbol-value
                   (find-symbol "*SOURCE-REGISTRY*" :asdf/source-registry)))
        (result nil)
        (search-down (string-downcase search-string)))
    (when (hash-table-p registry)
      (loop :for name :being :the :hash-keys :of registry
            :do (when (search search-down (string-downcase name))
                  (push name result))))
    (sort result #'string<)))

(defun list-testcases-data (package-name)
  "Return a list of Confidence testcases defined in PACKAGE-NAME.
   Each testcase is an alist with name and documentation."
  (let ((pkg (find-package package-name))
        (result nil)
        (testcase-key nil))
    (unless pkg
      (error "Package ~S not found." package-name))
    ;; Confidence uses :ORG.MELUSINA.CONFIDENCE/TESTCASE as the property key
    (setf testcase-key :org.melusina.confidence/testcase)
    (unless (find-package :confidence)
      (return-from list-testcases-data nil))
    ;; Scan symbols for the testcase property
    (do-symbols (sym pkg)
      (when (and (eq (symbol-package sym) pkg)
                 (get sym testcase-key))
        (push (list (cons "name" (format nil "~A:~A"
                                         (package-name pkg)
                                         (symbol-name sym)))
                    (cons "documentation"
                          (%truncate-string
                           (or (documentation sym 'function) "") 200)))
              result)))
    (sort result #'string<
          :key (lambda (alist) (cdr (assoc "name" alist :test #'string=))))))

(defun run-testcase-data (testcase-designator)
  "Run a Confidence testcase by designator (e.g. \"ATELIER/TESTSUITE:RUN-ALL-TESTS\").
   Returns an alist with name, total, success, failure, condition, outcome."
  (let* ((sym (let ((*package* (find-package :cl-user)))
                (read-from-string testcase-designator)))
         (output (make-string-output-stream)))
    (unless (and (symbolp sym) (fboundp sym))
      (error "~S is not a callable testcase." testcase-designator))
    (let ((*standard-output* (make-broadcast-stream *standard-output* output)))
      (funcall sym))
    (list (cons "name" testcase-designator)
          (cons "output" (%truncate-string
                          (get-output-stream-string output) 10000)))))



;;; ---- Slice 014: xref, CLOS inspector, trace ----

(defun %xref-results-to-alist (xref-entries)
  "Convert a list of (NAME . DEFINITION-SOURCE) conses returned by
   sb-introspect:who-* to alists. Each entry has the caller name,
   source file, and form path. Filters out entries with nil pathnames."
  #+sbcl
  (let ((result nil))
    (dolist (entry xref-entries (nreverse result))
      (let* ((name (car entry))
             (source (cdr entry))
             (pathname (sb-introspect:definition-source-pathname source)))
        (when pathname
          (push (list (cons "name" (princ-to-string name))
                      (cons "source-file" (namestring pathname))
                      (cons "form-path"
                            (princ-to-string
                             (sb-introspect:definition-source-form-path source))))
                result)))))
  #-sbcl
  (declare (ignore xref-entries))
  #-sbcl
  nil)

(defun who-calls-data (designator)
  "Return a list of alists describing callers of the function named by DESIGNATOR."
  #+sbcl
  (let ((sym (%parse-symbol-designator designator)))
    (%xref-results-to-alist (sb-introspect:who-calls sym)))
  #-sbcl
  (declare (ignore designator))
  #-sbcl
  nil)

(defun who-references-data (designator)
  "Return a list of alists describing functions that reference the variable
   named by DESIGNATOR."
  #+sbcl
  (let ((sym (%parse-symbol-designator designator)))
    (%xref-results-to-alist (sb-introspect:who-references sym)))
  #-sbcl
  (declare (ignore designator))
  #-sbcl
  nil)

(defun who-binds-data (designator)
  "Return a list of alists describing functions that bind the variable
   named by DESIGNATOR."
  #+sbcl
  (let ((sym (%parse-symbol-designator designator)))
    (%xref-results-to-alist (sb-introspect:who-binds sym)))
  #-sbcl
  (declare (ignore designator))
  #-sbcl
  nil)

(defun who-specializes-data (designator)
  "Return a list of alists describing methods that specialize on the class
   named by DESIGNATOR."
  #+sbcl
  (let* ((sym (%parse-symbol-designator designator))
         (class (find-class sym nil)))
    (if class
        (let ((result nil))
          (dolist (method (closer-mop:specializer-direct-generic-functions class)
                   (nreverse result))
            (dolist (m (closer-mop:generic-function-methods method))
              (when (member class (closer-mop:method-specializers m))
                (push (list (cons "generic-function"
                                  (let ((name (closer-mop:generic-function-name method)))
                                    (if (symbolp name)
                                        (format nil "~A:~A"
                                                (package-name (symbol-package name))
                                                (symbol-name name))
                                        (princ-to-string name))))
                            (cons "qualifiers"
                                  (mapcar #'princ-to-string
                                          (method-qualifiers m)))
                            (cons "specializers"
                                  (mapcar #'princ-to-string
                                          (closer-mop:method-specializers m))))
                      result)))))
        nil))
  #-sbcl
  (declare (ignore designator))
  #-sbcl
  nil)

(defun who-macroexpands-data (designator)
  "Return a list of alists describing functions that expand the macro
   named by DESIGNATOR."
  #+sbcl
  (let ((sym (%parse-symbol-designator designator)))
    (%xref-results-to-alist (sb-introspect:who-macroexpands sym)))
  #-sbcl
  (declare (ignore designator))
  #-sbcl
  nil)

(defun inspect-class-data (designator)
  "Return a detailed alist describing the CLOS class named by DESIGNATOR.
   Includes superclasses, subclasses, slots with readers/writers/initargs,
   and direct methods."
  (let* ((sym (%parse-symbol-designator designator))
         (class (find-class sym nil)))
    (unless class
      (error "~S does not name a class." designator))
    (closer-mop:ensure-finalized class)
    (flet ((class-name-string (c)
             (let ((name (class-name c)))
               (if (and name (symbolp name))
                   (format nil "~A:~A"
                           (package-name (symbol-package name))
                           (symbol-name name))
                   (princ-to-string c))))
           (slot-to-alist (slot)
             (list (cons "name" (symbol-name
                                 (closer-mop:slot-definition-name slot)))
                   (cons "readers"
                         (mapcar #'princ-to-string
                                 (closer-mop:slot-definition-readers slot)))
                   (cons "writers"
                         (mapcar #'princ-to-string
                                 (closer-mop:slot-definition-writers slot)))
                   (cons "initargs"
                         (mapcar #'princ-to-string
                                 (closer-mop:slot-definition-initargs slot)))
                   (cons "type"
                         (princ-to-string
                          (closer-mop:slot-definition-type slot)))
                   (cons "initform"
                         (if (closer-mop:slot-definition-initfunction slot)
                             (princ-to-string
                              (closer-mop:slot-definition-initform slot))
                             "")))))
      (list (cons "name" (class-name-string class))
            (cons "superclasses"
                  (mapcar #'class-name-string
                          (closer-mop:class-direct-superclasses class)))
            (cons "subclasses"
                  (let ((result nil))
                    (dolist (sub (closer-mop:class-direct-subclasses class)
                             (nreverse result))
                      (let ((name (class-name sub)))
                        (when (and name (symbolp name))
                          (push (class-name-string sub) result))))))
            (cons "slots"
                  (mapcar #'slot-to-alist
                          (closer-mop:class-direct-slots class)))
            (cons "methods"
                  (let ((result nil))
                    (dolist (gf (closer-mop:specializer-direct-generic-functions
                                 class)
                             (nreverse result))
                      (dolist (m (closer-mop:generic-function-methods gf))
                        (when (member class
                                      (closer-mop:method-specializers m))
                          (push (list (cons "generic-function"
                                            (princ-to-string
                                             (closer-mop:generic-function-name gf)))
                                      (cons "qualifiers"
                                            (mapcar #'princ-to-string
                                                    (method-qualifiers m)))
                                      (cons "specializers"
                                            (mapcar #'princ-to-string
                                                    (closer-mop:method-specializers m))))
                                result))))))))))

(defun trace-function-data (designator)
  "Trace the function named by DESIGNATOR. Returns an alist confirming
   the function is now traced. Uses EVAL because TRACE is a macro."
  (let ((sym (%parse-symbol-designator designator)))
    (unless (fboundp sym)
      (error "~S is not a defined function." designator))
    (eval `(trace ,sym))
    (list (cons "symbol" designator)
          (cons "traced" t))))

(defun untrace-function-data (designator)
  "Remove tracing from the function named by DESIGNATOR. Returns an
   alist confirming tracing was removed. Uses EVAL because UNTRACE
   is a macro."
  (let ((sym (%parse-symbol-designator designator)))
    (eval `(untrace ,sym))
    (list (cons "symbol" designator)
          (cons "traced" nil))))

(defun who-tests-data (designator)
  "Return a list of Confidence testcase names whose functions directly
   call the function named by DESIGNATOR. Uses sb-introspect:who-calls
   to find callers, then checks which callers carry the Confidence
   testcase property."
  #+sbcl
  (let* ((sym (%parse-symbol-designator designator))
         (callers (sb-introspect:who-calls sym))
         (testcase-key :org.melusina.confidence/testcase)
         (matched nil))
    (unless (find-package :confidence)
      (return-from who-tests-data nil))
    ;; who-calls returns (NAME . DEFINITION-SOURCE) conses.
    ;; Check if NAME is a symbol with the testcase property.
    (dolist (entry callers)
      (let ((name (car entry)))
        (when (and (symbolp name)
                   (get name testcase-key))
          (push (format nil "~A:~A"
                        (package-name (symbol-package name))
                        (symbol-name name))
                matched))))
    (sort (remove-duplicates matched :test #'string=) #'string<))
  #-sbcl
  (declare (ignore designator))
  #-sbcl
  nil)

(defun run-impacted-data (designator)
  "Discover testcases impacted by the function named by DESIGNATOR
   via WHO-TESTS-DATA, run each, and return per-testcase results."
  (let ((testcase-names (who-tests-data designator)))
    (if (null testcase-names)
        (list (cons "function" designator)
              (cons "testcases" nil)
              (cons "message" "No impacted testcases found."))
        (let ((results nil))
          (dolist (tc-name testcase-names)
            (handler-case
                (let ((tc-result (run-testcase-data tc-name)))
                  (push (list (cons "name" tc-name)
                              (cons "status" "passed")
                              (cons "output"
                                    (cdr (assoc "output" tc-result
                                                :test #'string=))))
                        results))
              (error (c)
                (push (list (cons "name" tc-name)
                            (cons "status" "failed")
                            (cons "error" (princ-to-string c)))
                      results))))
          (list (cons "function" designator)
                (cons "testcases" (nreverse results))
                (cons "total" (length testcase-names))
                (cons "passed" (count "passed" results
                                      :key (lambda (r)
                                             (cdr (assoc "status" r
                                                         :test #'string=)))
                                      :test #'string=))
                (cons "failed" (count "failed" results
                                      :key (lambda (r)
                                             (cdr (assoc "status" r
                                                         :test #'string=)))
                                      :test #'string=)))))))

;;;; End of file `introspection.lisp'
