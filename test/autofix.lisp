;;;; autofix.lisp — Testsuite for LINT :action :fix integration

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/test)


;;;;
;;;; Autofix pipeline helpers
;;;;

(defun apply-autofix-to-file (pathname)
  "Run inspection on PATHNAME, resolve each finding using only production
maintainers (those in the ATELIER package), and apply all resolutions.
Returns the list of findings produced."
  (let* ((atelier::*maintainers*
           (let ((clean (make-hash-table :test 'eq)))
             (maphash (lambda (name m)
                        (unless (eq (symbol-package name)
                                    (find-package :atelier/test))
                          (setf (gethash name clean) m)))
                      atelier::*maintainers*)
             clean))
         (findings (atelier:perform-inspection pathname))
         (resolutions-by-file (make-hash-table :test 'equal)))
    (dolist (finding findings)
      (when (typep finding 'atelier:line-finding)
        (dolist (resolution (atelier:resolve-finding finding))
          (push resolution
                (gethash (atelier:finding-file finding) resolutions-by-file)))))
    (maphash (lambda (p resolutions)
               (atelier:apply-resolutions-to-file p (nreverse resolutions)))
             resolutions-by-file)
    findings))


;;;;
;;;; Slow tests — file-based autofix integration
;;;;

(define-testcase validate-lint-fix ()
  "Verify that the fix pipeline rewrites source files.
Asserts that trailing whitespace on a code line is removed. Does not
assert exact file content because other maintainers (e.g. SPDX header)
may also modify the file."
  (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (format s "(defvar *x* 1)   ~%"))
    (let ((findings (apply-autofix-to-file p)))
      (assert-t (not (null findings)))
      ;; The trailing whitespace must be gone.
      (let ((content (uiop:read-file-string p :external-format :utf-8)))
        (assert-t (not (null (search "(defvar *x* 1)" content))))
        (assert-nil (search "(defvar *x* 1)   " content))))))

(define-testcase validate-lint-inspect ()
  "Verify that :ACTION :INSPECT does not modify source files."
  (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
    (let ((original (format nil "(defvar *x* 1)   ~%")))
      (with-open-file (s p :direction :output :if-exists :supersede
                         :external-format :utf-8)
        (write-string original s))
      (atelier:perform-inspection p)
      (assert-string= original
                      (uiop:read-file-string p :external-format :utf-8)))))

(define-testcase validate-lint-partial-fix ()
  "Verify that only findings with registered maintainers are fixed."
  (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (format s "(defun ~A () nil)   ~%"
              (make-string 100 :initial-element #\a)))
    (let* ((findings-before (apply-autofix-to-file p))
           (content-after (uiop:read-file-string p :external-format :utf-8)))
      (assert-t (>= (length findings-before) 2))
      (let ((newline-pos (position #\Newline content-after)))
        (assert-t (and newline-pos
                       (not (char= #\Space (char content-after (1- newline-pos))))))))))


;;;;
;;;; Fast tests — fixture auto-discovery for inspectors
;;;;

(define-testcase validate-one-inspector-fixture (inspector-name
                                                 &optional (fixture-name "baseline"))
  "Validate a single inspector fixture. INSPECTOR-NAME is a symbol like
ATELIER:CHECK-EARMUFFS. FIXTURE-NAME defaults to \"baseline\".
Files named \"good\" or \"clean\" should produce no findings.
Files named \"bad\", \"violation\", or anything else should produce findings.

Example: (validate-one-inspector-fixture 'atelier:check-earmuffs \"bad\")"
  (let ((fixture-path (inspector-fixture inspector-name fixture-name)))
    (unless (probe-file fixture-path)
      (format t "~&SKIP ~A/~A: ~A not found.~%" inspector-name fixture-name fixture-path)
      (return-from validate-one-inspector-fixture nil))
    (let* ((inspector (atelier:find-inspector inspector-name))
           (findings (when inspector (atelier:inspect-file inspector fixture-path)))
           (expect-clean (member fixture-name '("good" "clean" "correct" "necessary")
                                 :test #'string-equal)))
      (if expect-clean
          (assert-t (null findings))
          (assert-t (not (null findings)))))))

(define-testcase validate-inspector-fixtures ()
  "Run every discovered inspector fixture."
  (dolist (entry (discover-inspector-fixtures))
    (let ((inspector-name (car entry)))
      (dolist (fixture-path (cdr entry))
        (let ((fixture-name (pathname-name fixture-path)))
          (validate-one-inspector-fixture inspector-name fixture-name))))))


;;;;
;;;; Fast tests — fixture auto-discovery for pretty-printer
;;;;

(define-testcase validate-one-pretty-printer-fixture (fixture-name)
  "Validate a single pretty-printer fixture. FIXTURE-NAME is a string like
\"flet-single-binding\".

Example: (validate-one-pretty-printer-fixture \"flet-single-binding\")"
  (let ((fixture-path (pretty-printer-fixture fixture-name)))
    (unless (probe-file fixture-path)
      (format t "~&SKIP pretty-printer/~A: ~A not found.~%" fixture-name fixture-path)
      (return-from validate-one-pretty-printer-fixture nil))
    (multiple-value-bind (front-matter form column right-margins expected-strings)
        (read-pretty-print-fixture fixture-path)
      (declare (ignore front-matter))
      (loop :for right-margin :in right-margins
            :for expected :in expected-strings
            :for actual = (atelier:pretty-print-form form column
                                                     :right-margin right-margin)
            :do (assert-string= expected actual)))))

(define-testcase validate-pretty-printer-fixtures ()
  "Run every discovered pretty-printer fixture."
  (dolist (pathname (discover-pretty-printer-fixtures))
    (validate-one-pretty-printer-fixture (pathname-name pathname))))


;;;;
;;;; Fast tests — autofix-cycle fixture auto-discovery
;;;;
;;;;  An autofix-cycle fixture exercises the complete diagnostic cycle:
;;;;    inspector → finding → maintainer → resolution → apply.
;;;;  The fixture declares the quadruple in its YAML front-matter and
;;;;  supplies three body documents: input source, expected finding
;;;;  slot values (plist), and expected fixed code.
;;;;

(defun autofix-cycle-finding-accessor (slot-key)
  "Return the accessor function for SLOT-KEY on a FINDING instance.
Known keys map to exported FINDING-* readers; the synthetic keys
:CST-NODE-RAW, :OBSERVATION-MATCHES and :SOURCE-TEXT-SUBSTRING are
handled specially by AUTOFIX-CYCLE-FINDING-SLOT-CHECK and do not use
this function. Signals ERROR for unknown keys."
  (declare (type keyword slot-key)
           (values function))
  (case slot-key
    (:line #'atelier:finding-line)
    (:column #'atelier:finding-column)
    (:end-line #'atelier:finding-end-line)
    (:end-column #'atelier:finding-end-column)
    (:severity #'atelier:finding-severity)
    (:source-text #'atelier:finding-source-text)
    (:observation #'atelier:finding-observation)
    (:rationale #'atelier:finding-rationale)
    (:start-line #'atelier:finding-start-line)
    (t (error "Unknown autofix-cycle finding slot key ~S." slot-key))))

(defun autofix-cycle-finding-slot-check (finding expected-finding-class expected-plist)
  "Assert that FINDING is an instance of EXPECTED-FINDING-CLASS and that
every named slot in EXPECTED-PLIST equals the corresponding reader on
FINDING. Synthetic keys supported:

  :CST-NODE-RAW   — compare (cst:raw (finding-cst-node finding)) with EQUAL.
  :OBSERVATION-MATCHES  — substring-of test against FINDING-OBSERVATION.
  :SOURCE-TEXT-SUBSTRING — substring-of test against FINDING-SOURCE-TEXT.

EXPECTED-PLIST may be NIL to skip all slot checks (the class check still runs)."
  (declare (type list expected-plist))
  (assert-type finding expected-finding-class)
  (flet ((substring-p (needle haystack)
           (and (stringp haystack) (search needle haystack))))
    (flet ((check-slot (key value)
             (case key
               (:cst-node-raw
                (assert-equal value
                 (concrete-syntax-tree:raw (atelier:finding-cst-node finding))))
               (:observation-matches
                (assert-t
                 (not
                  (null
                   (substring-p value (atelier:finding-observation finding))))))
               (:source-text-substring
                (assert-t
                 (not
                  (null
                   (substring-p value (atelier:finding-source-text finding))))))
               (t
                (let ((accessor (autofix-cycle-finding-accessor key)))
                  (assert-equal value (funcall accessor finding)))))))
      (loop :for (key value) :on expected-plist :by #'cddr
            :do (check-slot key value)))))

(defun run-autofix-cycle (inspector maintainer source)
  "Run one full autofix cycle on SOURCE and return (values RESULT FINDINGS).

INSPECTOR is an ATELIER:INSPECTOR instance.
MAINTAINER is an ATELIER:MAINTAINER instance.
SOURCE is the input source string.

For a SYNTAX-INSPECTOR, the source is parsed once and INSPECT-SYNTAX is
called on the first top-level form. For a LINE-INSPECTOR, INSPECT-LINE is
called on each line in order, and findings are concatenated.

RESULT is the source after all resolutions have been applied. If no
resolution was produced, RESULT is EQUAL to SOURCE.

FINDINGS is the list of findings produced by the inspector (used by the
slot-check, not by the resolution path)."
  (declare (type string source)
           (values string list))
  (let* ((cst-forms
           (handler-case (atelier:parse-common-lisp source)
             (error () nil)))
         (top-form (first cst-forms))
         (line-vector (atelier:string-to-line-vector source))
         (findings
           (let ((atelier::*current-pathname* #p"fixture.lisp")
                 (atelier::*current-line-vector* line-vector)
                 (atelier::*current-cst-root* top-form))
             (ecase (atelier:inspector-level inspector)
               (:syntax
                (when top-form
                  (atelier:inspect-syntax inspector top-form)))
               (:line
                (loop :for line-number :from 1 :to (length line-vector)
                      :for line = (aref line-vector (1- line-number))
                      :append (atelier:inspect-line inspector line line-number))))))
         (resolutions
           (let ((atelier::*current-line-vector* line-vector))
             (loop :for finding :in findings
                   :for resolution = (atelier:prepare-resolution maintainer finding)
                   :when resolution :collect resolution))))
    (let ((result-string
            (if resolutions
                (atelier:apply-resolutions source resolutions)
                source)))
      (values result-string findings))))

(defun autofix-cycle-comparison-level (inspector)
  "Return :FORM or :STRING depending on how the results of an autofix
cycle under INSPECTOR should be compared. SYNTAX-INSPECTORS use :FORM
(structural AST equality); LINE-INSPECTORS use :STRING (textual equality)."
  (declare (type atelier:inspector inspector)
           (values (member :form :string)))
  (ecase (atelier:inspector-level inspector)
    (:syntax :form)
    (:line :string)))

(defun autofix-cycle-results-equal-p (a b level)
  "Return T if strings A and B are equal at the given LEVEL (:FORM or :STRING)."
  (declare (type string a b)
           (type (member :form :string) level)
           (values boolean))
  (ecase level
    (:form
     (and (equal (read-from-string a) (read-from-string b)) t))
    (:string
     (and (string= a b) t))))

(define-testcase validate-one-autofix-cycle-fixture (maintainer-symbol
                                                     &optional (case-name "baseline"))
  "Validate a single autofix-cycle fixture exercising the full
(inspector, finding, maintainer, resolution) quadruple.

Loads the fixture, runs the inspector, asserts the finding matches the
expected class and the expected plist of slot values, runs the maintainer,
asserts the resolution belongs to the expected class, applies it, and
asserts the result matches the expected fixed code (form equality for
syntax inspectors; string equality for line inspectors). Finally, reruns
the full cycle on the result and asserts self-idempotency at N=1.

Example: (validate-one-autofix-cycle-fixture 'atelier:fix-earmuffs)"
  (let ((fixture-path (autofix-cycle-fixture maintainer-symbol case-name)))
    (unless (probe-file fixture-path)
      (format t "~&SKIP ~A/~A: ~A not found.~%" maintainer-symbol case-name fixture-path)
      (return-from validate-one-autofix-cycle-fixture nil))
    (handler-case
        (multiple-value-bind (front-matter input-source expected-finding-slots
                              expected-fixed-code)
            (read-autofix-cycle-fixture fixture-path)
          (flet ((front-matter-symbol (key)
                   (let ((name (cdr (assoc key front-matter))))
                     (and name (find-symbol (string-upcase name) :atelier)))))
            (let* ((inspector-symbol (front-matter-symbol :inspector))
                   (finding-symbol (front-matter-symbol :finding))
                   (resolution-symbol (front-matter-symbol :resolution))
                   (inspector (atelier:find-inspector inspector-symbol))
                   (maintainer (atelier:find-maintainer maintainer-symbol)))
              (unless (and inspector-symbol finding-symbol resolution-symbol
                           inspector maintainer)
                (format t "~&SKIP ~A/~A: inspector/finding/resolution symbol or runtime object missing.~%"
                        maintainer-symbol case-name)
                (return-from validate-one-autofix-cycle-fixture nil))
              (let ((level (autofix-cycle-comparison-level inspector)))
                ;; First pass: inspect → check finding → maintain → apply.
                (multiple-value-bind (result-1 findings-1)
                    (run-autofix-cycle inspector maintainer input-source)
                  (let ((first-finding (first findings-1)))
                    (assert-t (not (null first-finding)))
                    (autofix-cycle-finding-slot-check
                     first-finding finding-symbol expected-finding-slots)
                    ;; The resolution class check: re-run prepare-resolution to
                    ;; get the actual resolution object and check its class.
                    (let ((atelier::*current-line-vector*
                            (atelier:string-to-line-vector input-source)))
                      (let ((resolution (atelier:prepare-resolution maintainer first-finding)))
                        (assert-type resolution resolution-symbol))))
                  ;; Primary assertion: result matches expected fixed code.
                  (assert-t (autofix-cycle-results-equal-p
                             result-1 expected-fixed-code level))
                  ;; Self-idempotency (N=1): running the cycle again yields the same result.
                  (let ((result-2 (run-autofix-cycle inspector maintainer result-1)))
                    (assert-t (autofix-cycle-results-equal-p
                               result-1 result-2 level))))))))
      (autofix-cycle-fixture-error (condition)
        (format t "~&SKIP ~A/~A: ~A~%" maintainer-symbol case-name condition)
        nil))))

(define-testcase validate-autofix-cycle-fixtures ()
  "Run every discovered autofix-cycle fixture as a full diagnostic-cycle test."
  (dolist (entry (discover-autofix-cycle-fixtures))
    (let ((maintainer-symbol (car entry)))
      (dolist (fixture-path (cdr entry))
        (let ((case-name (pathname-name fixture-path)))
          (validate-one-autofix-cycle-fixture maintainer-symbol case-name))))))

(define-testcase validate-autofix-cycle-fixtures-as-pretty-printer-fixed-points ()
  "For every discovered autofix-cycle fixture whose inspector operates at
the :SYNTAX level, assert that its expected fixed code document is a
fixed point of READ composed with PRETTY-PRINT-FORM.

Rationale: for any syntax-level maintainer (regardless of whether it
emits a SYNTAX-RESOLUTION or a TEXT-RESOLUTION), the pretty-printer is
the authority on canonical text. The expected fixed code in the fixture
must be what the pretty-printer would emit for the corresponding AST —
this keeps the pretty-printer as the single source of truth for formatted
Lisp output and prevents silent drift between text-resolution maintainers
and the formatter.

LINE-inspector fixtures are excluded: their expected document is a text
fragment with whitespace that is semantically meaningful, not a canonical
Lisp form."
  (dolist (entry (discover-autofix-cycle-fixtures))
    (dolist (fixture-path (cdr entry))
      (handler-case
          (multiple-value-bind (front-matter input-source expected-finding-slots
                                expected-fixed-code)
              (read-autofix-cycle-fixture fixture-path)
            (declare (ignore input-source expected-finding-slots))
            (let* ((inspector-symbol
                     (and (cdr (assoc :inspector front-matter))
                          (find-symbol (string-upcase
                                        (cdr (assoc :inspector front-matter)))
                                       :atelier)))
                   (inspector (and inspector-symbol
                                   (atelier:find-inspector inspector-symbol))))
              (when (and inspector
                         (eq (atelier:inspector-level inspector) :syntax))
                (let* ((form (read-from-string expected-fixed-code))
                       (reprinted (atelier:pretty-print-form form 0))
                       ;; Trim trailing newline(s) from the fixture document before
                       ;; comparing: JOIN-LINES re-introduces a final newline that
                       ;; PRETTY-PRINT-FORM does not emit.
                       (expected (string-right-trim '(#\Newline #\Space #\Tab)
                                                    expected-fixed-code)))
                  (assert-string= expected reprinted)))))
        (autofix-cycle-fixture-error (condition)
          (format t "~&SKIP ~A: ~A~%" fixture-path condition)
          nil)))))


;;;;
;;;; Entry points
;;;;

(define-testcase validate-autofix ()
  "Run all autofix integration tests (slow — file I/O)."
  (validate-lint-fix)
  (validate-lint-inspect)
  (validate-lint-partial-fix))

(define-testcase testsuite-autofix ()
  "Run all autofix and fixture-discovery tests."
  (validate-autofix)
  (validate-autofix-cycle-fixtures)
  (validate-autofix-cycle-fixtures-as-pretty-printer-fixed-points)
  (validate-inspector-fixtures)
  (validate-pretty-printer-fixtures))

;;;; End of file `autofix.lisp'
