;;;; define-tool-macro.lisp — Tests for the DEFINE-TOOL macro

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

;;; All tests use WITH-ISOLATED-REGISTRIES to keep test definitions
;;; out of the production registries.

(define-testcase validate-define-tool-tool-only ()
  "A DEFINE-TOOL without (:resource ...) registers a tool only."
  (with-isolated-registries
    (let ((*package* (find-package :atelier/mcp)))
      (eval (read-from-string
             "(define-tool test-ping () (:description \"smoke\") \"pong\")")))
    (assert-eql 1 (hash-table-count *tool-registry*))
    (assert-eql 0 (hash-table-count *concrete-resource-registry*))
    (assert-eql 0 (hash-table-count *template-resource-registry*))
    (let ((tool (find-tool-by-name "atelier:test-ping")))
      (assert-t (not (null tool)))
      (assert-string= "pong" (handle-tool-call tool nil))
      (assert-string= "smoke" (tool-description tool)))))

(define-testcase validate-define-tool-with-concrete-resource ()
  "A DEFINE-TOOL with (:resource :uri ...) and no placeholders
   registers both a tool and a concrete resource."
  (with-isolated-registries
    (let ((*package* (find-package :atelier/mcp)))
      (eval (read-from-string
             "(define-tool test-concrete ()
                (:description \"concrete\")
                (:resource :uri \"test://a\"
                           :name \"Concrete\"
                           :mime-type :application/json)
                (list (cons \"ok\" 1)))")))
    (assert-eql 1 (hash-table-count *tool-registry*))
    (assert-eql 1 (hash-table-count *concrete-resource-registry*))
    (assert-eql 0 (hash-table-count *template-resource-registry*))
    (let ((tool (find-concrete-resource-by-uri "test://a")))
      (assert-t (not (null tool)))
      (assert-string= "application/json" (resource-mime-type tool)))))

(define-testcase validate-define-tool-with-templated-resource ()
  "A DEFINE-TOOL with a {placeholder} URI registers as a template."
  (with-isolated-registries
    (let ((*package* (find-package :atelier/mcp)))
      (eval (read-from-string
             "(define-tool test-templated (&key name)
                (:description \"templated\")
                (:resource :uri \"test://t/{name}\"
                           :name \"Templated\"
                           :mime-type :application/json)
                (declare (type string name))
                (list (cons \"name\" name)))")))
    (assert-eql 1 (hash-table-count *tool-registry*))
    (assert-eql 0 (hash-table-count *concrete-resource-registry*))
    (assert-eql 1 (hash-table-count *template-resource-registry*))
    (multiple-value-bind (tool bindings)
        (match-resource-uri "test://t/foo")
      (assert-t (not (null tool)))
      (assert-equal '(("name" . "foo")) bindings))))

(define-testcase validate-define-tool-resource-only ()
  "A DEFINE-TOOL with (:tool nil) registers only as a resource."
  (with-isolated-registries
    (let ((*package* (find-package :atelier/mcp)))
      (eval (read-from-string
             "(define-tool test-resource-only ()
                (:description \"resource only\")
                (:resource :uri \"test://ro\"
                           :name \"RO\"
                           :mime-type :text/plain)
                (:tool nil)
                \"body\")")))
    (assert-eql 0 (hash-table-count *tool-registry*))
    (assert-eql 1 (hash-table-count *concrete-resource-registry*))))

(define-testcase validate-define-tool-rejects-positional-args ()
  "A positional parameter is a compile-time error."
  (assert-condition
   (let ((*package* (find-package :atelier/mcp)))
     (eval (read-from-string
            "(define-tool bad (x) (:description \"bad\") x)")))
   error))

(define-testcase validate-define-tool-rejects-missing-description ()
  "A missing (:description) clause is a compile-time error."
  (assert-condition
   (let ((*package* (find-package :atelier/mcp)))
     (eval (read-from-string
            "(define-tool bad () \"body\")")))
   error))

(define-testcase validate-define-tool-rejects-template-mismatch ()
  "A URI template placeholder not matching &key parameters errors."
  (assert-condition
   (let ((*package* (find-package :atelier/mcp)))
     (eval (read-from-string
            "(define-tool bad (&key other)
               (:description \"bad\")
               (:resource :uri \"test://{name}\"
                          :name \"Bad\"
                          :mime-type :application/json)
               other)")))
   error))

(define-testcase validate-define-tool-tests ()
  (validate-define-tool-tool-only)
  (validate-define-tool-with-concrete-resource)
  (validate-define-tool-with-templated-resource)
  (validate-define-tool-resource-only)
  (validate-define-tool-rejects-positional-args)
  (validate-define-tool-rejects-missing-description)
  (validate-define-tool-rejects-template-mismatch))

;;;; End of file `define-tool-macro.lisp'
