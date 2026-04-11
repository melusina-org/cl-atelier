;;;; input-schema-derivation.lisp — Tests for derive-input-schema-from-lambda-list

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

(define-testcase validate-input-schema-empty ()
  "Empty lambda list produces {type:object, properties:{}}."
  (assert-string= "{\"type\":\"object\",\"properties\":{}}"
                  (encode-to-string
                   (derive-input-schema-from-lambda-list '() '()))))

(define-testcase validate-input-schema-one-string ()
  "A single &key parameter with a type declaration is required and typed."
  (assert-string=
   "{\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\"}},\"required\":[\"name\"]}"
   (encode-to-string
    (derive-input-schema-from-lambda-list
     '(&key name)
     '((declare (type string name)))))))

(define-testcase validate-input-schema-two-typed ()
  "Two typed parameters produce both properties and required entries."
  (let* ((schema (derive-input-schema-from-lambda-list
                  '(&key name count)
                  '((declare (type string name))
                    (declare (type integer count)))))
         (encoded (encode-to-string schema)))
    (assert-t* (search "\"name\":{\"type\":\"string\"}" encoded))
    (assert-t* (search "\"count\":{\"type\":\"integer\"}" encoded))
    (assert-t* (search "\"required\":[\"name\",\"count\"]" encoded))))

(define-testcase validate-input-schema-untyped ()
  "A &key parameter with no type declaration gets an empty property schema."
  (assert-string=
   "{\"type\":\"object\",\"properties\":{\"x\":{}},\"required\":[\"x\"]}"
   (encode-to-string
    (derive-input-schema-from-lambda-list '(&key x) '()))))

(define-testcase validate-input-schema-member-type ()
  "A (member ...) type becomes a string with enum."
  (let* ((schema (derive-input-schema-from-lambda-list
                  '(&key level)
                  '((declare (type (member :debug :info :warn :error) level)))))
         (encoded (encode-to-string schema)))
    (assert-t* (search "\"enum\":[\"debug\",\"info\",\"warn\",\"error\"]" encoded))))

(define-testcase validate-derive-input-schema-from-lambda-list ()
  (validate-input-schema-empty)
  (validate-input-schema-one-string)
  (validate-input-schema-two-typed)
  (validate-input-schema-untyped)
  (validate-input-schema-member-type))

;;;; End of file `input-schema-derivation.lisp'
