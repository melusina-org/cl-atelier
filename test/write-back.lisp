;;;; write-back.lisp — Testsuite for the Atelier write-back engine

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/test)


;;;;
;;;; Fast tests — in-memory offset arithmetic and string resolution
;;;;

(define-testcase validate-resolution-text-span ()
  "Verify that LINE-COLUMN-TO-OFFSET derives correct character offsets.
Uses a three-line vector: each line is 3 chars, separated by newlines.
  Line 1: positions 0–2 (abc), newline at 3
  Line 2: positions 4–6 (def), newline at 7
  Line 3: positions 8–10 (ghi)"
  (let ((lines (coerce '("abc" "def" "ghi") 'vector)))
    (assert-t (= 0 (atelier::line-column-to-offset 1 0 lines)))
    (assert-t (= 2 (atelier::line-column-to-offset 1 2 lines)))
    (assert-t (= 4 (atelier::line-column-to-offset 2 0 lines)))
    (assert-t (= 6 (atelier::line-column-to-offset 2 2 lines)))
    (assert-t (= 8 (atelier::line-column-to-offset 3 0 lines)))
    (assert-t (= 10 (atelier::line-column-to-offset 3 2 lines)))))

(define-testcase validate-string-to-line-vector ()
  "Verify string-to-line-vector splits correctly."
  (let ((lines (atelier:string-to-line-vector (format nil "abc~%def~%ghi"))))
    (assert-t (= 3 (length lines)))
    (assert-string= "abc" (aref lines 0))
    (assert-string= "def" (aref lines 1))
    (assert-string= "ghi" (aref lines 2))))

(define-testcase validate-apply-resolutions-to-string ()
  "Verify that apply-resolutions on a string replaces text at correct positions.
Replace 'foo' and 'baz' in 'foo bar baz' → 'qux bar quux'."
  (let* ((content "foo bar baz")
         (resolution-foo
           (atelier:make-text-resolution
            :maintainer :test :kind :automatic :description "replace foo"
            :finding (atelier:make-line-finding
                      :inspector :test :severity :warning
                      :observation "" :rationale "" :file #p"test.lisp"
                      :line 1 :column 0 :end-line 1 :end-column 3
                      :source-text "foo")
            :replacement "qux"))
         (resolution-baz
           (atelier:make-text-resolution
            :maintainer :test :kind :automatic :description "replace baz"
            :finding (atelier:make-line-finding
                      :inspector :test :severity :warning
                      :observation "" :rationale "" :file #p"test.lisp"
                      :line 1 :column 8 :end-line 1 :end-column 11
                      :source-text "baz")
            :replacement "quux")))
    (assert-string= "qux bar quux"
                    (atelier:apply-resolutions content
                                              (list resolution-foo resolution-baz)))))

(define-testcase validate-multi-line-span-in-memory ()
  "Verify that a resolution spanning multiple lines replaces the full span in memory."
  (let* ((content (format nil "line1~%line2~%line3~%"))
         (resolution
           (atelier:make-text-resolution
            :maintainer :test :kind :automatic :description "replace lines 1-2"
            :finding (atelier:make-line-finding
                      :inspector :test :severity :warning
                      :observation "" :rationale "" :file #p"test.txt"
                      :line 1 :column 0 :end-line 2 :end-column 5
                      :source-text (format nil "line1~%line2"))
            :replacement "replaced")))
    (assert-string= (format nil "replaced~%line3~%")
                    (atelier:apply-resolutions content (list resolution)))))

(define-testcase validate-syntax-resolution-in-memory ()
  "Verify that a syntax-resolution transform works in memory.
Parse '(+ 1 2)' from a string, transform swaps + for -, verify result."
  (let* ((content "(+ 1 2)")
         (cst-forms (atelier:parse-common-lisp content))
         (cst-node (first cst-forms))
         (finding
           (atelier:make-syntax-finding
            :inspector :test :severity :warning
            :observation "" :rationale "" :file #p"test.lisp"
            :line 1 :column 0 :end-line 1 :end-column 7
            :source-text "(+ 1 2)"
            :cst-node cst-node :cst-root cst-node))
         (resolution
           (atelier:make-syntax-resolution
            :maintainer :test :kind :automatic :description "swap + for -"
            :finding finding
            :transform (flet ((swap-operator (form)
                                (list* '- (rest form))))
                         #'swap-operator))))
    (assert-string= "(- 1 2)"
                    (atelier:apply-resolutions content (list resolution)))))


;;;;
;;;; Slow tests — file write-back (encoding, atomicity)
;;;;

(define-testcase validate-file-write-back-utf8 ()
  "Verify that apply-resolutions on a pathname preserves UTF-8 encoding.
File with multi-byte characters (©, –, ë) must survive the round-trip."
  (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (write-string ";;;; © Michaël – test" s))
    (let* ((resolution
             (atelier:make-text-resolution
              :maintainer :test :kind :automatic :description "replace test"
              :finding (atelier:make-line-finding
                        :inspector :test :severity :warning
                        :observation "" :rationale "" :file p
                        :line 1 :column 17 :end-line 1 :end-column 21
                        :source-text "test")
              :replacement "done"))
           (result (atelier:apply-resolutions p (list resolution))))
      (assert-string= ";;;; © Michaël – done" result)
      (assert-string= ";;;; © Michaël – done"
                      (uiop:read-file-string p :external-format :utf-8)))))

(define-testcase validate-file-write-back-atomicity ()
  "Verify that apply-resolutions on a pathname writes atomically.
The file must exist with correct content after the operation."
  (uiop:with-temporary-file (:pathname p :type "txt" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (write-string "hello world" s))
    (let* ((resolution
             (atelier:make-text-resolution
              :maintainer :test :kind :automatic :description "replace hello"
              :finding (atelier:make-line-finding
                        :inspector :test :severity :warning
                        :observation "" :rationale "" :file p
                        :line 1 :column 0 :end-line 1 :end-column 5
                        :source-text "hello")
              :replacement "goodbye")))
      (atelier:apply-resolutions p (list resolution))
      ;; File must exist and have correct content.
      (assert-t (not (null (probe-file p))))
      (assert-string= "goodbye world"
                      (uiop:read-file-string p :external-format :utf-8)))))


;;;;
;;;; Testcase entry points
;;;;

(define-testcase validate-write-back ()
  "Run all write-back engine tests."
  ;; Fast tests
  (validate-resolution-text-span)
  (validate-string-to-line-vector)
  (validate-apply-resolutions-to-string)
  (validate-multi-line-span-in-memory)
  (validate-syntax-resolution-in-memory)
  ;; Slow tests (file I/O)
  (validate-file-write-back-utf8)
  (validate-file-write-back-atomicity))

(define-testcase testsuite-write-back ()
  "Run all write-back tests."
  (validate-write-back))

;;;; End of file `write-back.lisp'
