;;;; write-back.lisp — Testsuite for the Atelier write-back engine

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite)


;;;;
;;;; Fast tests — in-memory offset arithmetic
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


;;;;
;;;; Slow tests — file write-back
;;;;

(define-testcase validate-multiple-resolutions-ordering ()
  "Verify that multiple resolutions on one file are applied end-to-start.
Replacing 'foo' and 'baz' in 'foo bar baz' must yield 'qux bar quux'
regardless of the order resolutions are supplied."
  (uiop:with-temporary-file (:pathname p :type "txt" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (write-string "foo bar baz" s))
    (let* ((resolution-foo
             (atelier:make-text-resolution
              :maintainer :test :kind :automatic :description "replace foo"
              :finding (atelier:make-line-finding
                        :inspector :test :severity :warning
                        :observation "" :rationale "" :file p
                        :line 1 :column 0 :end-line 1 :end-column 3
                        :source-text "foo")
              :replacement "qux"))
           (resolution-baz
             (atelier:make-text-resolution
              :maintainer :test :kind :automatic :description "replace baz"
              :finding (atelier:make-line-finding
                        :inspector :test :severity :warning
                        :observation "" :rationale "" :file p
                        :line 1 :column 8 :end-line 1 :end-column 11
                        :source-text "baz")
              :replacement "quux")))
      (atelier:apply-resolutions-to-file p (list resolution-foo resolution-baz))
      (assert-string= "qux bar quux"
                      (uiop:read-file-string p :external-format :utf-8)))))

(define-testcase validate-multi-line-span-replacement ()
  "Verify that a resolution spanning multiple lines replaces the full span.
Replacing lines 1–2 of a three-line file leaves line 3 intact."
  (uiop:with-temporary-file (:pathname p :type "txt" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (format s "line1~%line2~%line3~%"))
    ;; Span covers "line1\nline2": line 1 col 0 → line 2 col 5.
    (let* ((resolution
             (atelier:make-text-resolution
              :maintainer :test :kind :automatic :description "replace lines 1-2"
              :finding (atelier:make-line-finding
                        :inspector :test :severity :warning
                        :observation "" :rationale "" :file p
                        :line 1 :column 0 :end-line 2 :end-column 5
                        :source-text (format nil "line1~%line2"))
              :replacement "replaced")))
      (atelier:apply-resolutions-to-file p (list resolution))
      (assert-string= (format nil "replaced~%line3~%")
                      (uiop:read-file-string p :external-format :utf-8)))))

(define-testcase validate-text-resolution-write-back ()
  "Verify that a text-resolution replaces a symbol name in a source file."
  (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (write-string "(defconstant wrong-name 42)" s))
    ;; "wrong-name" starts at column 13 and ends at column 23.
    (let* ((resolution
             (atelier:make-text-resolution
              :maintainer :test :kind :automatic :description "add earmuffs"
              :finding (atelier:make-line-finding
                        :inspector :test :severity :warning
                        :observation "" :rationale "" :file p
                        :line 1 :column 13 :end-line 1 :end-column 23
                        :source-text "wrong-name")
              :replacement "+wrong-name+")))
      (atelier:apply-resolutions-to-file p (list resolution))
      (assert-string= "(defconstant +wrong-name+ 42)"
                      (uiop:read-file-string p :external-format :utf-8)))))

(define-testcase validate-syntax-resolution-write-back ()
  "Verify that a syntax-resolution transform replaces the entire CST form.
Parses '(+ 1 2)' from a temp file; the transform swaps '+' for '-';
the file must contain '(- 1 2)' after apply."
  (uiop:with-temporary-file (:pathname p :type "lisp" :keep nil)
    (with-open-file (s p :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (write-string "(+ 1 2)" s))
    (let* ((cst-forms (atelier::parse-lisp-file p))
           (cst-node (first cst-forms))
           (finding
             (atelier:make-syntax-finding
              :inspector :test :severity :warning
              :observation "" :rationale "" :file p
              :line 1 :column 0 :end-line 1 :end-column 7
              :source-text "(+ 1 2)"
              :cst-node cst-node :cst-root cst-node))
           (resolution
             (atelier:make-syntax-resolution
              :maintainer :test :kind :automatic :description "swap + for -"
              :finding finding
              :transform (flet ((swap-operator (form)
                                  ;; Replace the operator symbol with -.
                                  (list* '- (rest form))))
                           #'swap-operator))))
      (atelier:apply-resolutions-to-file p (list resolution))
      (assert-string= "(- 1 2)"
                      (uiop:read-file-string p :external-format :utf-8)))))


;;;;
;;;; Testcase entry points
;;;;

(define-testcase validate-write-back ()
  "Run all write-back engine tests."
  (validate-resolution-text-span)
  (validate-multiple-resolutions-ordering)
  (validate-multi-line-span-replacement)
  (validate-text-resolution-write-back)
  (validate-syntax-resolution-write-back))

(define-testcase testsuite-write-back ()
  "Run all write-back tests."
  (validate-write-back))

;;;; End of file `write-back.lisp'
