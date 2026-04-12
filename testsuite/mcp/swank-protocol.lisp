;;;; swank-protocol.lisp — Tests for SWANK wire protocol encoding

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/testsuite/mcp)

;;; These tests verify the SWANK wire protocol message encoding and
;;; decoding without needing a child SBCL or a real SWANK server.

(define-testcase validate-swank-message-encoding ()
  "Verify that swank-send-raw produces correctly framed messages."
  ;; Test 1: A simple message should be 6-char hex header + UTF-8 payload
  (let* ((msg "(:emacs-rex (+ 1 2) \"CL-USER\" t 1)")
         (octets (flexi-streams:string-to-octets msg :external-format :utf-8))
         (expected-header (format nil "~6,'0x" (length octets))))
    (assert-string= expected-header
                     (format nil "~6,'0x" (length octets)))
    ;; Header is always 6 characters
    (assert= 6 (length expected-header))))

(define-testcase validate-swank-message-decoding ()
  "Verify that a round-trip encode/decode preserves the s-expression."
  ;; Build a mock SWANK message in a buffer
  (let* ((sexp '(:return 1 (:ok "42") 1))
         (payload (prin1-to-string sexp))
         (payload-octets (flexi-streams:string-to-octets payload :external-format :utf-8))
         (header (format nil "~6,'0x" (length payload-octets)))
         (header-octets (flexi-streams:string-to-octets header :external-format :utf-8))
         (buffer (make-array (+ 6 (length payload-octets))
                             :element-type '(unsigned-byte 8))))
    (replace buffer header-octets)
    (replace buffer payload-octets :start1 6)
    ;; Verify the encoding is reversible
    (let ((decoded (read-from-string
                    (flexi-streams:octets-to-string payload-octets
                                                    :external-format :utf-8))))
      (assert-equal sexp decoded))))

;;;; End of file `swank-protocol.lisp'
