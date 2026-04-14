;;;; uri-template.lisp — URI template parsing, matching, validation

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp-kernel)

;;; Atelier's URI templates are a level-1 subset of RFC 6570: a single
;;; {name} placeholder within an otherwise literal URI. No reserved
;;; expansions, no path expansions, no query expansions. Multiple
;;; placeholders in one template are supported.
;;;
;;; The parser turns "atelier://inspectors/{name}" into a sequence of
;;; (:literal "atelier://inspectors/") and (:placeholder "name"). The
;;; matcher walks the sequence against a concrete URI and extracts the
;;; placeholder bindings, or fails.

(defun parse-uri-template (template)
  "Parse TEMPLATE into a list of segments. Each segment is either a
   literal string or a placeholder name in the form (:placeholder NAME).
   Literal segments are returned as (:literal STRING)."
  (loop :with i := 0
        :with n := (length template)
        :while (< i n)
        :collect (multiple-value-bind (segment next) (%next-segment template i n)
                   (setf i next)
                   segment)))

(defun %next-segment (template i n)
  "Return (values SEGMENT NEXT-INDEX) for the segment starting at I in
   TEMPLATE of length N. SEGMENT is either a (:literal STRING) cons or
   a (:placeholder NAME) cons."
  (if (char= (char template i) #\{)
      (%next-placeholder template i n)
      (%next-literal template i n)))

(defun %next-placeholder (template i n)
  "Scan TEMPLATE from I expecting a placeholder {NAME}. Returns
   (values (:placeholder NAME) NEXT-INDEX). Signals ERROR on an
   unterminated placeholder."
  (let ((end (position #\} template :start (1+ i) :end n)))
    (unless end
      (error "Unterminated URI template placeholder in ~S at position ~D."
             template i))
    (values (list :placeholder (subseq template (1+ i) end))
            (1+ end))))

(defun %next-literal (template i n)
  "Scan TEMPLATE from I for a literal segment ending at the next {
   or at position N. Returns (values (:literal STRING) NEXT-INDEX)."
  (let ((end (or (position #\{ template :start i :end n) n)))
    (values (list :literal (subseq template i end))
            end)))

(defun uri-template-placeholders (template-or-segments)
  "Return the list of placeholder names (strings) in TEMPLATE-OR-SEGMENTS.
   Accepts either a template string or the parsed segment sequence."
  (let ((segments (if (stringp template-or-segments)
                      (parse-uri-template template-or-segments)
                      template-or-segments)))
    (loop :for segment :in segments
          :when (eq (first segment) :placeholder)
            :collect (second segment))))

(defun uri-template-static-p (template)
  "Return T if TEMPLATE has no {placeholder} segments — a concrete URI."
  (null (uri-template-placeholders template)))

(defun match-uri-against-template (uri template)
  "Match URI against TEMPLATE.
   Returns (VALUES BINDINGS MATCHED-P). On success, BINDINGS is an alist
   (possibly empty for a static match) and MATCHED-P is T. On failure,
   returns (VALUES NIL NIL). Callers must test MATCHED-P, not the
   bindings list, because an empty alist is a legitimate successful
   match for a template with no placeholders."
  (let ((segments (parse-uri-template template)))
    (%match-segments segments uri 0 (length uri) nil)))

(defun %match-segments (segments uri i n bindings)
  "Walk SEGMENTS against URI[I..N], accumulating BINDINGS.
   Returns the multiple-value convention of MATCH-URI-AGAINST-TEMPLATE."
  (cond
    ((null segments)
     (if (= i n) (values (nreverse bindings) t) (values nil nil)))
    ((eq (first (first segments)) :literal)
     (%match-literal (second (first segments)) (rest segments)
                     uri i n bindings))
    (t
     (%match-placeholder (second (first segments)) (rest segments)
                         uri i n bindings))))

(defun %match-literal (literal rest-segments uri i n bindings)
  "Try to match LITERAL starting at URI[I]. On match, recurse with
   REST-SEGMENTS; on mismatch, return (values nil nil)."
  (let ((end (+ i (length literal))))
    (if (and (<= end n)
             (string= literal uri :start2 i :end2 end))
        (%match-segments rest-segments uri end n bindings)
        (values nil nil))))

(defun %match-placeholder (name rest-segments uri i n bindings)
  "Extract the value for NAME. If REST-SEGMENTS starts with a literal,
   scan for that literal as a terminator; otherwise take everything to
   the end. Add (name . value) to BINDINGS and recurse."
  (let* ((terminator (%next-literal-of rest-segments))
         (value-end  (%scan-placeholder-end uri i n terminator)))
    (if value-end
        (%match-segments rest-segments uri value-end n
                         (cons (cons name (subseq uri i value-end)) bindings))
        (values nil nil))))

(defun %next-literal-of (segments)
  "Return the literal string that immediately follows the current
   placeholder in SEGMENTS, or NIL if the placeholder is terminal."
  (when (and segments (eq (first (first segments)) :literal))
    (second (first segments))))

(defun %scan-placeholder-end (uri start end terminator)
  "Return the index where the placeholder value ends. With no terminator
   the value runs to END. With a terminator, search for it and return
   the index where the terminator begins. Returns NIL on failure."
  (if terminator
      (search terminator uri :start2 start :end2 end)
      end))

(defun validate-uri-template-against-lambda-list (template lambda-list)
  "Signal INVALID-URI-TEMPLATE if the placeholder set in TEMPLATE does
   not equal the keyword parameter set in LAMBDA-LIST. On success,
   return T."
  (let ((placeholders (mapcar #'%name-as-string (uri-template-placeholders template)))
        (params (mapcar (lambda (p) (string-downcase (symbol-name p)))
                        (%lambda-list-keyword-parameters lambda-list))))
    (let ((extra (set-difference placeholders params :test #'string=))
          (missing (set-difference params placeholders :test #'string=)))
      (when (or extra missing)
        (error 'invalid-uri-template
               :template template
               :lambda-list lambda-list
               :message (format nil "URI template mismatch for ~S"
                                template)
               :mismatch (format nil "Unmatched placeholders: ~S; unused &key: ~S"
                                 extra missing))))
    t))

(defun %name-as-string (name)
  "Coerce a placeholder NAME (already a string) to its string form."
  (string-downcase name))

;;;; End of file `uri-template.lisp'
