;;;; utilities.lisp — Utilities for the Atelier Lisp System

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:atelier)

(defun string-match (pattern text)
  "Predicate recognising TEXT matching a globbing PATTERN."
  (let ((text-length (length text))
        (pattern-length (length pattern)))
    (labels
        ((match-step (i j)
           (case (when (and (<= j text-length) (< i pattern-length))
		   (elt pattern i))
             ((nil)
              (eq j text-length))
             (#\?
              (and (< j text-length) (match-step (1+ i) (1+ j))))
             (#\*
	      (or (match-step (1+ i) j) (match-step i (1+ j))))
             (t
              (when (< j text-length)
                (and (char= (elt pattern i) (elt text j))
                     (match-step (1+ i) (1+ j))))))))
      (match-step 0 0))))

(defun string-suffix-p (pattern text)
  "Predicate recognising TEXT ending with PATTERN."
  (string-match (concatenate 'string "*" pattern) text))

(defun string-prefix-p (pattern text)
  "Predicate recognising TEXT starting with PATTERN."
  (string-match (concatenate 'string pattern "*") text))

(defun join-lines (lines &optional (separator #\Newline))
  (with-output-to-string (buffer)
    (loop for line in lines
	  for first-line = t then nil
	  do (unless first-line
	       (format buffer "~A" separator))
	  do (write-string line buffer))))

(defun string-lines (string)
  "Prepare the list of lines in STRING."
  (loop for start-of-line = 0 then (1+ end-of-line)
        for end-of-line = (position #\Newline string :start start-of-line)
        collect (subseq string start-of-line end-of-line)
        while end-of-line))

(defun string-words (string)
  "Prepare the list of words in STRING."
  (ppcre:split '(:greedy-repetition 1 nil :whitespace-char-class) string))

(defun first-line (string)
  "The first line of STRING."
  (subseq string 0 (position #\Newline string)))

(defun (setf first-line) (new-value string)
  "Set the first line of STRING to NEW-VALUE."
  (setf (subseq string 0 (position #\Newline string)) new-value))

(defun edit-first-line (string first-line)
  "Edit the first line of STRING to FIRST-LINE as a new string."
  (let ((end-of-line
	  (position #\Newline string)))
    (with-output-to-string (buffer)
      (write-string first-line buffer)
      (write-string string buffer :start end-of-line))))

(defun last-line-position (string)
  "The position of the last line of STRING.
The position is represented as multiple values, the START-POS and END-POS."
  (let* ((length
	   (length string))
	 (end-pos
	   (if (char= #\Newline (char string (1- length)))
	       (1- length)
	       length))
	 (start-pos
	   (let ((position
		   (position #\Newline string :from-end t :end end-pos)))
	     (if position
		 (1+ position)
		 0))))
    (values start-pos end-pos)))

(defun last-line (string)
  "The last line of STRING."
  (multiple-value-bind (start-pos end-pos) (last-line-position string)
    (subseq string start-pos end-pos)))

(defun (setf last-line) (new-value string)
  "Set the last line of STRING to NEW-VALUE."
  (multiple-value-bind (start-pos end-pos) (last-line-position string)
    (setf (subseq string start-pos end-pos) new-value)))

(defun edit-last-line (string last-line)
  "Edit the last line of STRING to LAST-LINE as a new string."
  (multiple-value-bind (start-pos end-pos) (last-line-position string)
    (with-output-to-string (buffer)
      (write-string string buffer :start 0 :end start-pos)
      (write-string last-line buffer)
      (write-string string buffer :start end-pos))))


(defun string-list-p (object)
  "Predicate recognising lists of strings."
  (and (listp object)
       (every #'stringp object)))

(defun read-file-into-list (pathname)
  "Read PATHNAME into a list of lines.

When PATHNAME is a list of strings, it is returned as is, which
is useful for testing."
  (cond
    ((string-list-p pathname)
     pathname)
    (t
     (string-lines
      (read-file-into-string pathname :external-format :utf8)))))

(defun count-string-words (string)
  "Count the number of words in STRING."
  (flet ((count-matches (regex)
	   (/ (length (ppcre:all-matches regex string)) 2)))
    (1+ (- (count-matches "[ ]+")
	   (count-matches "^ +")
	   (count-matches " +$")))))
  
(defun add-pathname (pathname list-of-pathnames)
  "Add PATHNAME to LIST-OF-PATHNAMES unless it is already found there."
  (unless (member pathname list-of-pathnames)
    (push pathname list-of-pathnames)))

(defun indent (target-string &optional (margin-left 4))
  (let ((indentation
	  (make-string margin-left :initial-element #\Space)))
    (ppcre:regex-replace-all (ppcre:create-scanner "^" :multi-line-mode t)
			     target-string
			     indentation)))

(defun break-down (taste sequence)
  "Break down a list SEQUENCE into consecutive lists of constant TASTE.
The TASTE argument is a function which is applied on sequence elements to
taste them. Taste values are 

The answer is an alist whose terms have the form

   (TASTE1 . SEQUENCE1)

such that:

  1. The concatenation of the SEQUENCE1s yields SEQUENCE.
  2. Each element of the list SEQUENCE1 has the given TASTE1.
  3. Consecutive terms of the answer have distinct TASTE1.
"
  (flet ((catamorphism (state next)
	   (destructuring-bind (current-taste current-subsequence accumulator) state
	     (let ((next-taste
		     (funcall taste next)))
	       (cond
		 ((eq nil current-subsequence)
		  (list next-taste (list next) accumulator))
		 ((eq next-taste current-taste)
		  (list current-taste (cons next current-subsequence) accumulator))
		 (t
		  (list next-taste
			(list next)
			(cons (cons current-taste (nreverse current-subsequence)) accumulator))))))))
    (destructuring-bind (current-taste current-subsequence accumulator)
	(reduce #'catamorphism sequence :initial-value (list nil nil nil))
      (nreverse (cons (cons current-taste (nreverse current-subsequence)) accumulator)))))


(defun find-regular-files (&rest pathnames)
  (let ((prune-list
	  (list ".DS_Store" ".git" ".hg" ".svn" "CVS" "*.fasl")))
    (flet ((find-predicate (prune-list)
	     (let ((prune-expr
		     (loop :for (prune . tail) :on prune-list
			   :collect "-name"
			   :collect prune
			   :when tail
			   :collect "-o")))
	       `("(" ,@prune-expr ")" "-prune" "-o" "-type" "f" "-print"))))
  (uiop:run-program
   (append (list "/usr/bin/find")
	   (mapcar #'namestring pathnames)
	   (find-predicate prune-list))
   :output :lines))))

;;;; End of file `utilities.lisp'
