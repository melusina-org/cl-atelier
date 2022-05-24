;;;; codestyle-0002.lisp — Hint at very long lines

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.atelier)

(define-inspector :codestyle-0002 hint-at-file-line-when-it-is-very-long (line &key (maximum-length 100))
  "Hint at very long LINE.
The parameter MAXIMUM-LENGTH is set to 100, which matches typical
Common Lisp style guides. This is usually a good value for programming
but pure text should aim at a smaller length.  For pure text, a maximum
length of 80 or maybe 70 characters seems appropriate.

When a line has only one word, it is not counted as very long. When a line
is a lisp definition, it does not need to be short. This latter rule makes
grep more useful."
  (flet ((very-long-line-p (line)
	   (> (length line) maximum-length))
	 (single-word-line-p (line)
	   (<= (count-string-words line) 1))
	 (lisp-definition-line-p (line)
	   (some (lambda (regex) (ppcre:scan regex line))
		 '("^ *\\(define-"
		   "^ *\\(defun "
		   "^ *\\(defmacro "
		   "^ *\\(defvar "
		   "^ *\\(defparameter "
		   "^ *\\(defconstant "
		   "^ *\\(defmethod "))))
    
    (when (and (very-long-line-p line)
	       (not (single-word-line-p line))
	       (not (lisp-definition-line-p line)))
      (hint-at-file-line "This line is very long."
			 "The line ~A is longer than ~A characters, which makes it harder to read."
			 *hint-line* maximum-length))))

;;;; End of file `codestyle-0002.lisp'
