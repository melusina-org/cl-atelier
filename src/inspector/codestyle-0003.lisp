;;;; codestyle-0003.lisp — Hint at file when it lacks the canonical header line

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

(defun string-shebang-end-position (string)
  "The end position of a possible shebang line in STRING."
  (or (nth-value 1 (ppcre:scan '(:sequence
				 :start-anchor
				 "#!"
				 (:greedy-repetition 0 nil :everything)
				 (:greedy-repetition 1 nil #\Newline))
			       string))
      0))

(define-inspector :codestyle-003 hint-at-file-when-it-lacks-canonical-header-line (contents)
  "Hint at file when it lacks the canonical header line.
The canonical header line is a comment featuring the filename, a dash and a short description."
  (let ((considered-first-line
	  (let ((actual-start
		  (string-shebang-end-position contents)))
	    (subseq contents actual-start (position #\Newline contents :start actual-start))))
	(expected-first-line-prefix
	  (decorate-line-comment *linter* (format nil "~A —"  (file-namestring *hint-pathname*))))
	(corrected-first-line
	  nil)
	(regex
	  (list
	   :sequence
	   :start-anchor
	   (decorate-line-comment *linter* (file-namestring *hint-pathname*))
	   " — ")))
    (unless (ppcre:scan regex considered-first-line)
      (setf corrected-first-line
	    (copy-seq considered-first-line))
      (setf corrected-first-line
	    (ppcre:regex-replace (comment-prefix-regex *linter*) corrected-first-line ""))
      (setf corrected-first-line
	    (ppcre:regex-replace "^(.*)(-|–|—|--) *" corrected-first-line ""))
      (setf corrected-first-line
	    (concatenate 'string expected-first-line-prefix " " corrected-first-line))
      (let ((hint
	      (hint-at-file 
	       "The file does not feature a canonical header line."
	       "The first line of the file should be a canonical header line,
starting with

~A

but instead, this line is

~A

The automatic correction system guesses that

~A

is the correct line.~%"
	   expected-first-line-prefix
	   considered-first-line
	   corrected-first-line)))
	(restart-case
	    (anomaly hint)
	  (autocorrect ()
	    :report "Accept the automatic correction guess"
	    (let ((shebang-end-position
		    (string-shebang-end-position contents)))
	      (concatenate 'string
			   (subseq contents 0 shebang-end-position)
			   (edit-first-line (subseq contents shebang-end-position) corrected-first-line))))
	  (continue ()
	    :report "Remember the anomaly as a hint and continue"
	    hint))))))

;;;; End of file `codestyle-0003.lisp'
