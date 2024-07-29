;;;; codestyle-0006.lisp — Hint at file when it lacks project license information

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

(define-inspector :codestyle-006 hint-at-file-when-it-lacks-project-license-information (contents)
  "Hint at file when it lacks the canonical project license information."
  (labels
      ((sloppy-license-header-regex (designator)
	 (cons
	  :sequence
	  (loop :for word :in (string-words (slot-value (find-license designator) 'license-header))
		:collect `(:greedy-repetition 0 1 ,(comment-prefix-regex *linter*))
		:collect '(:greedy-repetition 0 nil :whitespace-char-class)
		:collect word
		:collect '(:greedy-repetition 0 nil :whitespace-char-class))))
       (sloppy-any-license-header-regex ()
	 (list
	  :sequence
	  (list
	   :alternation
	   '(:greedy-repetition 2 nil #\Newline)
	   `(:sequence #\Newline ,(comment-prefix-regex *linter*) #\Newline))
	  (cons
	   :alternation
	   (loop :for designator :in (list-licenses)
		 :collect (sloppy-license-header-regex designator)))
	  '(:greedy-repetition 0 nil #\Newline)))
       (project-license-header ()
	 (decorate-block-comment
	  *linter*
	  (string-lines
	   (string-trim '(#\Newline)
			  (slot-value
			   (find-license (parameter-replacement-text :license))
			   'license-header)))))
       (project-license-regex ()
	 (list :sequence (project-license-header))))
    (unless (ppcre:scan (project-license-regex) contents)
      (multiple-value-bind (guess-start guess-end)
	  (ppcre:scan (sloppy-any-license-header-regex) contents)
	(if guess-start
	    (let ((hint
		    (hint-at-file
		     "The file does not feature a project license information blob."
"The third comment block of the file should feature a project license
information

~A

but this block was not found. The automatic correction system guesses
that this comment block is currently written as

~A

and proposes to replace this block with the expected one to resolve
the anomaly.~%"
		     (project-license-header) (subseq contents guess-start guess-end))))
	      (restart-case
		  (anomaly hint)
		(autocorrect ()
		  :report "Accept the automatic correction guess"
		  (with-output-to-string (buffer)
		    (write-string contents buffer :end guess-start)
		    (write-char #\Newline buffer)
		    (write-char #\Newline buffer)
		    (write-string (project-license-header) buffer)
		    (write-char #\Newline buffer)
		    (write-char #\Newline buffer)
		    (write-string contents buffer :start guess-end )))
		(continue ()
		  :report "Remember the anomaly as a hint and continue"
		  hint)))
	    (hint-at-file
		     "The file does not feature a project license information blob."
"The third comment block of the file should feature a project license
information

~A

but this block was not found and the automatic correction system could
not find a suitable block to resolve the anomaly.~%"
	     (project-license-header)))))))

;;;; End of file `codestyle-0006.lisp'
