;;;; codestyle-0005.lisp — Hint at file when it lacks the canonical project identification

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

(define-inspector :codestyle-005 hint-at-file-when-it-lacks-canonical-project-identification (contents)
  "Hint at file when it lacks the canonical project identification.
The canonical project identification is a group of comment lines following the first line
and summarising the project name, the project page, the copyright holder and the
license under which the file is released."
  (labels ((project-name ()
	     (parameter-replacement-text :project-name))
	   (homepage ()
	     (parameter-replacement-text :homepage))
	   (copyright-year ()
	     (parameter-replacement-text :copyright-year))
	   (copyright-holder ()
	     (parameter-replacement-text :copyright-holder))
	   (project-identification-text ()
	     (decorate-block-comment
	      *linter*
	      (string-lines
	       (with-output-to-string (buffer)
		 (format buffer "~A (~A)~%" (project-name) (homepage))
		 (format buffer "This file is part of ~A.~%~%" (project-name))
		 (format buffer "Copyright © ~A ~A~%" (copyright-year) (copyright-holder))
		 (format buffer "All rights reserved.")))))
	   (project-identification-regex ()
	     (list :sequence (project-identification-text)))
	   (sloppy-identification-regex ()
	     (let ((identification-line
		     `(:sequence
		       ,(comment-prefix-regex *linter*)
		       (:alternation
			(:greedy-repetition 0 nil #\Space)
			(:sequence
			 #\Space
			 (:greedy-repetition 0 nil :everything)
			 (:alternation ,(project-name) "https" "Copyright" "This file is part of" "All rights reserved")	
			 (:greedy-repetition 0 nil :everything)))
		       #\Newline)))
	       `(:sequence
		 (:greedy-repetition 2 2 #\Newline)
		 (:greedy-repetition 1 5 ,identification-line)
		 (:alternation
		  #\Newline
		  (:sequence
		   ,(comment-prefix-regex *linter*)
		   #\Newline))))))
    (unless (ppcre:scan (project-identification-regex) contents)
      (multiple-value-bind (guess-start guess-end)
	  (ppcre:scan (sloppy-identification-regex) contents)
	(if guess-start
	    (let ((hint
		    (hint-at-file
		     "The file does not feature a canonical project identification."
"The second comment block of the file should feature a canonical project
identification

~A

but this block was not found. The automatic correction system guesses
that this comment block is currently written as

~A

and proposes to replace this block with the expected one to resolve
the anomaly.~%"
		     (project-identification-text) (subseq contents guess-start guess-end))))
	      (restart-case
		  (anomaly hint)
		(autocorrect ()
		  :report "Accept the automatic correction guess"
		  (with-output-to-string (buffer)
		    (write-string contents buffer :end guess-start)
		    (write-char #\Newline buffer)
		    (write-char #\Newline buffer)
		    (write-string (project-identification-text) buffer)
		    (write-char #\Newline buffer)
		    (write-char #\Newline buffer)
		    (write-string contents buffer :start guess-end )))
		(continue ()
		  :report "Remember the anomaly as a hint and continue"
		  hint)))
	    (hint-at-file
	     "The file does not feature a canonical project identification."
	     "The second comment block of the file should feature a canonical project
identification

~A

but this block was not found and the automatic correction system could
not find a suitable block to resolve the anomaly.~%"
	     (project-identification-text)))))))

;;;; End of file `codestyle-0005.lisp'
