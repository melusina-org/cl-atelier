;;;; codestyle-0004.lisp — Hint at file when it lacks the canonical footer line

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.atelier)

(define-inspector :codestyle-004 hint-at-file-when-it-lacks-canonical-footer-line (contents)
  "Hint at file when it lacks the canonical footer line.
The canonical footer line is a comment featuring a sentence about end of file, like

    End of file `example.lisp'."
  (flet ((last-non-blank (lines)
	   (lastcar (remove "" lines :test #'string=))))
    (let ((actual-last-line
	    (last-non-blank (string-lines contents)))
	  (expected-last-line
	    (decorate-line-comment
	     *linter*
	     (format nil "End of file `~A'" (file-namestring *hint-pathname*))))
	  (regex
	    (list
	     :sequence
	     :start-anchor
	     (decorate-line-comment *linter* "End of file `")
	     (file-namestring *hint-pathname*)
	     "'"
	     :end-anchor)))
      (unless (ppcre:scan regex actual-last-line)
	(let ((hint
		(hint-at-file
		 "The file does not feature a canonical footer line."
		 "The last line of the file should be the expected canonical footer line

~A

but instead, this line is

~A

The automatic correction system guesses that the last line should be replaced
with the expected canonical footer line.~%" expected-last-line actual-last-line)))
	  (restart-case
	      (anomaly hint)
	    (autocorrect ()
	      :report "Accept the automatic correction guess"
	      (edit-last-line contents expected-last-line))
	    (continue ()
	      :report "Remember the anomaly as a hint and continue"
	      hint)))))))

;;;; End of file `codestyle-0004.lisp'
