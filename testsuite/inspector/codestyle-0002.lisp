;;;; codestyle-0002.lisp — Hint at very long lines

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:atelier/testsuite)

(define-testcase validate-codestyle-0002-hint-at-file-line-when-it-is-very-long ()
    (let ((short-line
	    "A very short line")
	  (very-long-line-with-words
	    (apply #'concatenate 'string (loop :repeat 50 :collect " abc")))
	  (very-long-line-without-words
	    (apply #'concatenate 'string (loop :repeat 50 :collect "abc"))))
      (assert-eq
       2
       (length
	(loop :for line :in (list short-line short-line short-line
				  very-long-line-with-words
				  very-long-line-with-words
				  very-long-line-without-words)
	      :for hint = (atelier::hint-at-file-line-when-it-is-very-long line)
	      :unless (null hint)
	      :collect hint)))))


;;;; End of file `codestyle-0002.lisp'
