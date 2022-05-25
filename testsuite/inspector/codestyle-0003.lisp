;;;; codestyle-0003.lisp — Hint at file when it lacks the canonical header line

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.atelier/testsuite)

(define-testcase validate-codestyle-0003-hint-at-file-when-it-lacks-canonical-header-line ()
  (let ((file-1
	  (atelier::join-lines
	   '("; testsuite.lisp -- The testsuite for Example"
	     ""
	     ";; End of file `testsuite.lsip'")))
	(file-2
	  (atelier::join-lines
	   '(";;;; testsuite.lisp — The testsuite for Example"
	     ""
	     ";; End of file `testsuite.lsip'")))
	(atelier::*linter*
	  (atelier::find-plain-linter :application/lisp)))
    (assert-string= file-2
		    (with-fixed-linter-environment
		      (atelier::hint-at-file-when-it-lacks-canonical-header-line file-1)))))

;;;; End of file `codestyle-0003.lisp'
