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

(in-package #:org.melusina.atelier/testsuite)

(define-testcase validate-codestyle-0006-hint-at-file-when-it-lacks-project-license-information ()
  (let ((file-1
	  (atelier::join-lines
	   '("; testsuite.lisp -- The testsuite for Example"
	     ""
	     ";; Example (https://github.com/acme)"
	     ";;"
	     ";; Copyright 2021"
	     ";; All rights reserved"
	     ""
	     ";; This file must be used under the terms of"
	     ";; the  MIT License.  This  source  file  is"
	     ";; licensed as described in the file LICENSE,"
	     ";; which you should have received as part of"
	     ";; this distribution.    The terms  are also"
	     ";; available at https://opensource.org/licenses/MIT"
	     ""
	     ";; End of file `testsuite.lsip'")))
	(file-2
	  (atelier::join-lines
	   '("; testsuite.lisp -- The testsuite for Example"
	     ""
	     ";; Example (https://github.com/acme)"
	     ";;"
	     ";; Copyright 2021"
	     ";; All rights reserved"
	     ""
	     ";;;; This software is governed by the CeCILL-B license under French law and"
	     ";;;; abiding by the rules of distribution of free software.  You can  use,"
	     ";;;; modify and/ or redistribute the software under the terms of the CeCILL-B"
	     ";;;; license as circulated by CEA, CNRS and INRIA at the following URL"
	     ";;;; \"https://cecill.info/licences/Licence_CeCILL-B_V1-en.txt\""
	     ""
	     ";; End of file `testsuite.lsip'")))
	(atelier::*linter*
	  (atelier::find-plain-linter :application/lisp)))
    (assert-string= file-2
		    (with-fixed-linter-environment
		      (atelier::hint-at-file-when-it-lacks-project-license-information file-1)))))

;;;; End of file `codestyle-0006.lisp'
