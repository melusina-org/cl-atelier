;;;; setup.lisp — Setup for Atelier

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:cl-user)

;;;
;;; Atelier
;;;

(ql:quickload "org.melusina.atelier" :silent t)

(setf org.melusina.atelier:*parameter-bindings*
      '((:copyright-holder . "Michaël Le Barbier")
        (:copyright-year . "2017–2023")
	(:project-filename . "atelier")
        (:project-name . "Atelier")
	(:project-description . "Atelier for Lisp developers")
        (:project-long-description .
	 #.(concatenate 'string
	    "The atelier for Lisp developers is providing useful tools for Lisp developpers"
	    " such as project templates and a linter."))
        (:homepage . "https://github.com/melusina-org/cl-atelier")
        (:license . :mit)))

;;;; End of file `setup.lisp'
