;;;; codestyle-0001.lisp — Hint at files when they are not valid UTF8

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

(define-inspector :codestyle-0001 hint-at-file-when-character-encoding-is-not-utf8 (pathname)
  "Hint at PATHNAME when it is not a valid UTF8 file."
  (handler-case
      (and (read-file-into-string pathname :external-format :utf8) nil)
    (error ()
      (make-instance 'hint-at-file
		     :code *hint-code*
		     :pathname pathname
		     :description "The file contents is not valid UTF8."))))

;;;; End of file `codestyle-0001.lisp'
