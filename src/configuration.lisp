;;;; configuration.lisp — Configuration for the Atelier Lisp System

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

(defparameter *resourcedir* #.(or (uiop:getenv "ATELIER_RESOURCEDIR")
				  (when *compile-file-pathname*
				    (make-pathname
				     :directory
				     (append
				      (butlast (pathname-directory *compile-file-pathname*))
				      '("resource"))))
				  (when *load-pathname*
				    (make-pathname
				     :directory
				     (append
				      (butlast (pathname-directory *load-pathname*))
				      '("resource")))))
	      "The pathname to the directory holding program resources.")

;;;; End of file `configuration.lisp'
