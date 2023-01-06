;;;; project.lisp — Projects for the Atelier Lisp System

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

(defparameter *project* nil
  "The default project for atelier operations.")

(defclass project nil
  ((project-name
    :initarg 
   project-homepage
   project-pathname
   project-copyright
   project-license))

;;;; End of file `project.lisp'
