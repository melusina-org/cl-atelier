;;;; project.lisp — Projects for the Atelier Lisp System

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)

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
