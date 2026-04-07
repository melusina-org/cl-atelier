;;;; configuration.lisp — Configuration for the Atelier Lisp System

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)

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
