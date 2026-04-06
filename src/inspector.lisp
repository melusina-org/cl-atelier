;;;; inspector.lisp — Inspector registry for Atelier

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:atelier)

(define-named-class inspector ()
  ((name
    :initarg :name
    :reader inspector-name
    :type symbol)
   (level
    :initarg :level
    :reader inspector-level
    :type (member :file :line :region :syntax))
   (description
    :initarg :description
    :reader inspector-description
    :type (or null string)
    :initform nil))
  (:name name))

(defun list-inspectors ()
  "Return a list of all registered inspector symbols."
  (loop :for name :being :the :hash-key :of *inspectors*
        :collect name))

;;;; End of file `inspector.lisp'
