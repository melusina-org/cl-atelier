;;;; check-file-encoding.lisp — UTF-8 encoding inspector

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

(define-file-inspector check-file-encoding
    ((pathname pathname) project-configuration)
  "Check that source files are valid UTF-8.
Return an ENCODING-FINDING when PATHNAME is not valid UTF-8, or NIL."
  (declare (ignore inspector project-configuration))
  (handler-case
      (progn
        (read-file-into-string pathname :external-format :utf-8)
        nil)
    (error ()
      (list (make-file-finding
              :inspector 'check-file-encoding
              :severity :error
              :observation (format nil "File ~A is not valid UTF-8."
                                  (file-namestring pathname))
              :rationale "Non-UTF-8 files cause encoding errors in editors, CI, and compilation."
              :file pathname)))))

;;;; End of file `check-file-encoding.lisp'
