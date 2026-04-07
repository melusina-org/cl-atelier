;;;; check-file-encoding.lisp — UTF-8 encoding inspector

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)

(define-file-inspector check-file-encoding ((pathname pathname))
  "Check that source files are valid UTF-8.
Return an ENCODING-FINDING when PATHNAME is not valid UTF-8, or NIL."
  (handler-case
      (progn
        (read-file-into-string pathname :external-format :utf-8)
        nil)
    (error ()
      (list (make-instance 'encoding-finding
              :inspector 'check-file-encoding
              :severity :error
              :observation (format nil "File ~A is not valid UTF-8."
                                  (file-namestring pathname))
              :rationale "Non-UTF-8 files cause encoding errors in editors, CI, and compilation."
              :file pathname)))))

;;;; End of file `check-file-encoding.lisp'
