;;;; probe-environment.lisp — atelier:probe-environment tool

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(define-tool probe-environment ()
  (:description
   "Report the Lisp implementation, machine, and software environment.
    Lets an MCP client identify the running image with one call.")
  (list (cons "lisp-implementation-type"    (or (lisp-implementation-type)    ""))
        (cons "lisp-implementation-version" (or (lisp-implementation-version) ""))
        (cons "machine-instance"            (or (machine-instance)            ""))
        (cons "machine-type"                (or (machine-type)                ""))
        (cons "machine-version"             (or (machine-version)             ""))
        (cons "software-type"               (or (software-type)               ""))
        (cons "software-version"            (or (software-version)            ""))
        (cons "short-site-name"             (or (short-site-name)             ""))
        (cons "long-site-name"              (or (long-site-name)              ""))))

;;;; End of file `probe-environment.lisp'
