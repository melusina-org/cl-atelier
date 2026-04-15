;;;; main.lisp — Entrypoints for the Atelier Lisp System

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)

(defun initialized-p ()
  (and (gethash :mit *license-repository*)
       (gethash 'license *template-repository*)
       t))

(defun initialize ()
  (license-repository-load)
  (template-repository-load)
  (values))

;;;; End of file `main.lisp'
