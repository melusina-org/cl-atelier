;;;; list-systems.lisp — atelier:list-systems tool/resource

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(defun %asdf-source-registry-systems ()
  "Return an alist of (name source-file) for every system the ASDF
   source registry can find on disk. Does NOT load any system. Reads
   the internal source-registry hash-table after ensuring the registry
   is initialised."
  (asdf/source-registry:ensure-source-registry)
  (let ((registry (symbol-value
                   (find-symbol "*SOURCE-REGISTRY*" :asdf/source-registry)))
        (result nil))
    (when (hash-table-p registry)
      (loop :for name :being :the :hash-keys :of registry
              :using (:hash-value path)
            :do (push (list (cons "name" name)
                            (cons "source-file" (if path (namestring path) "")))
                      result)))
    (sort result #'string<
          :key (lambda (entry) (cdr (assoc "name" entry :test #'equal))))))

(define-tool list-systems ()
  (:description
   "List ASDF systems visible to the source registry, without loading any.
    Each entry has the system name and the absolute path to its .asd file.")
  (:resource :uri       "lisp://systems"
             :name      "ASDF systems"
             :mime-type :application/json)
  (%asdf-source-registry-systems))

;;;; End of file `list-systems.lisp'
