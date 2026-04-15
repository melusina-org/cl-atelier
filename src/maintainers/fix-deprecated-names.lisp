;;;; fix-deprecated-names.lisp — Deprecated name maintainers

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier)


;;;;
;;;; Fix Deprecated System Name
;;;;

(define-automatic-maintainer fix-deprecated-system-name
    ((finding deprecated-system-name-finding))
  "Rename a deprecated system name in the .asd file.
Produces a text-resolution that replaces the deprecated name with the
canonical name at the finding's location. For example, replaces
my-project/testsuite with my-project/test."
  (let* ((observation (finding-observation finding))
         ;; Extract the deprecated system name from the observation.
         ;; Observation format: "System \"NAME\" uses deprecated suffix \"SUFFIX\"."
         (name-start (position #\" observation))
         (name-end (and name-start (position #\" observation :start (1+ name-start))))
         (deprecated-name (and name-start name-end
                               (subseq observation (1+ name-start) name-end))))
    (when deprecated-name
      (let* ((slash-pos (position #\/ deprecated-name))
             (main-part (and slash-pos (subseq deprecated-name 0 slash-pos)))
             (suffix (and slash-pos (subseq deprecated-name (1+ slash-pos))))
             (replacement-suffix (deprecated-suffix-replacement suffix))
             (canonical-name (and main-part replacement-suffix
                                  (concatenate 'string main-part "/" replacement-suffix))))
        (when canonical-name
          ;; The source-text is the defsystem line excerpt.
          ;; Replace just the name within it.
          (let* ((source (finding-source-text finding))
                 (new-source (cl-ppcre:regex-replace-all
                              (cl-ppcre:quote-meta-chars deprecated-name)
                              source canonical-name)))
            (make-text-resolution
             :maintainer 'fix-deprecated-system-name
             :finding finding
             :kind :automatic
             :description (format nil "Rename system ~A to ~A."
                                  deprecated-name canonical-name)
             :replacement new-source)))))))


;;;;
;;;; Fix Deprecated Component Name
;;;;

(define-automatic-maintainer fix-deprecated-component-name
    ((finding deprecated-component-name-finding))
  "Rename a deprecated component name in the .asd file.
Produces a text-resolution that replaces the deprecated component name
with the canonical name at the finding's location."
  (:maturity :experimental)
  (let* ((observation (finding-observation finding))
         ;; Extract deprecated and replacement names from the observation.
         ;; Observation format: "Component \"NAME\" should be renamed to \"REPLACEMENT\"."
         (name-start (position #\" observation))
         (name-end (and name-start (position #\" observation :start (1+ name-start))))
         (deprecated-name (and name-start name-end
                               (subseq observation (1+ name-start) name-end)))
         (replacement (and deprecated-name
                           (deprecated-component-replacement deprecated-name))))
    (when (and deprecated-name replacement)
      (let* ((source (finding-source-text finding))
             (new-source (cl-ppcre:regex-replace-all
                          (cl-ppcre:quote-meta-chars deprecated-name)
                          source replacement)))
        (make-text-resolution
         :maintainer 'fix-deprecated-component-name
         :finding finding
         :kind :automatic
         :description (format nil "Rename component ~A to ~A."
                              deprecated-name replacement)
         :replacement new-source)))))

;;;; End of file `fix-deprecated-names.lisp'
