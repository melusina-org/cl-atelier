;;;; transcript-resources.lisp — Three transcript resource views

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

;;; Three resource-only definitions exposing the same on-disk
;;; transcript file under different MIME types: sexp (raw), JSON
;;; (derived), Markdown (derived). The session id is a path parameter.

(defun %transcript-path-for (session-id)
  "Return the on-disk pathname for SESSION-ID's transcript file."
  (merge-pathnames
   (concatenate 'string session-id ".sexp")
   (%default-transcript-directory)))

(defun %read-transcript-or-error (session-id)
  "Return the parsed entries for SESSION-ID, or signal
   RESOURCE-NOT-FOUND when the file does not exist."
  (let ((path (%transcript-path-for session-id)))
    (unless (probe-file path)
      (error 'resource-not-found
             :uri (format nil "lisp://transcript/~A.sexp" session-id)
             :message (format nil "No transcript for session ~A." session-id)))
    (read-transcript-entries path)))

(define-tool transcript-sexp (&key session-id)
  (:description
   "Raw sexp transcript file for an MCP session, one plist per line.")
  (:resource :uri       "lisp://transcript/{session-id}.sexp"
             :name      "MCP session transcript (sexp)"
             :mime-type :text/plain)
  (:tool nil)
  (declare (type string session-id))
  (let ((path (%transcript-path-for session-id)))
    (unless (probe-file path)
      (error 'resource-not-found
             :uri (format nil "lisp://transcript/~A.sexp" session-id)
             :message (format nil "No transcript for session ~A." session-id)))
    (read-transcript path)))

(define-tool transcript-json (&key session-id)
  (:description
   "JSON-encoded view of an MCP session transcript, one entry per array element.")
  (:resource :uri       "lisp://transcript/{session-id}.json"
             :name      "MCP session transcript (JSON)"
             :mime-type :application/json)
  (:tool nil)
  (declare (type string session-id))
  (sexp-to-json-entries (%read-transcript-or-error session-id)))

(define-tool transcript-markdown (&key session-id)
  (:description
   "Markdown rendering of an MCP session transcript, one section per entry.")
  (:resource :uri       "lisp://transcript/{session-id}.md"
             :name      "MCP session transcript (Markdown)"
             :mime-type :text/markdown)
  (:tool nil)
  (declare (type string session-id))
  (sexp-to-markdown-entries (%read-transcript-or-error session-id)
                            :session-id session-id))

;;;; End of file `transcript-resources.lisp'
