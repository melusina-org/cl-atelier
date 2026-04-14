;;;; transcript.lisp — MCP session transcript subsystem

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

;;; Transcript writes are atomic per entry: PRIN1 + TERPRI +
;;; FINISH-OUTPUT, in that order. A SIGKILL between entries leaves a
;;; readable file; one between forms inside an entry leaves a torn
;;; tail that the reader skips. See INV-15 (proposed) and slice.md S6.

(defclass transcript ()
  ((session-id
    :initarg :session-id
    :reader transcript-session-id
    :type string)
   (path
    :initarg :path
    :reader transcript-path
    :type pathname)
   (next-seq
    :initform 1
    :accessor transcript-next-seq
    :type (integer 1)
    :documentation "Per-session monotonic sequence counter."))
  (:documentation "An open MCP session transcript."))

(defun %xdg-state-home-base ()
  "Return the user's XDG_STATE_HOME base directory as a pathname.
   Honours the XDG_STATE_HOME environment variable; defaults to
   ~/.local/state/. Mirrors the XDG Base Directory Specification.
   Provided locally because UIOP/CONFIGURATION:XDG-STATE-HOME is not
   exported by older UIOP versions."
  (let ((env (uiop:getenv "XDG_STATE_HOME")))
    (uiop:ensure-directory-pathname
     (if (and env (plusp (length env)))
         env
         (merge-pathnames ".local/state/" (user-homedir-pathname))))))

(defun %default-transcript-directory ()
  "Return the directory under which session transcript files live."
  (uiop:ensure-directory-pathname
   (merge-pathnames "atelier/mcp/transcripts/" (%xdg-state-home-base))))

(defun %generate-session-id ()
  "Return a fresh session id string. Universal-time + a random
   suffix is enough for slice 009 — sessions are sequential per
   process and the directory is per-user."
  (format nil "~36R-~36R" (get-universal-time) (random (expt 2 24))))

(defun make-transcript (&key
                          (session-id (%generate-session-id))
                          (directory (%default-transcript-directory)))
  "Create a TRANSCRIPT instance, ensuring its directory exists."
  (ensure-directories-exist directory)
  (make-instance 'transcript
                 :session-id session-id
                 :path (merge-pathnames
                        (concatenate 'string session-id ".sexp")
                        directory)))

(defmethod write-transcript-entry ((transcript transcript) entry)
  "Append ENTRY (a plist) to TRANSCRIPT, atomically per entry.
   The seq field is filled from the transcript counter if absent."
  (let* ((seq (incf (transcript-next-seq transcript)))
         (full-entry (list* :seq (1- seq)
                            :timestamp (%iso-timestamp)
                            entry)))
    (with-open-file (stream (transcript-path transcript)
                            :direction :output
                            :if-exists :append
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (let ((*print-readably* nil)
            (*print-pretty* nil))
        (prin1 full-entry stream))
      (terpri stream)
      (finish-output stream))
    full-entry))

(defun %iso-timestamp (&optional (universal-time (get-universal-time)))
  "Format UNIVERSAL-TIME as a basic ISO-8601 UTC string."
  (multiple-value-bind (sec min hr day mon year) (decode-universal-time universal-time 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year mon day hr min sec)))

(defun read-transcript (path)
  "Return the raw text of the transcript file at PATH as a string.
   Used for the lisp://transcript/{id}.sexp resource view."
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (with-output-to-string (out)
      (loop :for char := (read-char stream nil nil)
            :while char :do (write-char char out)))))

(defun read-transcript-entries (path)
  "Read PATH as a sequence of plist entries. Tolerates a torn tail:
   if the final line is incomplete, it is skipped without signalling."
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (loop :for line := (read-line stream nil nil)
          :while line
          :for entry := (%safe-read-entry line)
          :when entry :collect entry)))

(defun %safe-read-entry (line)
  "Try to READ-FROM-STRING LINE as a plist. Returns the plist on
   success, NIL on any read error or partial parse."
  (let ((line (string-trim '(#\Space #\Tab) line)))
    (when (and (> (length line) 0)
               (char= (char line 0) #\())
      (handler-case
          (let ((*read-eval* nil))
            (read-from-string line))
        (error () nil)))))

;;;; End of file `transcript.lisp'
