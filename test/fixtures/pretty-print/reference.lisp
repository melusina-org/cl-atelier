;;;; reference.lisp — Reference fixture for the pretty-printer

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:cl-user)


(defpackage #:pretty-printer-reference
  (:use #:common-lisp)
  (:import-from #:alexandria #:hash-table-keys #:make-keyword #:assoc-value)
  (:export #:*default-configuration*
           #:*configuration-search-path*
           #:+maximum-nesting-depth+
           #:configuration-key
           #:configuration-entry
           #:configuration-entry-key
           #:configuration-entry-value
           #:configuration-entry-documentation
           #:configuration-manager
           #:make-configuration-manager
           #:initialize-manager
           #:parse-configuration-value
           #:create-configuration-report
           #:with-configuration
           #:resolve-configuration-chain
           #:validate-configuration-entry
           #:dispatch-on-entry-type
           #:collect-active-entries
           #:format-entry-for-display
           #:apply-configuration-defaults
           #:coerce-entry-value))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun configuration-version ()
    "Return the configuration format version string.
This function is available at compile time for use in macros."
    "1.0.0"))


(defconstant +maximum-nesting-depth+
  16
  "Maximum depth of nested configuration includes.")


(defvar *default-configuration*
  (list :name "default" :version (configuration-version) :entries nil :metadata
        (list :created-by "system" :created-at "2026-01-01" :priority 0 :tags
              (list :builtin :readonly) :description
              "Default configuration with no overrides."))
  "The default configuration property list, used when no user configuration is found.")


(defparameter *configuration-search-path*
  (list #P"/etc/atelier/" #P"/usr/local/etc/atelier/"
        (merge-pathnames #P".config/atelier/" (user-homedir-pathname)) #P"./")
  "Ordered list of directories searched for configuration files.
The first match wins. Each entry is a pathname designator pointing
to a directory that may contain configuration files.")


(deftype configuration-key ()
  "A keyword symbol used as a configuration entry key."
  '(and keyword (not null)))


(defstruct
    (configuration-entry
      (:constructor make-configuration-entry
		    (&key key value documentation source priority))
      (:predicate configuration-entry-p) (:copier nil))
  "A single configuration entry with metadata."
  (key nil :type (or keyword null) :read-only t)
  (value nil :type t)
  (documentation "" :type string)
  (source "unknown" :type string :read-only t)
  (priority 0 :type fixnum))


(defclass configuration-manager nil
  ((name :initarg :name :reader configuration-manager-name :type string
         :documentation
         "Human-readable name for this configuration manager.")
   (entries :initarg :entries :accessor configuration-manager-entries
            :type hash-table :initform (make-hash-table :test 'eq)
            :documentation
            "Hash table mapping configuration keys to their entries.")
   (parent :initarg :parent :reader configuration-manager-parent :type
           (or configuration-manager null) :initform nil :documentation
           "Optional parent manager for hierarchical lookup.")
   (read-only-p :initarg :read-only-p :reader
		configuration-manager-read-only-p :type boolean :initform nil
		:documentation "When true, mutation operations signal an error.")
   (change-hooks :initarg :change-hooks :accessor
		 configuration-manager-change-hooks :type list :initform nil
		 :documentation
		 "List of functions called with (key old-value new-value)
on every successful SET operation.  Each hook is a function designator
accepting three arguments."))
  (:documentation "Manages a hierarchical set of configuration entries.
Supports parent-chain lookup, read-only enforcement, and change
notification hooks.  Entries are stored in a hash table keyed by
configuration keywords."))


(defgeneric initialize-manager
    (manager source)
  (:documentation "Populate MANAGER with entries from SOURCE.
SOURCE may be a pathname, a stream, or a property list.
Returns MANAGER.
Signals an error if SOURCE is not a recognised format."))


(defmethod initialize-manager
    ((manager configuration-manager) (source pathname))
  "Load configuration entries from a file at SOURCE into MANAGER."
  (with-open-file
      (stream source :direction :input :external-format :utf-8
	      :if-does-not-exist nil)
    (when stream
      (initialize-manager manager stream)))
  manager)


(defmethod initialize-manager ((manager configuration-manager) (source list))
  "Load configuration entries from a property list SOURCE into MANAGER."
  (loop :for (key value) :on source :by #'cddr
        :do (setf (gethash key (configuration-manager-entries manager))
                  (make-configuration-entry :key key :value value :source
					    "plist")))
  manager)


(defun make-configuration-manager
    (&rest initargs &key name entries parent read-only-p change-hooks)
  "Create and return a CONFIGURATION-MANAGER."
  (declare (ignore name entries parent read-only-p change-hooks))
  (apply #'make-instance 'configuration-manager initargs))


(defun parse-configuration-value (raw-value)
  "Parse RAW-VALUE and return a typed configuration value.
Strings that look like integers or floats are converted.
The literals \"true\" and \"false\" become T and NIL."
  (declare (type string raw-value)
           (values t))
  (cond ((string-equal raw-value "true") t)
        ((string-equal raw-value "false") nil)
        ((every #'digit-char-p raw-value) (parse-integer raw-value))
        (t raw-value)))


(defun create-configuration-report
    (manager
     &key (stream *standard-output*) (format :text) (include-defaults t)
       (include-documentation nil) (sort-by :key) (filter-fn nil)
       (max-entries most-positive-fixnum) (title "Configuration Report"))
  "Write a formatted report of all entries in MANAGER to STREAM.
FORMAT is one of :TEXT, :SEXP, or :TABLE.
When INCLUDE-DEFAULTS is true, entries from parent managers are included.
When INCLUDE-DOCUMENTATION is true, each entry's documentation string is shown.
SORT-BY is :KEY, :PRIORITY, or :SOURCE.
FILTER-FN, when non-nil, is a predicate on CONFIGURATION-ENTRY.
MAX-ENTRIES limits the number of reported entries.
TITLE is the report heading."
  (declare (type configuration-manager manager)
           (type stream stream)
           (type (member :text :sexp :table) format)
           (type boolean include-defaults include-documentation)
           (type (member :key :priority :source) sort-by)
           (type (or function null) filter-fn)
           (type fixnum max-entries)
           (type string title)
           (values null))
  (format stream "~&~A~%~A~%" title
          (make-string (length title) :initial-element #\=))
  (let* ((entries
          (collect-active-entries manager :include-defaults include-defaults
				  :filter-fn filter-fn))
         (sorted
          (sort (copy-list entries) #'string< :key
                (ecase sort-by
                  (:key (lambda (e) (symbol-name (configuration-entry-key e))))
                  (:priority
                   (lambda (e)
                     (princ-to-string (configuration-entry-priority e))))
                  (:source #'configuration-entry-source))))
         (limited (subseq sorted 0 (min max-entries (length sorted)))))
    (dolist (entry limited)
      (format-entry-for-display entry stream format include-documentation)))
  nil)


(defmacro with-configuration ((var manager-form) &body body)
  "Evaluate BODY with VAR bound to the configuration manager
returned by MANAGER-FORM.  Ensures the manager is properly
initialised before BODY executes."
  (let ((manager-sym (gensym "MANAGER")))
    `(let* ((,manager-sym ,manager-form) (,var ,manager-sym))
       (declare (type configuration-manager ,var))
       ,@body)))


(defun resolve-configuration-chain (manager key)
  "Look up KEY in MANAGER, walking the parent chain if not found locally.
Returns two values: the value and the manager that provided it.
Returns (VALUES NIL NIL) if KEY is not found anywhere in the chain."
  (declare (type configuration-manager manager)
           (type configuration-key key)
           (values t (or configuration-manager null)))
  (labels ((walk-chain (current-manager depth)
             (when (> depth +maximum-nesting-depth+)
               (error "Configuration parent chain exceeds maximum ~
                       nesting depth of ~D for key ~S."
                      +maximum-nesting-depth+ key))
             (let ((entry
                    (gethash key
                             (configuration-manager-entries current-manager))))
               (if entry
                   (values (configuration-entry-value entry) current-manager)
                   (let ((parent
                          (configuration-manager-parent current-manager)))
                     (when parent
                       (walk-chain parent (1+ depth))))))))
    (walk-chain manager 0)))


(defun validate-configuration-entry (entry)
  "Check that ENTRY has a valid key and a non-nil value.
Returns T if valid, NIL otherwise."
  (declare (type configuration-entry entry)
           (values boolean))
  (let* ((key (configuration-entry-key entry))
         (value (configuration-entry-value entry))
         (valid-key-p (and (keywordp key) (not (null key))))
         (valid-value-p (not (null value))))
    (and valid-key-p valid-value-p t)))


(defun dispatch-on-entry-type (entry)
  "Return a keyword describing the type of ENTRY's value."
  (declare (type configuration-entry entry)
           (values keyword))
  (let ((value (configuration-entry-value entry)))
    (typecase value
      (integer :integer)
      (float :float)
      (string :string)
      (keyword :keyword)
      (pathname :pathname)
      (list :list)
      (hash-table :hash-table)
      (t :other))))


(defun collect-active-entries
    (manager &key (include-defaults t) (filter-fn nil))
  "Return a list of all active configuration entries in MANAGER.
When INCLUDE-DEFAULTS is true, entries from parent managers are merged
with local entries taking precedence.  When FILTER-FN is non-nil,
only entries satisfying the predicate are included."
  (declare (type configuration-manager manager)
           (type boolean include-defaults)
           (type (or function null) filter-fn)
           (values list))
  (let ((result nil) (seen (make-hash-table :test 'eq)))
    (labels ((collect-from-manager (current-manager)
               (maphash
                (lambda (key entry)
                  (unless (gethash key seen)
                    (setf (gethash key seen) t)
                    (when (or (null filter-fn) (funcall filter-fn entry))
                      (push entry result))))
                (configuration-manager-entries current-manager))
               (when (and include-defaults
                          (configuration-manager-parent current-manager))
                 (collect-from-manager
                  (configuration-manager-parent current-manager)))))
      (collect-from-manager manager))
    (nreverse result)))


(defun format-entry-for-display (entry stream format include-documentation)
  "Write ENTRY to STREAM in the specified FORMAT.
FORMAT is one of :TEXT, :SEXP, or :TABLE.
When INCLUDE-DOCUMENTATION is true, the entry's documentation
string is included in the output."
  (declare (type configuration-entry entry)
           (type stream stream)
           (type (member :text :sexp :table) format)
           (type boolean include-documentation)
           (values null))
  (ecase format
    (:text
     (format stream "~&  ~A = ~S~%" (configuration-entry-key entry)
             (configuration-entry-value entry))
     (when include-documentation
       (unless (string= "" (configuration-entry-documentation entry))
         (format stream "    ~A~%"
                 (configuration-entry-documentation entry)))))
    (:sexp
     (format stream "~&  (~S ~S)" (configuration-entry-key entry)
             (configuration-entry-value entry)))
    (:table
     (format stream "~&| ~20A | ~30S | ~10A |" (configuration-entry-key entry)
             (configuration-entry-value entry)
             (configuration-entry-source entry))))
  nil)


(defun apply-configuration-defaults (manager defaults)
  "For each entry in DEFAULTS, set it in MANAGER only if the key
is not already present.  DEFAULTS is a property list of key-value
pairs.  Returns the number of defaults applied."
  (declare (type configuration-manager manager)
           (type list defaults)
           (values fixnum))
  (when (configuration-manager-read-only-p manager)
    (error "Cannot apply defaults: manager ~S is read-only."
           (configuration-manager-name manager)))
  (let ((count 0) (entries (configuration-manager-entries manager)))
    (loop :for (key value) :on defaults :by #'cddr
          :unless (gethash key entries)
          :do (setf (gethash key entries)
                    (make-configuration-entry :key key :value value :source
					      "defaults" :documentation "" :priority -1)) (incf
                    count))
    count))


(defun coerce-entry-value (entry target-type)
  "Coerce the value of ENTRY to TARGET-TYPE.
TARGET-TYPE is one of :STRING, :INTEGER, :KEYWORD, :PATHNAME, or :BOOLEAN.
Returns the coerced value.
Signals an error if the coercion is not possible."
  (declare (type configuration-entry entry)
           (type keyword target-type)
           (values t))
  (let ((value (configuration-entry-value entry)))
    (case target-type
      (:string
       (etypecase value
         (string value)
         (symbol (string-downcase (symbol-name value)))
         (integer (princ-to-string value))
         (pathname (namestring value))))
      (:integer
       (etypecase value (integer value) (string (parse-integer value))))
      (:keyword
       (etypecase value
         (keyword value)
         (string (intern (string-upcase value) :keyword))
         (symbol (intern (symbol-name value) :keyword))))
      (:pathname (etypecase value (pathname value) (string (pathname value))))
      (:boolean
       (case value
         ((t :true :yes 1) t)
         ((nil :false :no 0) nil)
         (t
          (if (stringp value)
              (not
               (member value '("false" "no" "0" "nil") :test #'string-equal))
              (not (null value))))))
      (t
       (error "Unknown target type ~S for coercion of ~S." target-type
              value)))))


(defun build-composite-report (managers title)
  "Build a single report string from multiple MANAGERS.
Each manager's entries are formatted and concatenated with a separator.
TITLE is the overall report heading."
  (declare (type list managers)
           (type string title)
           (values string))
  (flet ((format-manager-section (manager)
           (with-output-to-string (section-stream)
             (format section-stream "~&--- ~A ---~%"
                     (configuration-manager-name manager))
             (create-configuration-report manager :stream section-stream
					  :format :text :title "")))
         (build-separator (width)
           (concatenate 'string (string #\Newline)
                        (make-string width :initial-element #\-)
                        (string #\Newline))))
    (let ((separator (build-separator 72)))
      (concatenate 'string title separator
                   (reduce
                    (lambda (accumulated next)
                      (concatenate 'string accumulated separator next))
                    (mapcar #'format-manager-section managers) :initial-value
                    "")))))


(defun merge-configurations
    (primary secondary &key (conflict-resolution :primary-wins))
  "Merge two configuration managers into a new manager.
When keys conflict, CONFLICT-RESOLUTION determines the winner:
  :PRIMARY-WINS   — the entry from PRIMARY is kept.
  :SECONDARY-WINS — the entry from SECONDARY is kept.
  :HIGHER-PRIORITY — the entry with the higher priority is kept.
Returns a new CONFIGURATION-MANAGER."
  (declare (type configuration-manager primary secondary)
           (type (member :primary-wins :secondary-wins :higher-priority)
		 conflict-resolution)
           (values configuration-manager))
  (let ((merged
         (make-configuration-manager :name
				     (concatenate 'string (configuration-manager-name primary) "+"
						  (configuration-manager-name secondary)))))
    (flet ((merge-entry (key primary-entry secondary-entry)
             (cond ((null secondary-entry) primary-entry)
                   ((null primary-entry) secondary-entry)
                   (t
                    (ecase conflict-resolution
                      (:primary-wins primary-entry)
                      (:secondary-wins secondary-entry)
                      (:higher-priority
                       (if (>= (configuration-entry-priority primary-entry)
                               (configuration-entry-priority secondary-entry))
                           primary-entry
                           secondary-entry)))))))
      (let ((entries (configuration-manager-entries merged)))
        (maphash (lambda (key entry) (setf (gethash key entries) entry))
                 (configuration-manager-entries primary))
        (maphash
         (lambda (key entry)
           (let ((existing (gethash key entries)))
             (setf (gethash key entries) (merge-entry key existing entry))))
         (configuration-manager-entries secondary))))
    merged))


(defun handle-configuration-load-error (pathname handler)
  "Attempt to load configuration from PATHNAME.
If loading fails, invoke HANDLER with the condition and return NIL.
HANDLER receives two arguments: the condition and the pathname."
  (declare (type pathname pathname)
           (type function handler)
           (values (or configuration-manager null)))
  (let ((manager (make-configuration-manager :name (namestring pathname))))
    (handler-case (progn (initialize-manager manager pathname) manager)
      (file-error (condition) (funcall handler condition pathname)
                  nil)
      (end-of-file (condition) (funcall handler condition pathname)
                   nil))))


(defun find-configuration-in-search-path
    (
     &key (search-path *configuration-search-path*) (filename "config.sexp")
     (on-error :skip))
  "Search for a configuration file named FILENAME in each directory
of SEARCH-PATH.  Returns the first successfully loaded
CONFIGURATION-MANAGER, or NIL if none is found.

ON-ERROR controls behaviour when a configuration file exists but
cannot be loaded:
  :SKIP    — silently continue to the next directory.
  :WARN   — issue a warning and continue.
  :ERROR  — signal the original condition."
  (declare (type list search-path)
           (type string filename)
           (type (member :skip :warn :error) on-error)
           (values (or configuration-manager null)))
  (dolist (directory search-path)
    (let ((candidate (merge-pathnames filename directory)))
      (when (probe-file candidate)
        (restart-case (let ((manager
                             (make-configuration-manager :name
							 (namestring candidate))))
                        (initialize-manager manager candidate)
                        (return manager))
          (skip-configuration-file nil :report
				   (lambda (stream)
				     (format stream "Skip configuration file ~A ~
                                      and try the next directory."
					     candidate))
				   nil))))))
