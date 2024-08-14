;;;; lint.lisp — Linter facilities for Atelier

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:atelier)

(defparameter *linter-interactive-p*
  (and (position :swank *features*) t)
  "Flag governing the interactive mode of the linter.
When the flag is a generalised boolean, the linter interactions are enabled
otherwise they are disabled.

When linter interactions are enabled, the system gives the operator the chance
to accept corrections guessed by the automatic correction system, if any. Should
the operator not take this chance, the anomaly is recorded into the linter report.

When the flag is NIL, anomalies are always recorded into the linter report and
the linter is exiting the program when done.

The default value of the parameter is based on the :SWANK feature.")

(defvar *inspector-table* (make-hash-table)
  "The table of inspector corresponding to their codes.")

(defvar *linter-table* (make-hash-table)
  "The table of linters corresponding to their file types.")

(defparameter *linter* nil
  "The current linter used.")

(defparameter *hint-code* nil
  "The hint code locally bound by the DEFINE-HINT macro.")

(defparameter *hint-pathname* nil
  "The relative pathname of the file being linted.")

(defparameter *hint-srcdir* nil
  "The source directory for the project being linted.")

(defparameter *hint-line* nil
  "The number of the line being hinted at.")

;;;
;;; Hints
;;;

(defclass hint nil
  ((code
    :initarg :code
    :initform (error "A hint must have a unique hint code used as a reference.")
    :documentation "The hint code uniquely defines a hint.")
   (description
    :initarg :description
    :initform (error "A hint must have a hint description.")
    :documentation "A short description of the hint.")
   (explanation
    :initarg :explanation
    :initform nil
    :documentation "A longer explanation describing the hint."))
  (:documentation "The hint class represent hints."))

(defclass hint-at-file (hint)
  ((pathname
    :initarg :pathname
    :initform nil
    :documentation "The relative pathname of the affected file."))
  (:documentation "The hint class represents hints that are affecting a file."))

(defun hint-at-file (description &optional control-string &rest format-arguments)
  "Make a hint CODE for PATHNAME stating a description.
The description is prepared by formatting CONTROL-STRING and FORMAT-ARGUMENTS."
  (make-instance 'hint-at-file
		 :code *hint-code*
		 :pathname *hint-pathname*
		 :description description
		 :explanation (when control-string
				(apply #'format nil control-string format-arguments))))

(defclass hint-at-file-line (hint)
  ((pathname
    :initarg :pathname
    :initform nil
    :documentation "The relative pathname of the affected file.")
   (line
    :initarg :line
    :initform nil
    :documentation "The line number withing the affected file."))
  (:documentation "The hint class represent hints that are affecting a spcific line in a file."))

(defun hint-at-file-line (description &optional control-string &rest format-arguments)
  "Make a hint CODE for PATHNAME and LINE stating a description.
The description is prepared by formatting CONTROL-STRING and FORMAT-ARGUMENTS."
  (make-instance 'hint-at-file-line
		 :code *hint-code*
		 :pathname *hint-pathname*
		 :line *hint-line*
		 :description description
		 :explanation (when control-string
				(apply #'format nil control-string format-arguments))))


;;;;
;;;; Anomaly Condition
;;;;

(define-condition anomaly nil
  ((code
    :initarg :code
    :initform *hint-code*
    :documentation "The code code of an anomaly.")
   (description
    :initarg :description
    :initform (error "An anomaly requires a description.")
    :documentation "A short, generic explanation of an anomaly.")
   (explanation
    :initarg :explanation
    :initform nil
    :documentation "A longer, context-specific explanation of an anomaly."))
  (:documentation
   "The class of anomalies spotted by inspectors.")
  (:report (lambda (anomaly stream)
	     (with-slots (description explanation) anomaly
	       (write-string description stream)
	       (write-char #\Newline stream)
	       (write-string explanation stream)))))

 (defun anomaly (description &optional control-string &rest format-arguments)
  "Signal an anomaly with the given parameters."
  (cond
    ((typep description 'hint)
     (with-slots (code description explanation) description
       (error
	'anomaly
	:code code
	:description description
	:explanation explanation)))
    (t
     (error
      'anomaly
      :code *hint-code*
      :description description
      :explanation (when control-string
		     (apply #'format nil control-string format-arguments))))))


;;;
;;; Inspector database
;;;

(defmacro define-inspector (code name lambda-list &body body)
  "Define inspector NAME emitting hint codes CODE.
The inspector NAME is a function with the given LAMBDA-LIST
and BODY.  The inspector is called with a PATHNAME when
it is a file inspector, with a file CONTENTS when it is
a contents inspector, with file LINES when it is a line
inspector or with CODE when it is a CODE inspector."
  (let ((docstring
	  (when (stringp (first body))
	    (list (first body))))
	(actual-body
	  (if (stringp (first body))
	      (rest body)
	      body)))
    (unless (keywordp code)
      (error "~S: The code designating this inspector must be a keyword." code))
    `(progn
       (setf (gethash ,code *inspector-table*) (quote ,name))
       (defun ,name ,lambda-list
	 ,@docstring
	 (let ((*hint-code* ,code))
	   ,@actual-body)))))

(defun find-inspector (inspector-designator)
  "Find the inspector designated by INSPECTOR-DESIGNATOR."
  (cond
    ((functionp inspector-designator)
     inspector-designator)
    ((keywordp inspector-designator)
     (gethash inspector-designator *inspector-table*))
    ((symbolp inspector-designator)
     (symbol-function inspector-designator))
    (t
     (error "Not sure how to find inspector designated by ~S" inspector-designator))))

(defun list-inspectors ()
  "List INSPECTORS."
  (loop :for code
	:being :the :hash-key
	:using (hash-value inspector) :of *inspector-table*
	:collect (list
		  :code code
		  :inspector inspector
		  :description (first-line (documentation inspector 'function)))))


;;;;
;;;; Linter
;;;;

(defclass linter nil
  ((file-type
    :initarg :file-type
    :initform nil
    :documentation "File types which are subject to the hints owned by the structure.")
   (file-predicate
    :initarg :file-predicate
    :initform t
    :documentation "The FILE-PREDICATE recognises the file of the given FILE-TYPE.
The FILE-PREDICATE can have one of the following forms:

  * T
     The predicate that recognises every file.
  * NIL
     The predicate that recognises no file.
  * (:HAS-PREFIX STRING)
     The predicate that recognises files whose name starts with STRING.
  * (:HAS-SUFFIX STRING)
     The predicate that recognises files whose name ends with STRING.
  * (:HAS-NAME STRING)
     The predicate that recognises files whose name is STRING.
  * (:HAS-SHEBANG STRING)
     The predicate that recognises files with a SHEBANG similar to STRING.
  * (:OR PREDICATE1 …)
     The disjunction of predicates.
  * (:AND PREDICATE1 …)
     The conjunction of predicates."))
  (:documentation
   "The linter class represents a set of hints and a filter selecting files
subject to these hints."))

(defgeneric file-inspectors (linter)
  (:documentation "Inspectors emitting hints applying to the file.
These inspectors are called with the PATHNAME of the file. They are a good place
to validate the file encoding, or file properties.")
  (:method-combination append)
  (:method append ((instance linter))
    nil))

(defgeneric content-inspectors (linter)
  (:documentation "Inspectors emitting hints applying to the file contents.
These inspectors are called with CONTENTS of the file, a string. They are a good place
to validate the presence of fixed texts or the absence of authentication tokens and
the like.")
  (:method-combination append)
  (:method append ((instance linter))
    nil))

(defgeneric line-inspectors (linter)
  (:documentation "Inspectors emitting hints applying to individual lines of a file.
The inspectors are called on a LINE of the file, a string.  They are a good place
to strip whitespace, detect long lines, etc.")
  (:method-combination append)
  (:method append ((instance linter))
    nil))

(defgeneric decorate-line-commment (linter line-comment)
  (:documentation "Decorate a LINE-COMMENT for LINTER.
This decorates LINE-COMMENT according to FILE-TYPE so that LINE-COMMENT can
be inserted as a standalone comment line in a source code file of the corresponding
FILE-TYPE."))

(defmethod decorate-line-comment :before (linter (line-comment string))
  "Reject lines which contain a newline character."
  (if (position #\Newline line-comment)
      (error "A LINE-COMMENT cannot contain newline characters.")))

(defgeneric decorate-block-comment (linter block-comment)
  (:documentation "Decorate a BLOCK-COMMENT for LINTER.
This decorates BLOCK-COMMENT according to FILE-TYPE so that BLOCK-COMMENT can
be inserted as a standalone comment block in a source code file of the corresponding
FILE-TYPE.

A BLOCK-COMMENT can either be a string or a list of strings.")
  (:method (instance (block-comment string))
    (decorate-block-comment instance (string-lines block-comment)))
  (:method (instance (block-comment list))
    (unless (every #'stringp block-comment)
      (error "A BLOCK-COMMENT list must be a list of strings."))
    (join-lines
     (loop :for line :in block-comment
	   :collect (decorate-line-comment instance line)))))


;;;;
;;;; Extensive Linter
;;;;

(defclass extensive-linter (linter)
  ((file-inspectors
    :initarg :file-inspectors
    :initform nil
    :documentation "The extensive list of FILE-INSPECTORS selected by this LINTER.")
   (content-inspectors
    :initarg :content-inspectors
    :initform nil
    :documentation "The extensive list of CONTENT-INSPECTORS selected by this LINTER.")
   (line-inspectors
    :initarg :line-inspectors
    :initform nil
    :documentation "The extensive list of LINE-INSPECTORS selected by this LINTER."))
  (:documentation
   "The EXTENSIVE-LINTER class represents an extensive set of INSPECTORS."))

(defmethod file-inspectors append ((instance extensive-linter))
  (slot-value instance 'file-inspectors))

(defmethod content-inspectors append ((instance extensive-linter))
  (slot-value instance 'content-inspectors))

(defmethod line-inspectors append ((instance extensive-linter))
  (slot-value instance 'line-inspectors))


;;;;
;;;; Canonical Source Linter
;;;;

(defclass canonical-source-linter (linter)
  nil
  (:documentation
   "A SOURCE-LINTER verrifies that a file follows canonical project rules."))

(defmethod file-inspectors append ((instance canonical-source-linter))
  '(hint-at-file-when-character-encoding-is-not-utf8))

(defmethod content-inspectors append ((instance canonical-source-linter))
  '(hint-at-file-when-it-lacks-canonical-header-line
    hint-at-file-when-it-lacks-canonical-footer-line
    hint-at-file-when-it-lacks-canonical-project-identification
    hint-at-file-when-it-lacks-project-license-information))


;;;;
;;;; Inline Comment Linter
;;;;

(defclass inline-comment-linter (linter)
  ((comment-prefix
    :initarg :comment-prefix
    :initform (error "An INLINE-COMMENT-LINTER requires a COMMENT-PREFIX."))
   (comment-prefix-regex
    :initarg :comment-prefix-regex
    :initform (error "An INLINE-COMMENT-LINTER requires a COMMENT-PREFIX-REGEX."))))

(defmethod decorate-line-comment ((instance inline-comment-linter) line)
  (string-trim '(#\Space) (concatenate 'string (slot-value instance 'comment-prefix) line)))

(defun comment-prefix-regex (linter)
  (slot-value linter 'comment-prefix-regex))


;;;;
;;;; Block Comment Linter
;;;;

(defclass block-comment-linter (linter)
  ((comment-prefix
    :initarg :comment-prefix
    :initform (error "An BLOCK-COMMENT-LINTER requires a COMMENT-PREFIX."))
   (comment-prefix-regex
    :initarg :comment-prefix-regex
    :initform (error "An BLOCK-COMMENT-LINTER requires a COMMENT-PREFIX-REGEX."))
   (comment-continuation
    :initarg :comment-continuation
    :initform (error "An BLOCK-COMMENT-LINTER requires a COMMENT-CONTINUATION."))
   (comment-suffix
    :initarg :comment-suffix
    :initform (error "An BLOCK-COMMENT-LINTER requires a COMMENT-SUFFIX."))))

(defmethod decorate-block-comment ((instance block-comment-linter) (block-comment list))
  (with-output-to-string (buffer)
    (with-slots (comment-prefix comment-suffix comment-continuation) instance
      (labels
	  ((write-first-line (line)
	     (write-string comment-prefix buffer)
	     (write-string line buffer))
	   (write-last-line (line)
	     (write-string comment-continuation buffer)
	     (write-string line buffer)
	     (write-string comment-suffix buffer))
	   (write-other-line (line)
	     (write-string comment-continuation buffer)
	     (write-string line buffer))
	   (write-block-comment (first-line other-lines last-line)
	     (when first-line
	       (write-first-line first-line))
	     (when other-lines
	       (mapc #'write-other-line other-lines))
	     (when last-line
	       (write-last-line last-line)))
	   (write-single-line (single-line)
	     (write-string comment-prefix buffer)
	     (write-string single-line buffer)
	     (write-string comment-suffix buffer)))
	(if (< 1 (length block-comment))
	    (write-block-comment
	     (first block-comment)
	     (butlast (rest block-comment))
	     (lastcar block-comment))
	    (write-single-line (first block-comment)))))))


;;;;
;;;; Plain Linters
;;;;

(defclass plain-line-comment-linter (extensive-linter
				     inline-comment-linter
				     canonical-source-linter)
  nil
  (:documentation
   "The class of linters for languages whose comments follow the line comment style."))

(defun make-plain-line-comment-linter (&rest initargs &key file-predicate file-type
							   comment-prefix comment-prefix-regex
							   file-inspectors content-inspectors line-inspectors)
  "Make an inline comment linter."
  (declare (ignore file-predicate file-type
		   comment-prefix comment-prefix-regex
		   file-inspectors content-inspectors line-inspectors))
  (apply #'make-instance 'plain-line-comment-linter initargs))

(defclass plain-block-comment-linter (extensive-linter
				      block-comment-linter
				      canonical-source-linter)
  nil
  (:documentation
   "The class of linters for languages whose comments follow the block comment style."))

(defun make-plain-block-comment-linter (&rest initargs &key file-predicate file-type
							    comment-prefix comment-prefix-regex
							    comment-continuation comment-suffix
							    file-inspectors content-inspectors line-inspectors)
  "Make a block comment linter."
  (declare (ignore file-predicate file-type
		   comment-prefix comment-prefix-regex
		   comment-continuation comment-suffix
		   file-inspectors content-inspectors line-inspectors))
  (apply #'make-instance 'plain-block-comment-linter initargs))

(defun define-plain-linter (&rest initargs &key file-predicate file-type
						comment-prefix comment-prefix-regex
						comment-continuation comment-suffix
						file-inspectors content-inspectors line-inspectors)
  "Define a plain linter from either style and add it to the linter table."
  (declare (ignore file-predicate
		   comment-prefix comment-prefix-regex
		   file-inspectors content-inspectors line-inspectors))
  (let ((linter
	  (cond
	    ((and comment-continuation comment-suffix)
	     (apply #'make-plain-block-comment-linter initargs))
	    (t
	     (apply #'make-plain-line-comment-linter initargs)))))
    (setf (gethash file-type *linter-table*) linter)
    linter))

(defun find-plain-linter (designator)
  "Find a plain linter by its DESIGNATOR.
The designator is a keyword representing the filetype the linter wants to take
care of."
  (gethash designator *linter-table*))


;;;;
;;;; Basic Set of Plain Linters
;;;;

(define-plain-linter
  :file-type :application/lisp
  :file-predicate '(:or
		    (:has-suffix ".lisp")
		    (:has-suffix ".asd")
		    (:has-suffix ".asdf"))
  :comment-prefix ";;;; "
  :comment-prefix-regex '(:sequence
			  (:greedy-repetition 1 nil #\Semicolon)
			  (:greedy-repetition 0 nil #\Space))
  :line-inspectors '(hint-at-file-line-when-it-is-very-long))

(define-plain-linter
  :file-type :application/shellscript
  :file-predicate '(:or
		    (:has-suffix ".sh")
		    (:has-suffix ".zsh")
		    (:has-suffix ".ksh")
		    (:has-suffix ".bash")
		    (:has-suffix ".sh.in")
		    (:has-suffix ".zsh.in")
		    (:has-suffix ".ksh.in")
		    (:has-suffix ".bash.in")
		    (:has-name "configure.ac")
		    (:has-shebang "*/bin/sh*"))
  :comment-prefix "# "
  :comment-prefix-regex '(:sequence
			  (:greedy-repetition 1 nil #\Number_sign)
			  (:greedy-repetition 0 nil #\Space))
  :line-inspectors '(hint-at-file-line-when-it-is-very-long))

(define-plain-linter
  :file-type :application/makefile
  :file-predicate '(:or
		    (:has-name "Makefile")
		    (:has-name "Makefile.in")
		    (:has-name "Makefile.config.in")
		    (:has-prefix "Makefile.")
		    (:has-suffix ".mk")
		    (:has-suffix ".mk.in"))
  :comment-prefix "# "
  :comment-prefix-regex '(:sequence
			  (:greedy-repetition 1 nil #\Number_sign)
			  (:greedy-repetition 0 nil #\Space))
  :line-inspectors '(hint-at-file-line-when-it-is-very-long))

(define-plain-linter
  :file-type :application/dockerfile
  :file-predicate '(:or
		    (:has-name "Dockerfile")
		    (:has-prefix "Dockerfile.")
		    (:has-suffix ".dockerfile"))
  :comment-prefix "# "
  :comment-prefix-regex '(:sequence
			  (:greedy-repetition 1 nil #\Number_sign)
			  (:greedy-repetition 0 nil #\Space))
  :line-inspectors '(hint-at-file-line-when-it-is-very-long))

(define-plain-linter
  :file-type :application/tex
  :file-predicate '(:or
		    (:has-suffix ".tex")
		    (:has-suffix ".cls")
		    (:has-suffix ".mac")
		    (:has-suffix ".sty"))
  :comment-prefix "% "
  :comment-prefix-regex '(:sequence
			  (:greedy-repetition 1 nil #\Percent_sign)
			  (:greedy-repetition 0 nil #\Space))
  :line-inspectors '(hint-at-file-line-when-it-is-very-long))

(define-plain-linter
  :file-type :application/mpost
  :file-predicate '(:or
		    (:has-suffix ".mp")
		    (:has-suffix ".mpost"))
  :comment-prefix "% "
  :comment-prefix-regex '(:sequence
			  (:greedy-repetition 1 nil #\Percent_sign)
			  (:greedy-repetition 0 nil #\Space))
  :line-inspectors '(hint-at-file-line-when-it-is-very-long))


;;;;
;;;; Lint File
;;;;

(defun lint-file (pathname inspectors)
  "Ensure the file CONTENTS under PATHNAME validates some file contents with INSPECTORS."
  (loop :with hints = nil
	:for inspector :in inspectors
	:for current-hints = (funcall inspector pathname)
	:do (cond
	      ((eq nil current-hints)
	       nil)
	      ((typep current-hints 'hint)
	       (setf hints (nconc hints (list current-hints))))
	      ((and (listp current-hints)
		    (every (lambda (item) (typep item 'hint)) current-hints))
	       (setf hints (nconc hints current-hints)))
	      (t
	       (error "Cannot handle contents inspector answer ~S" current-hints)))
	:finally (return hints)))


;;;;
;;;; Lint Contents
;;;;

(defun lint-contents (contents inspectors)
  "Ensure the file CONTENTS under PATHNAME validates some file contents with INSPECTORS."
  (loop :with contents = contents
	:with hints = nil
	:for inspector :in inspectors
	:for hint-or-contents = (funcall inspector contents)
	:do (cond
	      ((eq nil hint-or-contents)
	       nil)
	      ((stringp hint-or-contents)
	       (setf contents hint-or-contents))
	      ((typep hint-or-contents 'hint)
	       (setf hints (nconc hints (list hint-or-contents))))
	      ((and (listp hint-or-contents)
		    (every (lambda (item) (typep item 'hint)) hint-or-contents))
	       (setf hints (nconc hints hint-or-contents)))
	      (t
	       (error "Cannot handle contents inspector answer ~S" hint-or-contents)))
	:finally (return (values hints contents))))


;;;;
;;;; Lint Lines
;;;;

(defun lint-lines (lines inspectors)
  "Ensure the file LINES under PATHNAME validate some line INSPECTORS.
When CONTENTS is supplied, it is used as the content of the file
instead of reading the file contents from PATHNAME. Valid values
for CONTENTS are a string or a list of strings."
  (flet ((lint-1 (lines inspector)
	   (loop :for *hint-line* = 1 :then (1+ *hint-line*)
		 :with lines = lines
		 :with hints = nil
		 :for tail :on lines
		 :for hint-or-line = (funcall inspector (first tail))
		 :do (cond
		       ((eq nil hint-or-line)
			nil)
		       ((stringp hint-or-line)
			(setf (first tail) hint-or-line))
		       ((typep hint-or-line 'hint)
			(setf hints (nconc hints (list hint-or-line))))
		       ((and (listp hint-or-line)
			     (every (lambda (item) (typep item 'hint)) hint-or-line))
			(setf hints (nconc hints hint-or-line)))
		       (t
			(error "Cannot handle line inspector answer ~S" hint-or-line)))
		 :finally (return (values hints lines)))))
    (loop :with lines = lines
	  :with hints = nil
	  :for inspector :in inspectors
	  :do (multiple-value-bind (current-hints current-lines)
		  (lint-1 lines inspector)
		(setf lines current-lines)
		(setf hints (nconc hints current-hints)))
	  :finally (return (values hints lines)))))


;;;;
;;;; Arrange Hints
;;;;

(defun arrange-hints-by-key (key hints)
  "Arrange HINTS into an alist mapping KEYS to their list of HINTS."
  (let ((buffer
	  (make-hash-table))
	(answer
	  nil))
    (loop for hint in hints
	  do (with-slots (pathname) hint
	       (push hint (gethash (funcall key hint) buffer nil))))
    (maphash #'(lambda (key value)
		 (push (cons key value) answer))
	     buffer)
    answer))


(defun arrange-hints-by-file (hints)
  "Arrange HINTS into an alist mapping PATHNAMES to their list of HINTS."
  (arrange-hints-by-key
   #'(lambda (hint) (slot-value hint 'pathname)) hints))

(defun arrange-hints-by-code (hints)
  "Arrange HINTS into an alist mapping CODES to their list of HINTS."
  (arrange-hints-by-key
   #'(lambda (hint) (slot-value hint 'code)) hints))

(defun arrange-hints-by-file-then-code (hints)
  (mapcar #'(lambda (assoc-cell)
	      (cons (car assoc-cell) (arrange-hints-by-code (cdr assoc-cell))))
	  (arrange-hints-by-file hints)))

(defun format-hints-by-file-then-code (destination hints)
  (flet ((format-hint (destination hint)
	   (with-slots (description explanation) hint
	     (format destination "    ~A~%" description)
	     (when explanation
	       (format destination "~A~%" (indent explanation 4))))))
  (loop for (pathname . hints-by-code) in (arrange-hints-by-file-then-code hints)
	do (format destination "Hints for ~A:~%" (namestring pathname))
	do (loop for (code . hints) in hints-by-code
		 do (format destination "  Hint ~A:~%" code)
		 do (loop for hint in hints
			  do (format-hint destination hint))))))


;;;;
;;;; Lint
;;;;

(defun linter-match (linter pathname)
  "Predicate recognising if a LINTER applies to PATHNAME."
  (labels ((has-suffix (expr)
	     (unless (= 1 (length expr))
	       (error "A :HAS-SUFFIX clause must have one argument."))
	     (unless (stringp (first expr))
	       (error "A :HAS-SUFFIX clause must have a string agument."))
	     (string-suffix-p (first expr) (namestring pathname)))
	   (has-prefix (expr)
	     (unless (= 1 (length expr))
	       (error "A :HAS-PREFIX clause must have one argument."))
	     (unless (stringp (first expr))
	       (error "A :HAS-PREFIX clause must have a string agument."))
	     (string-prefix-p (first expr) (namestring pathname)))
	   (has-name (expr)
	     (unless (= 1 (length expr))
	       (error "A :HAS-NAME clause must have one argument."))
	     (unless (stringp (first expr))
	       (error "A :HAS-NAME clause must have a string agument."))
	     (string= (first expr) (namestring pathname)))
	   (has-shebang (expr)
	     (let ((first-line
		     (first (read-file-into-list pathname))))
	       (unless (= 1 (length expr))
		 (error "A :HAS-SUFFIX clause must have one argument."))
	       (unless (stringp (first expr))
		 (error "A :HAS-SUFFIX clause must have a string agument."))
	       (and (string-match "#!*" first-line)
		    (string-match (first expr) (subseq first-line 2)))))
	   (eval-predicate (expr)
	     (ecase (first expr)
	       (:or
		(some #'eval-predicate (rest expr)))
	       (:and
		(every #'eval-predicate (rest expr)))
	       (:has-name
		(has-name (rest expr)))
	       (:has-suffix
		(has-suffix (rest expr)))
	       (:has-prefix
		(has-prefix (rest expr)))
	       (:has-shebang
		(has-shebang (rest expr)))
	       (nil
		nil)
	       (t
		t))))
    (eval-predicate (slot-value linter 'file-predicate))))

(defun linter-for-file (pathname)
  "Find a linter to apply for PATHNAME."
  (loop :for file-type
	:being :the :hash-key
	:using (hash-value linter) :of *linter-table*
	:when (linter-match linter pathname)
	:return linter))
  
(defun lint (&rest pathnames)
  "Lint file PATHNAMES with the given linters.

When the first item of PATHNAMES is a list itself, then the linter
is called on pathnames on that list."
  (when (template-repository-empty-p)
    (initialize))
  (when (listp (first pathnames))
    (return-from lint
      (apply #'lint (first pathnames))))
  (labels ((finalize (hints lines)
	     (values hints (join-lines lines)))
	   (lint-1 (pathname)
	     (let ((*hint-pathname*
		     pathname)
		   (*linter*
		     (linter-for-file pathname))
		   (hints
		     nil)
		   (original-contents
		     nil)
		   (file-contents
		     nil))
	       (when *linter*
		 (setf original-contents
		       (read-file-into-string pathname))
		 (setf file-contents
		       (copy-seq original-contents))
		 (multiple-value-bind (current-hints)
		     (lint-file pathname (file-inspectors *linter*))
		   (setf hints (nconc hints current-hints)))
		 (multiple-value-bind (current-hints current-contents)
		     (lint-contents file-contents (content-inspectors *linter*))
		   (setf hints (nconc hints current-hints)
			 file-contents (string-lines current-contents)))
		 (multiple-value-bind (current-hints current-contents)
		     (lint-lines file-contents (line-inspectors *linter*))
		   (setf hints (nconc hints current-hints)
			 file-contents (join-lines current-contents))))
	       (when (and file-contents (not (string= file-contents original-contents)))
		 (with-open-file (stream pathname :direction :output :if-exists :supersede)
		   (write-string file-contents stream)))
	       hints))
	   (handler-lint-1 (pathname)
	     (cond
	       (*linter-interactive-p*
		(lint-1 pathname))
	       (t
		(handler-bind
		    ((anomaly
		       (lambda (c)
			 (declare (ignore c))
			 (invoke-restart 'continue)))
		     (simple-error
		       (lambda (c)
			 (format *error-output* "~&Failure: lint: ~A" pathname)
			 (describe c *error-output*)
			 (uiop:quit 1))))
		  (lint-1 pathname)))))
	   (epilogue (hints)
	     (format-hints-by-file-then-code t hints)
	     (cond
	       (*linter-interactive-p*
		(not hints))
	       (t
		(uiop:quit (if hints 1 0))))))
    (let ((hints nil))
      (rashell:do-find (pathname ('(:or
				    (:and (:name ".DS_Store") :prune)
				    (:and (:name ".git") :prune)
				    (:and (:name ".hg") :prune)
				    (:and (:name ".svn") :prune)
				    (:and (:name "CVS") :prune)
				    (:and (:name "*.fasl") :prune)
				    (:and (:has-kind :regular) :print))
				  pathnames))
	(setf hints (nconc hints (handler-lint-1 (pathname pathname)))))
      (epilogue hints))))

;;;; End of file `lint.lisp'
