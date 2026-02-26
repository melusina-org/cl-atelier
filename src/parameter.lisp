;;;; parameter.lisp — Parameters for the Atelier

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

;;;
;;; Parameters
;;;

(defparameter *parameter-bindings* nil
  "An alist providing text replacement for various parameters.")

(defparameter *parameter-block*
  '(:license-text :license-header)
  "The list of parameters that are to be replaced as block variables.")

(defparameter *parameter-placeholder-regex*
  '(:sequence #\$ #\{
    (:register
     (:greedy-repetition 1 nil
      (:char-class (:range #\A #\Z) (:range #\a #\z) (:range #\0 #\9) #\_ #\-)))
    #\})
  "The regular expression matching parameter placeholders.")

(defun parameter-name-equal (parameter-name1 parameter-name2)
  "Predicate recognising equal parameter names.
Two parameter names are equal if they are equal regardless of case or difference
between shell-case or lisp-case."
  (labels
      ((normalize (text)
	 (ppcre:regex-replace-all "-" (string-upcase text) "_"))
       (parameter-text (parameter-name)
	 (typecase parameter-name
	   (symbol
	    (normalize (symbol-name parameter-name)))
	   (string
	    (normalize parameter-name))
	   (t
	    (error "~S: This parameter name is not supported." parameter-name)))))
    (string-equal
     (parameter-text parameter-name1)
     (parameter-text parameter-name2))))

(defun parameter-keyword (parameter-name)
  "The keyword associated with PARAMETER-NAME."
  (labels
      ((normalize (text)
	 (ppcre:regex-replace-all "_" (string-upcase text) "-")))
    (typecase parameter-name
      (keyword
       parameter-name)
      (symbol
       (make-keyword (normalize (symbol-name parameter-name))))
      (string
       (make-keyword (normalize parameter-name)))
      (t
       (error "~S: This parameter name is not supported." parameter-name)))))

(defun parameter-replacement-text (item &optional (alist *parameter-bindings*) default)
  (let ((binding
	  (assoc item alist :test #'parameter-name-equal)))
    (cond
      ((cdr binding)
       (cdr binding))
      (default
       default)
      (t
       (restart-case
	   (error "There is no replacement text for ~S under current bindings." item)
	 (use-replacement-text (value)
	   value))))))

(defun list-parameter-names (template-text)
  "The list of parameter names occuring in TEMPLATE-TEXT."
  (when (keywordp template-text)
    (return-from list-parameter-names))
  (let ((all-parameters nil)
	(this-parameter nil))
    (ppcre:do-scans (match-start match-end reg-starts reg-ends
		     (ppcre:create-scanner *parameter-placeholder-regex*)
		     template-text)
      (setf this-parameter
	    (subseq template-text (aref reg-starts 0) (aref reg-ends 0)))
      (unless (position this-parameter all-parameters :test #'parameter-name-equal)
	(push this-parameter all-parameters)))
    (nreverse all-parameters)))

(defun sort-parameter-bindings (bindings)
  "Sort BINDINGS in a replacement order.
The replacement order is a topological sort and processing paramters in this order
to handle a template text ensures all possible parameter names have been replaced.

It is an error to sort BINDINGS featuring a cyclic dependency."
  (let ((graph
	  (mapcar (lambda (binding)
		    (cons (car binding)
			  (list-parameter-names (cdr binding))))
		  bindings)))
    (labels
	((next (path visited node)
	   (let ((nextpath
		   (cons node path))
		 (edges
		   (cdr (assoc node graph :test #'parameter-name-equal))))
	     (reduce (lambda (visited node)
		       (explore nextpath visited node))
		     edges
		     :initial-value visited)))
	 (explore (path visited node)
	   "Deep-first search graph one node."
	   (cond
	     ((member node path :test #'parameter-name-equal)
	      (error "Cyclic dependency on parameter ~A." node))
	     ((member node visited :test #'parameter-name-equal)
	      visited)
	     (t
	      (cons node (next path visited node)))))
	 (dfs (visited startnode)
	   (explore nil visited startnode))
	 (toposort ()
	   (reduce (lambda (visited binding)
		     (dfs visited (car binding)))
		   bindings
		   :initial-value nil)))
      (toposort))))

(defun merge-parameter-bindings (bindings1 bindings2)
  "Make new bindings by adding to BINDINGS1 new parameters found in BINDINGS2.
When an entry of BINDINGS2 maps a key to NIL, the entry is ignored. When an entry
of BINDINGS2 uses the same key as an entry of BINDINGS1, that entry is ignored."
  (flet ((newp (binding)
	   (and (cdr binding)
		(not (assoc (car binding) bindings1 :test #'parameter-name-equal)))))
    (loop :with accumulator = bindings1
	  :for binding :in bindings2
	  :do (when (newp binding)
		(push binding accumulator))
	  :finally (return accumulator))))
    

(defun parameter-replace (template-text bindings)
  "Substitute parameters from TEMPLATE-TEXT according to BINDINGS."
  (labels
      ((parameter-name-text-regex (parameter-name)
	 (list :sequence #\$ #\{
	       (ppcre:regex-replace-all "-" (string-upcase parameter-name) "_")
	       #\}))
       (replace-text (template-text parameter-name)
	 (let ((replacement-text
		 (parameter-replacement-text parameter-name bindings)))
	   (ppcre:regex-replace-all
	    (parameter-name-text-regex parameter-name)
	    template-text replacement-text)))
       (parameter-name-block-regex (parameter-name)
	 (ppcre:create-scanner
	  (list :sequence
		:start-anchor
		'(:register (:greedy-repetition 0 nil :everything))
		(list :register (parameter-name-text-regex parameter-name))
		'(:register (:greedy-repetition 0 nil :everything))
		:end-anchor)
	  :multi-line-mode t))
       (taste-comment-style (prefix suffix)
         (cond
           ((or
             (and (string= prefix "(* ") (string= suffix " *)"))
             (and (string= prefix "{* ") (string= suffix " *}"))
             (and (string= prefix "/* ") (string= suffix " */")))
            :block)
           (t
            :linear)))
       (replacement-block (target-string start end match-start match-end reg-starts reg-ends)
	 (declare (ignore start end match-start match-end))
	 (let* ((prefix
		  (subseq target-string (aref reg-starts 0) (aref reg-ends 0)))
		(suffix
		  (subseq target-string (aref reg-starts 2) (aref reg-ends 2)))
		(parameter-name
		  (subseq target-string (+ 2 (aref reg-starts 1)) (1- (aref reg-ends 1))))
		(replacement-text
		  (parameter-replacement-text parameter-name bindings))
		(lines
		  (string-lines (string-right-trim '(#\Newline #\Space) replacement-text)))
		(separator
                  (if (eq (taste-comment-style prefix suffix) :block)
                      (format nil "~%~A" (make-string (length prefix) :initial-element #\Space))
                      (format nil "~A~%~A" suffix prefix))))
	   (with-output-to-string (buffer)
	     (write-string prefix buffer)
             (write-string (first lines) buffer)
             (loop for line in (rest lines)
                   do (write-string separator buffer)
                   do (write-string line buffer))
	     (write-string suffix buffer))))
       (replace-block (template-text parameter-name)
	 (ppcre:regex-replace-all
	  (parameter-name-block-regex parameter-name)
	  template-text #'replacement-block))
       (replace-one (template-text parameter-name)
	 (cond
	   ((not (assoc parameter-name bindings :test #'parameter-name-equal))
	    template-text)
	   ((member parameter-name *parameter-block* :test #'parameter-name-equal)
	    (replace-block template-text parameter-name))
	   (t
	    (replace-text template-text parameter-name)))))
    (reduce #'replace-one (sort-parameter-bindings bindings) :initial-value template-text)))

;;;; End of file `parameter.lisp'
