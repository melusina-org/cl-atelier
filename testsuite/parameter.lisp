;;;; parameter.lisp — Testing parameters for the Atelier

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.atelier/testsuite)

(defparameter *parameter-bindings*
  '((:project-name . "Atelier")
    (:license-text . "Copyright © ${COPYRIGHT_YEAR} ${COPYRIGHT_HOLDER}
All rights reserved.
")
    (:copyright-holder . "A. U. Thor")
    (:copyright-year . "2021")))

(defparameter *template-text-shell*
  "#!/bin/sh

# This file is part of ${PROJECT_NAME}.
#
# ${LICENSE_TEXT}

main()
{
  exit 1
}

main \"$@\"
")

(defparameter *template-text-cpp*
  "/* expected.cpp */

/* This file is part of ${PROJECT_NAME}. */

/* ${LICENSE_TEXT} */

int
main()
{
  return 1;
}
")

(define-assertion assert-string-contains (substring string)
  "The assertion (ASSERT-STRING-CONTAINS SUBSTRING STRING) is true,
iff SUBSTRING is a substring of STRING."
  (labels
      ((mainloop (match-start match-index)
	 (cond
	   ((> match-start (length string))
	    nil)
	   ((>= match-index (length substring))
	    t)
	   ((> (+ 1 match-index match-start) (length string))
	    nil)
	   ((eq (char string (+ match-index match-start))
		(char substring match-index))
	    (mainloop match-start (1+ match-index)))
	   (t
	    (mainloop (1+ match-start) 0)))))
    (mainloop 0 0)))

(define-assertion assert-subsequence (sequence1 sequence2 &key key (test #'eql))
  "The assertion (ASSERT-SUBSEQUENCE SEQUENCE1 SEQUENCE2) is true,
iff SEQUENCE1 is a subsequence of SEQUENCE2."
  (labels
      ((element (sequence n)
	 (if key
	     (funcall key (elt sequence n))
	     (elt sequence n)))
       (mainloop (match-start match-index)
	 (cond
	   ((> match-start (length sequence2))
	    nil)
	   ((>= match-index (length sequence1))
	    t)
	   ((> (+ 1 match-index match-start) (length sequence2))
	    nil)
	   ((funcall test
		     (element sequence2 (+ match-index match-start))
		     (element sequence1  match-index))
	    (mainloop match-start (1+ match-index)))
	   (t
	    (mainloop (1+ match-start) 0)))))
    (mainloop 0 0)))


(define-testcase ensure-sort-parameter-bindings-lists-all-parameters ()
  (assert-set-equal
   '("A" "B" "C" "U" "V")
   (atelier::sort-parameter-bindings '(("A" . "${B} ${C}")("C" . "${B}")("U" . "${V}")))
   :test #'atelier::parameter-name-equal)
  (assert-set-equal
   '(:license-text :copyright-holder :copyright-year :project-name)
   (atelier::sort-parameter-bindings *parameter-bindings*)
   :test #'atelier::parameter-name-equal))

(define-testcase ensure-sort-parameter-bindings-finds-all-cycles ()
  (assert-subsequence
   '("A" "C" "B")
   (atelier::sort-parameter-bindings '(("A" . "${B} ${C}")("C" . "${B}")("U" . "${V}")))
   :test #'atelier::parameter-name-equal)
  (assert-subsequence
   '("U" "V")
   (atelier::sort-parameter-bindings '(("A" . "${B} ${C}")("C" . "${B}")("U" . "${V}")))
   :test #'atelier::parameter-name-equal)
  (assert-subsequence
   '(:license-text :copyright-holder :copyright-year)
   (atelier::sort-parameter-bindings *parameter-bindings*)
   :test #'atelier::parameter-name-equal))

(define-testcase ensure-parameter-replace-handles-dependent-replacements ()
  (assert-string=
   (atelier::parameter-replace "A => ${A}" '(("A" . "${B} ${C}")("C" . "${B}")("U" . "${V}")))
   "A => ${B} ${B}"))

(define-testcase ensure-template-examples-are-processed-correctly ()
  (let ((expected-text-shell
	  "#!/bin/sh

# This file is part of Atelier.
#
# Copyright © 2021 A. U. Thor
# All rights reserved.

main()
{
  exit 1
}

main \"$@\"
")
	(expected-text-cpp
	  "/* expected.cpp */

/* This file is part of Atelier. */

/* Copyright © 2021 A. U. Thor
   All rights reserved. */

int
main()
{
  return 1;
}
"))
    (assert-string= expected-text-shell
		    (atelier::parameter-replace *template-text-shell* *parameter-bindings*))
    (assert-string= expected-text-cpp
		    (atelier::parameter-replace *template-text-cpp* *parameter-bindings*))))

(define-testcase testsuite-parameter ()
  (ensure-sort-parameter-bindings-lists-all-parameters)
  (ensure-sort-parameter-bindings-finds-all-cycles)
  (ensure-parameter-replace-handles-dependent-replacements)
  (ensure-template-examples-are-processed-correctly))

;;;; End of file `parameter.lisp'
