;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lapidary:Macros.Lisp
;;;
;;; This file contains the macros that are used by Lapidary. 
;;;

(in-package "LAPIDARY")

;;; macros for mapping Opal names to Jade generated names

(defmacro map-family (family)
  `(case ,family
     (:times :|times|)
     (:helvetica :|helvetica|)
     (:fixed :|fixed|)
     (:variable :|variable|)))

(defmacro map-face (face)
  `(case ,face
     (:roman :|roman|)
     (:italic :|italic|)
     (:bold :|bold|)
     (:bold-italic :|bold-italic|)))

(defmacro map-size (size)
  `(case ,size
     (:small :|small|)
     (:medium :|medium|)
     (:large :|large|)))

;;; Makes a keyword from a string: "foo" -> :foo
(declaim (inline keyword-from-string))
(defun keyword-from-string (string)
  ;;  `(read-from-string (concatenate 'string ":" ,string)))
  (intern (string-upcase string) :keyword))

;;; determines whether lapidary considers this object an instance of a line
(defmacro is-a-line-p (obj)
  `(or (is-a-p ,obj opal:line)
       (is-a-p ,obj garnet-gadgets:arrow-line)
       (is-a-p ,obj garnet-gadgets:double-arrow-line)))

;;; determines whether lapidary considers this object a line
(defmacro line-p (obj)
  `(or (eq ,obj opal:line)
       (eq ,obj garnet-gadgets:arrow-line)
       (eq ,obj garnet-gadgets:double-arrow-line)))

;;; determines whether the object is a primary selection
(defmacro is-p-selected (obj) `(get-local-value ,obj :p-feedback-obj))

;;; determines whether the object is a secondary selection
(defmacro is-s-selected (obj) `(get-local-value ,obj :s-feedback-obj))

;;; returns either :x1 or :x2, depending on which is the leftmost endpoint.
;;; in case of ties, it returns the topmost endpoint
(defmacro left-endpoint (line)
  `(if (or (< (g-value ,line :x1) (g-value ,line :x2))
	   (and (= (g-value ,line :x1) (g-value ,line :x2))
		(<= (g-value ,line :y1) (g-value ,line :y2))))
       :x1 :x2))


;;;-------------------------------------
;;; macros for Ed's centering functions
;;;-------------------------------------
(defmacro gv-center-my-left (gob)
  `(opal:gv-center-x-is-center-of ,gob))
(defmacro gv-center-my-top (gob)
  `(opal:gv-center-y-is-center-of ,gob))
;;;-------------------------------------

;; determine whether a constraint should be attached to an object--it
;; should only be done if there is one primary and one secondary selection
(defmacro apply-box-constraint-p ()
  `(eq (g-value *selection-info* :selection-type) 'one-one))

;;; indicates whether an object has been marked
(defmacro marked-p (obj)
  `(g-value ,obj :mark))

;;; this make-path procedure may be needed in future releases of lapidary,
;;; since it is able to find paths between objects that do not go through
;;; a top-level aggregate. 

;;; marks or unmarks an object
(defmacro set-marked-bit (obj value)
  `(s-value ,obj :mark ,value))
