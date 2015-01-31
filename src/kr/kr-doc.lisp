;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: :Kr -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written by Russell Almond at Statistical Sciences  ;;
;;  as an independent contribution to the Garnet project at          ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;                                                                   ;;
;;  The authors of the code make no warentee expressed or implied    ;;
;;  about its utility, rather we hope that someone may find a use    ;;
;;  for it.                                                          ;;
;;*******************************************************************;;

;;; $Id$
;;


;;; This package introduces a documentation convention for KR objects
;;  and provides some simple functions to read that documentation.
;;
;; The first half of the convention is easy.  Each schema can have a
;; slot called :documentation which contains information for the
;; programmer about the schema.

;; The second half of the convention provides information about
;; slots.  This is done through the :slot-doc slot of the schema.
;; This is a paired list of the form slot-name, doc-string.  

;; The function kr:get-slot-doc (<schema> <slot>) accesses the
;; doc-string for a schema. It will search first the local slot and
;; then back through the inheritence chain to find the documentation
;; for a slot.  The function kr:set-slot-doc  (<schema> <slot>
;; <value>) will set the documentation associated with the slot.

;; In order to prevent documentation strings from adding volume to
;; images where they are not wanted, I've added a feature switch
;; kr-doc.  This should be used to protect documentation when it is
;; not wanted (i.e., when this file has not been loaded first.)
;; Example:
;;  (kr:create-instance 'verbose-rectangle opal:rectangle
;;    #+kr-doc (:documentation "Opal:rectangle with documentation strings.")
;;    #+kr-doc (:slot-doc :left "Horizontal Co-ordinate for Rectangle."
;;		          :top "Vertical Co-ordinate for Rectangle."
;;		          :height "Vertical Extent of Rectangle."
;;		          :width "Horizontal Extent of Rectangle."
;;		          :line-style "The color, width and dashing of the border."
;;		          :filling-style "The color and shading of the interior." 
;;		          :draw-function "How does drawing interact with
;;                                        objects underneath."
;;		          :visible "Is the object to be drawn?"))


;;; KR part of garnet must be loaded.
(in-package :Kr)

(export '(get-slot-doc set-slot-doc))

(defun get-slot-doc (schema slot)
  "Returns the documentation string associated with <slot> in <schema>."
  (declare (type (or Schema List) schema)
	   (type (or Keyword Symbol) slot))
  (cond ((null schema) nil)
	((consp schema)
	 (let ((doc-string (get-slot-doc (car schema) slot)))
	   (declare (type (or String Null) doc-string))
	   (if (equal nil doc-string)
	       (get-slot-doc (cdr schema) slot)
	     doc-string)))
	((schema-p schema)
	 (let ((doc-string (getf (get-local-value schema :slot-doc) slot)))
	   (if (stringp doc-string) doc-string
	     (get-slot-doc (get-local-value schema :is-a) slot))))
	(t (error "~S is not a schema or list of schemas."
		  schema))))


(defun set-slot-doc (schema slot doc-string)
  "Sets the documentation string associated with <slot> in <schema>."
  (declare (type Schema  schema)
	   (type (or Keyword Symbol) slot)
	   (type String doc-string))
  (let ((doc-plist (get-local-value schema :slot-doc)))
    (setf (getf doc-plist slot) doc-string)
    (s-value schema :slot-doc  doc-plist)))
	
(pushnew :KR-DOC *Features*)

	
	 
  
  
