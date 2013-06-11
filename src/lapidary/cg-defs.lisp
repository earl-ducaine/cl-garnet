;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constraint Gadget:Defs.Lisp
;;;
;;; This file contains many of the schemas, defconstants, defvars, defmacros, 
;;; and defstructs which are used by Constraint Gadget.  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Change Log
;;;
;;; 5/10/93 bvz Created
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "GARNET-GADGETS")

(eval-when (eval load compile)
  (export '(box-constraint-do-go
	    line-constraint-do-go 
	    show-box-constraint-menu show-line-constraint-menu
	    c32 destroy-constraint-support-slots
	    cg-destroy-constraint valid-integer-p)))
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; fonts used in constraint menus
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *bold-font* (create-instance NIL opal:font (:face :bold)))

(defvar *large-bold-italic-serif-font* (create-instance NIL opal:font
				         (:size :large) (:face :bold-italic)
					 (:family :serif)))

;;; font used for slot names in constraint menus
(defvar *slot-font* (create-instance NIL opal:font
		       (:size :large) (:family :serif) (:face :bold-italic)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; determine whether a constraint should be attached to an object--it
;;; should only be done if there is one primary and one secondary selection
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro apply-box-constraint-p ()
  `(and (g-value *constraint-gadget* :obj-to-constrain)
	(g-value *constraint-gadget* :obj-to-reference)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; determines whether the constraint
;;; gadget considers this object an instance of a line
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro is-a-line-p (obj)
  `(or (is-a-p ,obj opal:line)
       (is-a-p ,obj garnet-gadgets:arrow-line)
       (is-a-p ,obj garnet-gadgets:double-arrow-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; exported gadget used to collect information about:
;;;   1) which object should be constrained
;;;   2) which object should be used as a reference in the constraint
;;;   3) where in the aggregate hierarchy to stop when searching for
;;;        the common root of two objects--you may want to stop before
;;;        the top-level. This would be true for example if you had both an
;;;        editor-agg and a feedback-agg
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(create-schema '*constraint-gadget*
   (:obj-to-constrain nil)
   (:obj-to-reference nil)
   (:selection-type (o-formula (let ((p-selection (gvl :obj-to-constrain))
				     (s-selection (gvl :obj-to-reference)))
				 (cond ((and p-selection (null s-selection))
					'one-zero)
				       ((and p-selection s-selection)
					'one-one)
				       (t nil)))))
   (:top-level-agg nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; error gadget to display error messages
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(create-instance '*constraint-gadget-error-window* garnet-gadgets:error-gadget)

(defun constraint-gadget-error (msg &optional (wait-p t))
  (if wait-p
      (garnet-gadgets:display-error-and-wait *constraint-gadget-error-window* msg)
      (garnet-gadgets:display-error *constraint-gadget-error-window* msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; query gadget to display queries
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(create-instance '*constraint-gadget-query-window* garnet-gadgets:query-gadget)

(defun constraint-gadget-query (msg labels &optional (wait-p t))
  (if wait-p
      (garnet-gadgets:display-query-and-wait *constraint-gadget-query-window*
					     msg labels)
      (garnet-gadgets:display-query *constraint-gadget-query-window* msg
				    labels)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   titled frame for line constraint menu
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(create-instance 'TITLED-FRAME opal:aggregadget
   (:maybe-constant :font :string :left :top :width :height)
   (:left (o-formula (gvl :parent :left)))
   (:top (o-formula (gvl :parent :top)))
   (:width (o-formula (+ 30 (gvl :parent :contents :width))))
   (:height (o-formula (+ (floor (gvl :text :height) 2)
			  (gvl :frame :height))))
   (:string ":slot")
   (:font *bold-font*)
   (:parts
    `((:frame ,opal:rectangle
		(:left ,(o-formula (gvl :parent :left)))
		(:top ,(o-formula (opal:gv-center-y (gvl :parent :text))))
		(:width ,(o-formula (gvl :parent :width)))
		(:height ,(o-formula (+ 20 (gvl :parent :parent :contents
						:height)))))
      (:text-frame ,opal:rectangle
		   (:left ,(o-formula (+ (gvl :parent :left) 10)))
		   (:top ,(o-formula (gvl :parent :text :top)))
		   (:width ,(o-formula (+ (gvl :parent :text :width) 10)))
		   (:height ,(o-formula (gvl :parent :text :height)))
		   (:line-style NIL)
		   (:filling-style ,opal:white-fill))
      (:text ,opal:text
	     (:left ,(o-formula (+ (gvl :parent :text-frame :left) 5)))
	     (:top ,(o-formula (gvl :parent :top)))
	     (:string ,(o-formula (gvl :parent :string)))
	     (:font ,(o-formula (gvl :parent :font)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Useful constants for attaching constraints to lines
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant 45deg (+ 1 (/ (sqrt 2) 2)))
(defconstant 135deg (- 1 (/ (sqrt 2) 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; prototype formulas for the constraints shown in the constraint gadget's
;;; constraint menus
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *left-outside-formula* 
  (o-formula (- (opal:gv-right-is-left-of (gvl :left-over))
					  (gvl :left-offset))
	     0
	     (:menu-item :out-left)))

(defvar *left-inside-formula* (o-formula (+ (gvl :left-over :left)
					    (gvl :left-offset))
					 0
					 (:menu-item :in-left)))
(defvar *left-center-formula* 
  (o-formula (- (+ (gvl :left-over :left)
		   (truncate (* (gvl :left-over :width)
				(gvl :left-offset))))
		(truncate (gvl :width) 2))
	     0
	     (:menu-item :center)))

(defvar *right-inside-formula* 
  (o-formula (1+ (- (opal:gv-right (gvl :left-over))
		    (gvl :width)
		    (gvl :left-offset)))
	     0
	     (:menu-item :in-right)))

(defvar *right-outside-formula* (o-formula (+ (opal:gv-right (gvl :left-over))
					      (gvl :left-offset))
					   0
					   (:menu-item :out-right)))

(defvar *left-constraint-vector* 
  (make-array 5 :initial-contents (list *left-outside-formula*
					*left-inside-formula*
					*left-center-formula*
					*right-inside-formula*
					*right-outside-formula*)))

(defvar *top-outside-formula* 
  (o-formula (- (opal:gv-bottom-is-top-of (gvl :top-over))
		(gvl :top-offset))
	     0
	     (:menu-item :out-top)))

(defvar *top-inside-formula* (o-formula (+ (gvl :top-over :top)
					   (gvl :top-offset))
					0
					(:menu-item :in-top)))

(defvar *top-center-formula* 
  (o-formula (- (+ (gvl :top-over :top)
		   (truncate (* (gvl :top-over :height)
				(gvl :top-offset))))
		(truncate (gvl :height) 2))
	     0
	     (:menu-item :center)))

(defvar *bottom-inside-formula* 
  (o-formula (1+ (- (opal:gv-bottom (gvl :top-over))
		    (gvl :height)
		    (gvl :top-offset)))
	     0
	     (:menu-item :in-bot)))

(defvar *bottom-outside-formula* (o-formula (+ (opal:gv-bottom (gvl :top-over))
					       (gvl :top-offset))
					    0
					    (:menu-item :out-bot)))

(defvar *top-constraint-vector* 
  (make-array 5 :initial-contents (list *top-outside-formula*
					*top-inside-formula*
					*top-center-formula*
					*bottom-inside-formula*
					*bottom-outside-formula*)))

(defvar *width-formula* (o-formula (round (+ (* (gvl :width-scale) 
						(gvl :width-over :width))
					     (gvl :width-difference)))
				   0
				   (:menu-item :con-button)))

(defvar *width-constraint-vector* 
  (make-array 1 :initial-contents (list *width-formula*)))

(defvar *height-formula* (o-formula (round (+ (* (gvl :height-scale) 
					 (gvl :height-over :height))
				      (gvl :height-difference)))
				    0
				    (:menu-item :con-button)))

(defvar *height-constraint-vector* 
  (make-array 1 :initial-contents (list *height-formula*)))
(defvar *custom-constraint* nil)

;; lists of offset slots used by objects
(defvar *list-offsets* (list :x1-offset :x2-offset :y1-offset :y2-offset))
(defvar *box-offsets* (list :left-offset :top-offset 
			    :width-difference :height-difference))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; some slots should not be altered by the user. These are stored in
;;; an object's :do-not-alter-slots
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s-value opal:text :do-not-alter-slots '(:width :height))
(s-value opal:bitmap :do-not-alter-slots '(:width :height))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; when a constraint is destroyed in a slot, some slots, such as
;;; :filling-style and :line-style, should inherit from their prototype;
;;; others, such as :left and :top, should assume their current values.
;;; the following list records those slots which should not inherit
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *do-not-inherit-list* '(:left :top :active))

