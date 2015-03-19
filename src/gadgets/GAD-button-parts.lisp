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
;;;  GAD-button-parts
;;;
;;;  This module contains definitions of schemata required for the button
;;;  items in the Garnet Gadgets.
;;;
;;;  This module must be loaded before any of the garnet button modules.
;;;
;;;  Written by Andrew Mickish

;;;
;;; Change log:
;;;
;;; 05/26/93: Andrew Mickish - Changed :constant list of ITEMS-AGGLIST
;;; 11/30/92: Andrew Mickish - Added :fixed-width and :fixed-height to
;;;             constant list of ITEMS-AGGLIST
;;; 02/17/92: Andrew Mickish - Added :constant lists
;;; 01/18/91: Andrew Mickish - Removed "path" from :direction formula of
;;;                            ITEMS-AGGLIST
;;; 07/04/90: Pavan Reddy - changed formulas for :top slot of GRAY-*-OUTLINE,
;;;           BUTTON-SHADOW-*, and *-BUTTON-TEXT for better centering
;;; 07/02/90: Andrew Mickish - Removed BUTTON-INV-RECT
;;; 06/25/90: Andrew Mickish - Removed BUTTON-INV-CIRC
;;; 06/21/90: Andrew Mickish - Fixed :left slot of INSIDE-BUTTON-TEXT and
;;;           BESIDE-BUTTON-TEXT to consider value of :h-align
;;; 02/26/90: Andrew Mickish - Removed :fixed-width-size and :fixed-height-size
;;;           from ITEMS-AGGLIST, set :fixed-width-p and :fixed-height-p to NIL
;;; 01/12/90: Andrew Mickish - Removed :visible slots of BUTTON-SHADOW-RECT
;;;           and BUTTON-SHADOW-CIRC.
;;;

(in-package "GARNET-GADGETS")


(defun Report-Selection (top-level-obj value)
  (let ((value-obj (g-value top-level-obj :value-obj)))
    (if value-obj
	(format t "Item ~S selected with value ~S.~%" value-obj value)
	(format t "Gadget ~S has value ~S~%" top-level-obj value))))


;;;  GRAY-RECT-OUTLINE:  This rectangle is laid on top of the shadow.
;;;  It will be partially covered by a white rectangle, giving the appearance
;;;  that this is just a gray border
;;;
(create-instance 'GRAY-RECT-OUTLINE opal:rectangle
   (:left (o-formula (gv (kr-path 0 :parent) :floating-left)))
   (:top (o-formula (gv (kr-path 0 :parent) :floating-top)))
   (:width (o-formula (gv (kr-path 0 :parent) :button-width)))
   (:height (o-formula (gv (kr-path 0 :parent) :button-height)))
   (:filling-style opal:gray-fill))


;;;  WHITE-RECT-FIELD:  This rectangle is laid on top of the gray rectangle,
;;;  leaving a gray border.
;;;
(create-instance 'WHITE-RECT-FIELD opal:rectangle
   (:left (o-formula (+ (gv-fixnum (kr-path 0 :parent) :floating-left)
			(gv-fixnum (kr-path 0 :parent) :gray-width))))
   (:top (o-formula (+ (gv-fixnum (kr-path 0 :parent) :floating-top)
		       (gv-fixnum (kr-path 0 :parent) :gray-width))))
   (:width (o-formula (- (gv-fixnum (kr-path 0 :parent) :button-width)
			 (* 2 (gv-fixnum (kr-path 0 :parent) :gray-width)))))
   (:height (o-formula (- (gv-fixnum (kr-path 0 :parent) :button-height)
			  (* 2 (gv-fixnum (kr-path 0 :parent) :gray-width)))))
   (:filling-style opal:white-fill))


;;;  BUTTON-SHADOW-RECT:  This black box is below all other objects, giving the
;;;  appearance that the button casts a shadow.
;;;
(create-instance 'BUTTON-SHADOW-RECT opal:rectangle
   (:left (o-formula (+ (gv-fixnum (kr-path 0 :parent) :button-left)
			(gv-fixnum (kr-path 0 :parent) :shadow-offset))))
   (:top (o-formula (+ (gv-fixnum (kr-path 0 :parent) :button-top)
		       (gv-fixnum (kr-path 0 :parent) :shadow-offset))))
   (:width (o-formula (gv-fixnum (kr-path 0 :parent) :button-width)))
   (:height (o-formula (gv-fixnum (kr-path 0 :parent) :button-height)))
   (:filling-style opal:black-fill))


;;;  BESIDE-BUTTON-TEXT:  This text object is placed either to the left or
;;;  the right of the button.
;;; 
(create-instance 'BESIDE-BUTTON-TEXT opal:text
   (:constant '(:actual-heightp))
   (:left (o-formula (let ((p (kr-path 0 :parent)))
		       (if (gv p :text-on-left-p)
			   (gv-fixnum p :left)
			   (+ (gv-fixnum p :left) (gv-fixnum p :button-unit-width)
			      (gv-fixnum p :text-offset))))))
   (:top (o-formula (- (gv-fixnum (kr-path 0 :parent) :center-y)
		       (floor (gvl-fixnum :height) 2))))
   (:string (o-formula (let ((s (gv (kr-path 0 :parent) :string)))
			 (if (stringp s)
			     s
			     (string-capitalize (string-trim ":" s))))))
   (:font (o-formula (gv (kr-path 0 :parent) :font))))


;;;  ITEMS-AGGLIST:  Generic aggrelist.  Note that all aggrelist slots must be
;;;  inherited in case the user specifies them.
;;;  Pitfall:  Be sure to put appropriate default values in the parent.
;;;
(create-instance 'ITEMS-AGGLIST opal:aggrelist
   (:constant '(:fixed-width-p :fixed-height-p :fixed-width-size
		:fixed-height-size :v-align))
   (:left (o-formula (gv (kr-path 0 :parent) :left)))
   (:top (o-formula (gv (kr-path 0 :parent) :top)))
   (:direction (o-formula (gvl :parent :direction)))
   (:h-align (o-formula (gv (kr-path 0 :parent) :h-align)))
   (:v-spacing (o-formula (gv (kr-path 0 :parent) :v-spacing)))
   (:h-spacing (o-formula (gv (kr-path 0 :parent) :h-spacing)))
   (:fixed-width-p NIL) (:fixed-height-p NIL)
   (:rank-margin (o-formula (gv (kr-path 0 :parent) :rank-margin)))
   (:pixel-margin (o-formula (gv (kr-path 0 :parent) :pixel-margin)))
   (:indent (o-formula (gv (kr-path 0 :parent) :indent)))
   (:items (o-formula (gv (kr-path 0 :parent) :items))))


; Declare that GAD-button-parts has been loaded
;
(setf (get :garnet-modules :GAD-button-parts) T)

