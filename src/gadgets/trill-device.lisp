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
;;;  Trill Device
;;;
;;;  Features and operation of the trill device:
;;;     1)  Click the left mouse button in the trill boxes to change the value
;;;         by :scr-incr and :page-incr increments.
;;;     2)  Text inside the feedback box changes to indicate the current value
;;;     3)  The value text may be edited directly after clicking the left
;;;         mouse button on it.
;;;     4)  The top level :value slot is the currently chosen value.
;;;         This slot may be set directly and formulae may depend on it.
;;;
;;;  Customizable slots:
;;;     1)  Left, top
;;;     2)  Min-Frame-Width -- If :val-1 and :val-2 are both non-NIL, then this
;;;                      slot is overridden if the width of the widest allowed
;;;                      feedback value is wider than this specified value
;;;                      (i.e., a fixed width is calculated).
;;;                      If either :val-1 or :val-2 are NIL, then this slot is
;;;                      overridden when the width of a feedback value exceeds
;;;                      this value (i.e., the width is dynamic).
;;;     2)  Min-height -- Overridden when the height of the text in the
;;;                       value feedback box is taller than this value
;;;     2)  Scr-trill-p  --  Whether to have trills that incr by :scr-incr
;;;     3)  Page-trill-p --  Whether to have trills that incr by :page-incr
;;;     4)  Scr-Incr, Page-incr  --  Values to increment position by in single
;;;                                    and double arrow boxes, respectively
;;;     5)  Val-1, Val-2  --  Range of values to be spanned.  Val-1 corresponds
;;;                           to the left side of the trill device.
;;;                           If val-1 is NIL, val-2 is max; there is no min.
;;;                           If val-2 is NIL, val-1 is min; there is no max.
;;;     6)  Value -- The currently selected value
;;;     7)  Selection-function -- Function called when :value changes
;;;     8)  Value-feedback-p  --  Whether to report the value in the value
;;;                               feedback box
;;;     9)  Value-feedback-font -- Font to report current value with
;;;    10)  Scroll-p -- Whether to allow the value to be changed
;;;    11)  Format-string -- formatting string for current value
;;;
;;;  NOTE:  This module requires schemata defined in GAD-scroll-parts,
;;;         GAD-slider-parts, GAD-h-arrows, and GAD-h-boxes.
;;;
;;;  Trill device demo:
;;;     This module contains a function which creates a window and a trill
;;;     device in the window.  To run it, enter (GARNET-GADGETS:trill-go).
;;;     To stop, enter (GARNET-GADGETS:trill-stop).
;;;
;;;  Designed by Brad Myers
;;;  Written by Andrew Mickish

;;;  CHANGE LOG:
;;;  12/14/92 Andrew Mickish - Added types and parameter declarations
;;;  02/11/92 Andrew Mickish - Added :maybe-constant list
;;;  11/30/90 Pavan Reddy - Moved use of "format" (vs "prin1-to-string") to
;;;           lower-level module (GAD-slider-parts).
;;;  08/14/90 Pavan Reddy - Changed use of "prin1-to-string" to "format" and
;;;           added a :format-string slot at the top level aggregadget for
;;;           formatting.  All this at the request of a user.
;;;  01/30/90 Andrew Mickish - Added :scroll-p slot to TRILL-DEVICE

(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Trill-Device))
  #+garnet-test
  (export '(Trill-Go Trill-Stop
	    Trill-Top-Agg Trill-Win Trill-Obj)))


(create-instance 'DEVICE-FEEDBACK opal:aggregadget
   (:left (o-formula (gv (kr-path 0 :parent) :value-feedback-left)))
   (:top (o-formula (gv (kr-path 0 :parent) :top)))
   (:height (o-formula (gv (kr-path 0 :parent) :value-feedback-height)))
   (:width (o-formula (gv (kr-path 0 :parent) :value-feedback-width)))
   (:font (o-formula (gv (kr-path 0 :parent) :value-feedback-font)))
   (:visible (o-formula (gv (kr-path 0 :parent) :value-feedback-p)))
   (:parts
    `((:value-rect ,value-rect)
      (:value-text ,value-text)))
   (:interactors
    `((:value-inter ,value-inter))))


;;;
;;;  TOP LEVEL AGGREGADGET
;;;

(create-instance 'TRILL-DEVICE opal:aggregadget
   :declare ((:parameters :left :top :min-frame-width :min-height
			  :scr-incr :page-incr :val-1 :val-2 :scr-trill-p
			  :page-trill-p :scroll-p :value-feedback-p
			  :value-feedback-font :format-string :value
			  :selection-function :visible)
	     (:type ((integer 0) :min-frame-width :min-height)
		    ((or number null) :val-1 :val-2)
		    (number :scr-incr :page-incr :value)
		    (kr-boolean :scr-trill-p :page-trill-p :scroll-p
		     :value-feedback-p)
		    (string :format-string)
		    ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
		     :value-feedback-font)
		    ((or null function symbol) :selection-function))
	     (:maybe-constant :left :top :min-frame-width :min-height :scr-incr
			      :page-incr :val-1 :val-2 :scr-trill-p
			      :page-trill-p :scroll-p :value-feedback-p
			      :format-string :value-feedback-font :visible))
   
   ;; Customizable slots
   ;;
   (:left 0)(:top 0)
   (:min-frame-width 20)
   (:min-height 20)
   (:scr-incr 1)
   (:page-incr 5)
   (:val-1 0)          ;; Range of values
   (:val-2 100)        ;;   :value may assume
   (:scr-trill-p T)
   (:page-trill-p T)
   (:scroll-p T)
   (:value-feedback-p T)
   (:format-string "~a")
   (:value-feedback-font opal:default-font)
   (:selection-function NIL)

   ;; Generally non-customizable slots
   ;;
   (:value 0)
   (:widest-value-width
    (o-formula (if (and (gvl :val-1) (gvl :val-2))
		   (max (opal:string-width (gvl :value-feedback-font)
					   (format NIL (gvl :format-string)
						   (gvl :val-1)))
			(opal:string-width (gvl :value-feedback-font)
					   (format NIL (gvl :format-string)
						   (gvl :val-2))))
		   (max (gvl :min-frame-width)
			(gvl :value-feedback :value-text :width)))))
   (:value-feedback-width (o-formula (if (gvl :value-feedback-p)
					 (+ 5 (gvl :widest-value-width))
					 0)))
   (:highest-value-height (o-formula (opal:string-height
				      (gvl :value-feedback-font) "X")))
   (:value-feedback-height
    (o-formula (if (gvl :value-feedback-p)
		   (max (gvl :min-height)
			(+ 2 (gvl :highest-value-height)))
		   (gvl :min-height))))
   (:value-feedback-left (o-formula (if (gvl :page-trill-p)
					(+ (gvl :left-page-left)
					   (gvl :trill-width))
					(gvl :left-page-left))))
   (:trill-width (o-formula (gvl :height)))
   (:num-trills (o-formula (+ (if (gvl :scr-trill-p) 1 0)
			     (if (gvl :page-trill-p) 1 0))))
   (:trill-box-top (o-formula (gvl :top)))
   (:left-scr-left (o-formula (gvl :left)))
   (:left-page-left (o-formula (if (gvl :scr-trill-p)
				   (+ (gvl :left-scr-left) (gvl :trill-width))
				   (gvl :left-scr-left))))
   (:right-page-left (o-formula (+ (gvl :value-feedback-left)
				   (gvl :value-feedback-width))))
   (:right-scr-left (o-formula (if (gvl :page-trill-p)
				   (+ (gvl :right-page-left) (gvl :trill-width))
				   (gvl :right-page-left))))
   (:bound-height (o-formula (gvl :value-feedback-height)))
   (:width (o-formula (+ (* 2 (gvl :num-trills) (gvl :trill-width))
			 (gvl :value-feedback-width))))
   (:height (o-formula (gvl :value-feedback-height)))
   (:parts
    `((:value-feedback ,device-feedback)      ; report value in box
      (:left-scr-trill ,left-scr-trill)
      (:right-scr-trill ,right-scr-trill)
      (:left-page-trill ,left-page-trill)
      (:right-page-trill ,right-page-trill))))



;;;
;;;  DEMO FUNCTION
;;;

#+garnet-test (defparameter trill-win NIL)
#+garnet-test (defparameter trill-top-agg NIL)
#+garnet-test (defparameter trill-obj NIL)

#+garnet-test
(defun Trill-Go ()
  (create-instance 'trill-win inter:interactor-window
     (:left 650) (:top 10) (:width 350) (:height 200))
  (s-value trill-win
	   :aggregate
	   (create-instance 'trill-top-agg opal:aggregate
	      (:overlapping T)))
  (create-instance 'trill-obj trill-device (:left 50) (:top 50))
  (opal:add-components trill-top-agg trill-obj)
  (opal:update trill-win))

#+garnet-test
(defun Trill-Stop ()
  (opal:destroy trill-win ))
