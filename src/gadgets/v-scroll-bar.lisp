;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;  please contact garnet@cs.cmu.edu to be put on the mailing list.  ;;
;;*******************************************************************;;

;;; $Id$
;;


;;;  Vertical scroll bar
;; 
;;   Features and operation of the vertical scroll bar:
;;      1)  Drag the indicator with the left mouse button
;;      2)  Click the left mouse button in the scroll bar background to cause
;;          the indicator to jump to mouse location
;;      3)  Click the left mouse button in the trill boxes to move the
;;          indicator by :scr-incr and :page-incr increments
;;      4)  Text centered in the indicator changes to reflect new indicator
;;          position
;;      5)  The top level :value slot is the position of the indicator.
;;          This slot may be set directly and formulae may depend on it.
;;      6)  The function specified in :selection-function will be executed
;;          when the :values slot changes.
;; 
;;   Customizable slots:
;;      1)  Left, top, height
;;      2)  Min-width --  Will be overridden by the value calculated in
;;                        :indicator-width if the width of the text in the
;;                        indicator exceeds this width
;;      3)  Scr-trill-p  --  Whether to have single arrow trill boxes that
;;                           increment by :scr-incr
;;      4)  Page-trill-p --  Whether to have double arrow trill boxes that
;;                           increment by :page-incr
;;      5)  Indicator-text-p -- Whether to report indicator position
;;                              numerically inside the indicator
;;      6)  Int-feedback-p  --  Whether to follow mouse with thick outline box
;;                              instead of with indicator directly
;;      7)  Val-1, Val-2  --  Range of values the indicator spans.
;;                            Val-1 corresponds to the top of the scroll bar.
;;      8)  Scr-Incr  --  Value to increment position by in single arrow box
;;      9)  Page-incr  --  Value to increment postion by in double arrow box
;;     10)  Value -- The current value chosen by the user
;;     11)  Scroll-p -- Whether to allow scrolling
;;     12)  Selection-function -- Function executed whenever :value changes
;;     13)  Indicator-font -- Font to report indicator position with
;;     14)  Format-string -- formatting string of indicator value
;; 
;;   NOTE:  This module requires schemata defined in GAD-scroll-parts,
;;          GAD-v-arrows, and GAD-v-boxes.
;; 
;;   Vertical scroll bar demo:
;;      This module contains a function which creates a window and a scroll bar
;;      in the window.  To run it, enter (GARNET-GADGETS:v-scroll-go).
;;      To stop, enter (GARNET-GADGETS:v-scroll-stop).
;; 
;;   Designed by Brad Myers
;;   Written by Andrew Mickish


;;;  CHANGE LOG:
;;   12/14/92  Andrew Mickish - Added type and parameter declarations
;;   04/30/92  Andrew Mickish - Called get-standard-font for small font
;;   02/07/92  Andrew Mickish - Added :maybe-constant slots
;;   11/30/90  Pavan Reddy - Added use of :format-string slot so floats work.
;;   08/01/90  Pavan Reddy - Fixed divide-by-zero error that occurs when
;;      :val-1 and :val-2 slots of V-SCROLL-BAR are equal.  Fix contributed
;;      by Rod Williams.
;;   07/01/90  Andrew Mickish - Removed :box from V-INDICATOR-BOX and placed
;;      distinct values in each instance instead.  Changed :visible slot of
;;      :INT-FEEDBACK to depend on local :obj-over.
;;   01/18/90  Andrew Mickish - Changed :box of V-INDICATOR-BOX to '(0 0 0 0),
;;      Added :scroll-p slot to V-SCROLL-BAR, changed :filling-style in
;;      BOUNDING-AREA component of V-SCROLL-BAR, added :visible slot to
;;      V-INDICATOR-BOX.



(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(V-Scroll-Bar))
  #+garnet-test
  (export '(V-Scroll-Go V-Scroll-Stop
	    V-Scroll-Obj V-Scroll-Win V-Scroll-Top-Agg)))



;; VERTICAL INDICATOR BOX
;;
(create-instance 'V-INDICATOR-BOX opal:rectangle
   (:top (o-formula (let ((p (kr-path 0 :parent)))
		      (if (/= (gv p :val-1) (gv p :val-2))
			  (inter:Clip-and-Map (gv p :value)
					      (gv p :val-1) (gv p :val-2)
					      (gv p :bound-top)
					      (- (gv p :bound-bottom)
						 (gvl :height)))
			  (opal:gv-center-y-is-center-of (gv p))))))
   (:left (o-formula (+ 1 (gv (kr-path 0 :parent) :left))))
   (:width (o-formula (- (gv (kr-path 0 :parent) :bound-width) 2)))
   (:height (o-formula (gvl :width)))
   (:filling-style opal:white-fill)
   (:visible (o-formula (let ((p (kr-path 0 :parent)))
			  (and (gv p :scroll-p)
			       (/= (gv p :val-1) (gv p :val-2)))))))


;; TOP LEVEL AGGREGADGET
;;
(create-instance 'V-SCROLL-BAR opal:aggregadget
   :declare ((:parameters :left :top :height :min-width :val-1 :val-2
			  :scr-incr :page-incr :scr-trill-p :page-trill-p
			  :indicator-text-p :int-feedback-p :scroll-p
			  :format-string :indicator-font :value
			  :selection-function :visible)
	     (:type ((integer 0) :min-width)
		    (number :val-1 :val-2 :page-incr :scr-incr :value)
		    (kr-boolean :scr-trill-p :page-trill-p
		     :indicator-text-p :int-feedback-p :scroll-p)
		    (string :format-string)
		    ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
		     :indicator-font)
		    ((or null function symbol) :selection-function))
	     (:maybe-constant :left :top :height :min-width :val-1 :val-2
			      :scr-trill-p :page-trill-p :indicator-text-p
			      :page-incr :scr-incr :int-feedback-p :scroll-p
			      :format-string :indicator-font :visible))
  
   ;; Customizable slots
   ;;
   (:left 0)(:top 0)(:height 250)
   (:min-width 20)   ; Overridden by :indicator-width if indicator-text
                     ; doesn't fit.  
   (:val-1 0)
   (:val-2 100)
   (:scr-trill-p T)
   (:page-trill-p T)
   (:indicator-text-p T)
   (:page-incr 5)
   (:scr-incr 1)
   (:int-feedback-p T)
   (:scroll-p T)
   (:selection-function NIL)
   (:format-string "~a")
   (:indicator-font (opal:get-standard-font NIL NIL :small))

   ;; Generally non-customizable slots
   ;;
   (:value (o-formula (inter:Clip-and-Map (second (gvl :indicator :box))
					  (gvl :bound-top)
					  (- (gvl :bound-bottom)
					     (gvl :indicator :height) 2)
					  (gvl :val-1)
					  (gvl :val-2))))
   (:widest-value-width
    (o-formula (max (opal:string-width (gvl :indicator-font)
				       (format NIL (gvl :format-string)
					       (gvl :val-1)))
		    (opal:string-width (gvl :indicator-font)
				       (format NIL (gvl :format-string)
					       (gvl :val-2))))))
   (:indicator-width (o-formula (if (gvl :indicator-text-p)
				     (gvl :widest-value-width)
				     0)))  ; zero ensures use of :min-width
   (:trill-height (o-formula (gvl :bound-width)))
   (:bottom (o-formula (+ (gvl :top) (gvl :height))))
   (:num-trills (o-formula (+ (if (gvl :scr-trill-p) 1 0)
			     (if (gvl :page-trill-p) 1 0))))
   (:trill-box-left (o-formula (gvl :left)))
   (:top-scr-top (o-formula (gvl :top)))
   (:top-page-top (o-formula (if (gvl :scr-trill-p)
				(+ (gvl :top) (gvl :trill-height))
				(gvl :top))))
   (:bot-page-top (o-formula (if (gvl :scr-trill-p)      ;; The top of the 
				(- (gvl :bottom)         ;; bottom page box
				   (* 2 (gvl :trill-height)))
				(- (gvl :bottom) (gvl :trill-height)))))
   (:bot-scr-top (o-formula (- (gvl :bottom) (gvl :trill-height))))
   (:bound-left (o-formula (gvl :left)))
   (:bound-top (o-formula (+ (gvl :top) (* (gvl :num-trills)
					  (gvl :trill-height)))))
   (:bound-bottom (o-formula (- (gvl :bottom) (* (gvl :num-trills)
						(gvl :trill-height)))))
   (:bound-height (o-formula (- (gvl :bound-bottom) (gvl :bound-top))))
   (:bound-width (o-formula (if (and (gvl :indicator-text-p)
				     (> (gvl :indicator-width)
					(gvl :min-width)))
				(gvl :indicator-width)
				(gvl :min-width))))
   (:width (o-formula (gvl :bound-width)))
   (:parts
    `((:BOUNDING-AREA ,bound-box
		      (:filling-style
		       ,(o-formula (let ((p (kr-path 0 :parent)))
				     (if (and (gv p :scroll-p)
					      (/= (gv p :val-1) (gv p :val-2)))
					 opal:gray-fill
					 opal:white-fill)))))
      (:TOP-SCR-TRILL ,top-scr-trill)
      (:BOT-SCR-TRILL ,bot-scr-trill)
      (:TOP-PAGE-TRILL ,top-page-trill)
      (:BOT-PAGE-TRILL ,bot-page-trill)
      (:INDICATOR ,v-indicator-box
          (:box (0 0 0 0)))
      (:INDICATOR-TEXT ,indicator-text)
      (:INT-FEEDBACK ,v-indicator-box
	  (:box (0 0 0 0))
	  (:top ,(o-formula (second (gvl :box))))
	  (:line-style ,opal:line-2)
	  (:filling-style NIL)
	  (:visible ,(o-formula (gv-local (gv :self) :obj-over))))))
   (:interactors
    `((:SLIDE ,slide-inter
	      (:attach-point :where-hit))
      (:JUMP ,jump-inter)
      (:WHEEL-UP ,wheel-up-inter)
      
      (:WHEEL-DOWN ,wheel-down-inter
		   (:extra-function ,#'val-2-WHEEL-fn)))))



;;;  DEMO FUNCTION
;;

#+garnet-test (defparameter v-scroll-win NIL)
#+garnet-test (defparameter v-scroll-top-agg NIL)
#+garnet-test (defparameter v-scroll-obj NIL)

#+garnet-test 
(defun V-Scroll-Go ()

  (create-instance 'v-scroll-win inter:interactor-window
		   (:left 700) (:top 10) (:width 200) (:height 300))

  (s-value v-scroll-win
	   :aggregate
	   (create-instance 'v-scroll-top-agg opal:aggregate
			    (:overlapping T)))

  (create-instance 'v-scroll-obj v-scroll-bar (:left 60) (:top 30))
  (opal:add-components v-scroll-top-agg v-scroll-obj)

  (format t "1)  Drag the indicator with the left mouse button.~%")
  (format t "2)  Click the left mouse button in the scroll bar background~%")
  (format t "    to cause the indicator to jump to mouse location.~%")
  (format t "3)  Click the left mouse button in the trill boxes to move the~%")
  (format t "    indicator by :scr-incr and :page-incr increments.~%")
  (format t "4)  Text centered in the indicator changes to reflect new~%")
  (format t "    indicator position.~%")

  (opal:update v-scroll-win))

#+garnet-test 
(defun V-Scroll-Stop ()
  (opal:destroy v-scroll-win))
