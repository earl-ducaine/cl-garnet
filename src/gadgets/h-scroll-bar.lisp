;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;
;;         The Garnet User Interface Development Environment.        ;;
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;

;;; $Id$
;; 
;;  Horizontal scroll bar
;;
;;  Features and operation of the horizontal scroll bar:
;;     1)  Drag the indicator with the left mouse button
;;     2)  Click the left mouse button in the scroll bar background to cause
;;         the indicator to jump to mouse location
;;     3)  Click the left mouse button in the trill boxes to move the
;;         indicator by :scr-incr and :page-incr increments
;;     4)  Text centered in the indicator changes to reflect new indicator
;;         position
;;     5)  The top level :value slot is the position of the indicator.
;;         This slot may be set directly and formulae may depend on it.
;;     6)  The function in :selection-function will be executed when the
;;         :value slot is updated.
;;
;;  Customizable slots:
;;     1)  Left, top, width
;;     2)  Min-height -- Will be overridden by the value calculated in
;;                       :indicator-height if the width of the text in
;;                       the indicator exceeds this width (the indicator
;;                       is constructed as a square)
;;     3)  Scr-trill-p  --  Whether to have single arrow trill boxes that
;;                          increment by :scr-incr
;;     4)  Page-trill-p --  Whether to have double arrow trill boxes that
;;                          increment by :page-incr
;;     5)  Indicator-text-p -- Whether to report indicator position
;;                             numerically inside the indicator
;;     6)  Int-feedback-p  --  Whether to follow mouse with thick outline box
;;                             instead of with indicator directly
;;     7)  Val-1, Val-2  --  Range of values the indicator spans.
;;                           Val-1 corresponds to the left of the scroll bar.
;;     8)  Scr-Incr  --  Value to increment position by in single arrow box
;;     9)  Page-incr  --  Value to increment postion by in double arrow box
;;    10)  Value -- The value currently selected by the user
;;    11)  Scroll-p -- Whether to allow scrolling
;;    12)  Selection-function -- Function executed when :value changes
;;    13)  Indicator-font -- Font to report indicator position with
;;    14)  Format-string -- formatting string of indicator value
;;
;;  NOTE:  This module requires schemata defined in GAD-scroll-parts,
;;         GAD-h-arrows, and GAD-h-boxes.
;;
;;  Horizontal scroll bar demo:
;;     This module contains a function which creates a window and a scroll bar
;;     in the window.  To run it, enter (GARNET-GADGETS:h-scroll-go).
;;     To stop, enter (GARNET-GADGETS:h-scroll-stop).
;;
;;  Designed by Brad Myers
;;  Written by Andrew Mickish


;;;  CHANGE LOG:
;;  12/14/92  Andrew Mickish - Added type and parameter declarations
;;  09/17/92  Andrew Mickish - Added :height formula to H-SCROLL-BAR
;;  04/30/92  Andrew Mickish - Called get-standard-font for fonts
;;  02/07/92  Andrew Mickish - Added :maybe-constant slots
;;  11/30/90  Pavan Reddy - added use of :format-string slot so floats work.
;;  08/01/90  Pavan Reddy - fixed divide-by-zero error that occurs when
;;     :val-1 and :val-2 slots of H-SCROLL-BAR are equal.  Fix contributed
;;     by Rod Williams.
;;  07/01/90  Andrew Mickish - Removed :box from H-INDICATOR-BOX and placed
;;     distinct values in each instance instead.  Changed :visible slot of
;;     :INT-FEEDBACK to depend on local :obj-over.
;;  01/18/90  Andrew Mickish - Changed :box of H-INDICATOR-BOX to '(0 0 0 0),
;;     Added :scroll-p slot to H-SCROLL-BAR, changed :filling-style in
;;     BOUNDING-AREA component of H-SCROLL-BAR, added :visible slot to
;;     H-INDICATOR-BOX.
;;


(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(H-Scroll-Bar))
  #+garnet-test
  (export '(H-Scroll-Go H-Scroll-Stop
	    H-Scroll-Win H-Scroll-Top-Agg H-Scroll-Obj)))


;;; Instances.


;; HORIZONTAL INDICATOR BOX
;;
(create-instance 'H-INDICATOR-BOX opal:rectangle
   (:left (o-formula (let ((p (kr-path 0 :parent)))
		       (if (/= (gv p :val-1) (gv p :val-2))
			   (inter:Clip-and-Map (gv p :value)
					       (gv p :val-1) (gv p :val-2)
					       (gv  p :bound-left)
					       (- (gv p :bound-right)
						  (gvl :width)))
			   (opal:gv-center-x-is-center-of (gv p))))))
   (:top (o-formula (+ 1 (gv (kr-path 0 :parent) :top))))
   (:height (o-formula (- (gv (kr-path 0 :parent) :bound-height) 2)))
   (:width (o-formula (gvl :height)))
   (:filling-style opal:white-fill)
   (:visible (o-formula (let ((p (kr-path 0 :parent)))
			  (if (gv p :visible)
			      (and (gv p :scroll-p)
				   (/= (gv p :val-1) (gv p :val-2))))))))


;; TOP LEVEL AGGREGADGET
;;
(create-instance 'H-SCROLL-BAR opal:aggregadget
   :declare ((:parameters :left :top :width :min-height :val-1 :val-2
			  :scr-incr :page-incr :scr-trill-p :page-trill-p
			  :indicator-text-p :int-feedback-p :scroll-p
			  :format-string :indicator-font :value
			  :selection-function :visible)
	     (:type ((integer 0) :min-height)
		    (number :val-1 :val-2 :page-incr :scr-incr :value)
		    (kr-boolean :scr-trill-p :page-trill-p
		     :indicator-text-p :int-feedback-p :scroll-p)
 		    (string :format-string)
		    ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
		     :indicator-font)
		    ((or null function symbol) :selection-function))
	     (:maybe-constant :left :top :width :min-height :val-1 :val-2
			      :scr-trill-p :page-trill-p :indicator-text-p
			      :page-incr :scr-incr :int-feedback-p :scroll-p
			      :format-string :indicator-font :visible))
  
   ;; Customizable slots
   ;;
   (:left 0)(:top 0)(:width 250)
   (:min-height 20)  ; Overridden by :indicator-height if indicator-text
                     ; doesn't fit.
   (:val-1 0)
   (:val-2 100)
   (:selection-function NIL)
   (:scr-trill-p T)
   (:page-trill-p T)
   (:indicator-text-p T)
   (:page-incr 5)
   (:scr-incr 1)
   (:int-feedback-p T)
   (:scroll-p T)
   (:format-string "~a")
   (:indicator-font (opal:get-standard-font NIL NIL :small))

   ;; Generally non-customizable slots
   ;;
   (:value (o-formula (inter:Clip-and-Map (first (gvl :indicator :box))
					  (gvl :bound-left)
					  (- (gvl :bound-right)
					     (gvl :indicator :width) 2)
					  (gvl :val-1)
					  (gvl :val-2))))
   (:widest-value-width
    (o-formula (max (opal:string-width (gvl :indicator-font)
				       (format NIL (gvl :format-string)
					       (gvl :val-1)))
		    (opal:string-width (gvl :indicator-font)
				       (format NIL (gvl :format-string)
					       (gvl :val-2))))))
   (:indicator-height (o-formula (if (gvl :indicator-text-p)
				     (gvl :widest-value-width)
				     0)))  ; zero ensures use of :min-height
   (:trill-width (o-formula (gvl :bound-height)))
   (:right (o-formula (+ (gvl :left) (gvl :width))))
   (:num-trills (o-formula (+ (if (gvl :scr-trill-p) 1 0)
			     (if (gvl :page-trill-p) 1 0))))
   (:trill-box-top (o-formula (gvl :top)))
   (:left-scr-left (o-formula (gvl :left)))
   (:left-page-left (o-formula (if (gvl :scr-trill-p)
				(+ (gvl :left) (gvl :trill-width))
				(gvl :left))))
   (:right-page-left (o-formula (if (gvl :scr-trill-p)      ;; The left of the 
				    (- (gvl :right)         ;; right page box
				   (* 2 (gvl :trill-width)))
				(- (gvl :right) (gvl :trill-width)))))
   (:right-scr-left (o-formula (- (gvl :right) (gvl :trill-width))))
   (:bound-left (o-formula (+ (gvl :left) (* (gvl :num-trills)
					  (gvl :trill-width)))))
   (:bound-top (o-formula (gvl :top)))
   (:bound-width (o-formula (- (gvl :bound-right) (gvl :bound-left))))
   (:bound-height (o-formula (if (and (gvl :indicator-text-p)
				      (> (gvl :indicator-height)
					 (gvl :min-height)))
				 (gvl :indicator-height)
				 (gvl :min-height))))
   (:bound-right (o-formula (- (gvl :right) (* (gvl :num-trills)
					      (gvl :trill-width)))))
   (:height (o-formula (gvl :bound-height)))
   
   (:parts
    `((:BOUNDING-AREA ,bound-box
		      (:filling-style
		       ,(o-formula (let ((p (kr-path 0 :parent)))
				     (if (and (gv p :scroll-p)
					      (/= (gv p :val-1) (gv p :val-2)))
					 opal:gray-fill
					 opal:white-fill)))))
      (:LEFT-SCR-TRILL ,left-scr-trill)
      (:RIGHT-SCR-TRILL ,right-scr-trill)
      (:LEFT-PAGE-TRILL ,left-page-trill)
      (:RIGHT-PAGE-TRILL ,right-page-trill)
      (:INDICATOR ,h-indicator-box
          (:box (0 0 0 0)))
      (:INDICATOR-TEXT ,indicator-text)
      (:INT-FEEDBACK ,h-indicator-box
          (:box (0 0 0 0))
	  (:left ,(o-formula (first (gvl :box))))
	  (:line-style ,opal:line-2)
	  (:filling-style NIL)
	  (:visible ,(o-formula (if (gvl :parent :visible)
				    (gv-local (gv :self) :obj-over)))))))
   (:interactors
    `((:SLIDE ,slide-inter
	      (:attach-point :where-hit))
      (:JUMP ,jump-inter
	     (:attach-point :w))
      (:WHEEL-UP ,wheel-up-inter)
      (:WHEEL-DOWN ,wheel-down-inter))))


;;;
;;  DEMO FUNCTION
;;
#+garnet-test
(defparameter h-scroll-win NIL)
#+garnet-test
(defparameter h-scroll-top-agg NIL)
#+garnet-test
(defparameter h-scroll-obj NIL)

#+garnet-test 
(defun H-Scroll-Go ()

  (create-instance 'h-scroll-win inter:interactor-window
     (:left 700) (:top 10) (:width 300) (:height 200))

  (s-value h-scroll-win
	   :aggregate
	   (create-instance 'h-scroll-top-agg opal:aggregate
	      (:overlapping T)))

  (create-instance 'h-scroll-obj h-scroll-bar (:left 10) (:top 100))
  (opal:add-components h-scroll-top-agg h-scroll-obj)

  (format t "1)  Drag the indicator with the left mouse button.~%")
  (format t "2)  Click the left mouse button in the scroll bar background~%")
  (format t "    to cause the indicator to jump to mouse location.~%")
  (format t "3)  Click the left mouse button in the trill boxes to move the~%")
  (format t "    indicator by :scr-incr and :page-incr percent.~%")
  (format t "4)  Text centered in the indicator changes to reflect new~%")
  (format t "    indicator position.~%")

  (opal:update h-scroll-win))

#+garnet-test
(defun H-Scroll-Stop ()
  (opal:destroy h-scroll-win))


