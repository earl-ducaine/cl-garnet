;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.  If you are using this code or any part of Garnet,       ;;
;;  please contact garnet@cs.cmu.edu to be put on the mailing list.  ;;
;;*******************************************************************;;

;;; $Id$
;;


;;;  Motif Vertical Scroll Bar
;; 
;;   Features and operation of the vertical scroll bar:
;;      1)  Drag the indicator with the left mouse button.
;;      2)  Click the left mouse button in the scroll bar background to cause
;;          the indicator to jump by :page-incr increments.
;;      3)  Click the left mouse button in the trill boxes to move the
;;          indicator by :scr-incr increments.
;;      4)  The top level :value slot is the position of the indicator.
;;          This slot may be set directly and formulae may depend on it.
;;      5)  The function specified in :selection-function will be executed
;;          when the :values slot changes.
;; 
;;   Customizable slots:
;;      1)  Left, top, width, height
;;      2)  Scr-trill-p  --  Whether to arrow trill boxes that increment by
;;                           :scr-incr.
;;      3)  Val-1, Val-2  --  Range of values the indicator spans.
;;                            Val-1 corresponds to the top of the scroll bar.
;;      4)  Scr-Incr  --  Value to increment position by with the trill arrows.
;;      5)  Page-Incr  --  Value to increment postion by when mouse is clicked
;;                         in trough.
;;      6)  Percent-Visible -- Percent of the trough that should be occupied
;;                             by the indicator (from 0 to 1).
;;      7)  Foreground-Color
;;      8)  Value -- The current value chosen by the user.
;;      9)  Scroll-p -- Whether to allow scrolling.
;;     10)  Keyboard-selection-p -- Whether the keyboard interactor should
;;             operate on the scroll bar.
;;     11)  Selection-function -- Function executed whenever :value changes.
;; 
;;   NOTE:  This module requires schemata defined in Motif-Parts.
;; 
;;   Motif vertical scroll bar demo:
;;      This module contains a function which creates a window and a scroll bar
;;      in the window.  To run it, enter (GARNET-GADGETS:motif-v-scroll-go).
;;      To stop, enter (GARNET-GADGETS:motif-v-scroll-stop).
;; 
;;   Written by Andrew Mickish


;;;  CHANGE LOG:
;;   12/15/92  Andrew Mickish - Added type and parameter declarations
;;   06/24/92  Andrew Mickish - Changed :JUMP interactor to be MOTIF-JUMP
;;   05/29/92  Brad Myers - changed for auto-repeat button interactor
;;   02/11/92  Andrew Mickish - Added :maybe-constant list
;;   03/01/91  Andrew Mickish - Created


(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Motif-V-Scroll-Bar))
  #+garnet-test
  (export '(Motif-V-Scroll-Go Motif-V-Scroll-Stop
	    Demo-Motif-V-Scroll-Bar Motif-V-Scroll-Win Motif-V-Scroll-Top-Agg)))

(create-instance 'MOTIF-V-SCROLL-BAR-UP-ARROW opal:aggregadget
   (:left (o-formula (+ 1 (gv (kr-path 0 :parent) :bound-left))))
   (:top (o-formula (gv (kr-path 0 :parent) :up-arrow-top)))
   (:width (o-formula (- (gv (kr-path 0 :parent) :bound-width) 1)))
   (:height (o-formula (gvl :width)))
   (:center-x (o-formula (+ (gvl :left) (floor (- (gvl :width) 1) 2))))
   (:right (o-formula (+ (gvl :left) (- (gvl :width) 1))))
   (:bottom (o-formula (+ (gvl :top) (- (gvl :height) 1))))
   (:inc-by (o-formula (gv (kr-path 0 :parent) :scr-incr)))
   (:interim-selected NIL)
   (:visible (o-formula (and (gv (kr-path 0 :parent) :scr-trill-p)
			      (gv (kr-path 0 :parent) :visible))))
   (:parts
    `((:right-line ,opal:polyline
       ;; :left, :top, :width, & :height formulas obviate looking at
       ;; the :line-style slot, which frequently changes.
       (:left ,(o-formula (gv (kr-path 0 :parent) :left)))
       (:top ,(o-formula (gv (kr-path 0 :parent) :top)))
       (:width ,(o-formula (gv (kr-path 0 :parent) :width)))
       (:height ,(o-formula (gv (kr-path 0 :parent) :height)))
       (:point-list ,(o-formula (let* ((p (kr-path 0 :parent))
				       (bottom (gv p :bottom)))
				  (list (gv p :center-x) (gv p :top)
					(- (gv p :right) 1) bottom
					(gv p :left) bottom))))
       (:line-style ,(o-formula
		      (let ((p (kr-path 0 :parent :parent)))
			(if (gv (kr-path 1 :parent) :interim-selected)
			    (gv p :highlight-line-style)
			    (gv p :shadow-line-style)))))
       (:filling-style ,(o-formula
			 (let* ((p (kr-path 0 :parent :parent)))
			   (if (gv (kr-path 1 :parent) :interim-selected)
			       (gv p :background-fill)
			       (gv p :foreground-fill))))))
      (:left-line ,opal:line
       (:x1 ,(o-formula (gv (kr-path 0 :parent) :center-x)))
       (:y1 ,(o-formula (gv (kr-path 0 :parent) :top)))
       (:x2 ,(o-formula (gv (kr-path 0 :parent) :left)))
       (:y2 ,(o-formula (gv (kr-path 0 :parent) :bottom)))
       ;; :left, :top, :width, & :height formulas obviate looking at
       ;; the :line-style slot, which frequently changes.
       (:left ,(o-formula (gvl :x2)))
       (:top ,(o-formula (gvl :y1)))
       (:width ,(o-formula (gv (kr-path 0 :parent) :width)))
       (:height ,(o-formula (gv (kr-path 0 :parent) :height)))
       (:line-style ,(o-formula
		      (let ((p (kr-path 0 :parent :parent)))
			(if (gv (kr-path 1 :parent) :interim-selected)
			    (gv p :shadow-line-style)
			    (gv p :highlight-line-style))))))))
   (:interactors
    `((:trill ,MOTIF-TRILL))))

(create-instance 'MOTIF-V-SCROLL-BAR-DOWN-ARROW opal:aggregadget
   (:left (o-formula (gv (kr-path 0 :parent) :bound-left)))
   (:top (o-formula (gv (kr-path 0 :parent) :down-arrow-top)))
   (:width (o-formula (gv (kr-path 0 :parent) :bound-width)))
   (:height (o-formula (gvl :width)))
   (:center-x (o-formula (+ (gvl :left) (floor (gvl :width) 2))))
   (:right (o-formula (+ (gvl :left) (- (gvl :width) 1))))
   (:bottom (o-formula (+ (gvl :top) (- (gvl :height) 1))))
   (:inc-by (o-formula (gv (kr-path 0 :parent) :scr-incr)))
   (:interim-selected NIL)
   (:visible (o-formula (and (gv (kr-path 0 :parent) :scr-trill-p)
			     (gv (kr-path 0 :parent) :visible))))
   (:parts
    `((:left-line ,opal:polyline
       ;; :left, :top, :width, & :height formulas obviate looking at
       ;; the :line-style slot, which frequently changes.
       (:left ,(o-formula (gv (kr-path 0 :parent) :left)))
       (:top ,(o-formula (gv (kr-path 0 :parent) :top)))
       (:width ,(o-formula (gv (kr-path 0 :parent) :width)))
       (:height ,(o-formula (gv (kr-path 0 :parent) :height)))
       (:point-list ,(o-formula (let* ((p (kr-path 0 :parent))
				       (top (+ (gv p :top) 1)))
				  (list (gv p :center-x) (gv p :bottom)
					(+ 1 (gv p :left)) top
					(gv p :right) top))))
       (:line-style ,(o-formula
		      (let ((p (kr-path 0 :parent :parent)))
			(if (gv (kr-path 1 :parent) :interim-selected)
			    (gv p :shadow-line-style)
			    (gv p :highlight-line-style)))))
       (:filling-style ,(o-formula
			 (let* ((p (kr-path 0 :parent :parent)))
			   (if (gv (kr-path 1 :parent) :interim-selected)
			       (gv p :background-fill)
			       (gv p :foreground-fill))))))
      (:right-line ,opal:line
       (:x1 ,(o-formula (gv (kr-path 0 :parent) :right)))
       (:y1 ,(o-formula (+ 1 (gv (kr-path 0 :parent) :top))))
       (:x2 ,(o-formula (gv (kr-path 0 :parent) :center-x)))
       (:y2 ,(o-formula (gv (kr-path 0 :parent) :bottom)))
       ;; :left, :top, :width, & :height formulas obviate looking at
       ;; the :line-style slot, which frequently changes.
       (:left ,(o-formula (gvl :x2)))
       (:top ,(o-formula (gvl :y1)))
       (:width ,(o-formula (gv (kr-path 0 :parent) :width)))
       (:height ,(o-formula (gv (kr-path 0 :parent) :height)))
       (:line-style ,(o-formula
		      (let ((p (kr-path 0 :parent :parent)))
			(if (gv (kr-path 1 :parent) :interim-selected)
			    (gv p :highlight-line-style)
			    (gv p :shadow-line-style))))))))
   (:interactors
    `((:trill ,MOTIF-TRILL
       (:extra-function ,#'val-2-fn)))))



(create-instance 'MOTIF-V-SCROLL-BAR MOTIF-GADGET-PROTOTYPE
  :declare ((:parameters :left :top :width :height :val-1 :val-2
			 :scr-incr :page-incr :scr-trill-p :percent-visible
			 :scroll-p :keyboard-selection-p :foreground-color
			 :value :active-p :selection-function :visible)
	    (:type (number :val-1 :val-2 :scr-incr :page-incr :value)
		   (kr-boolean :scr-trill-p :scroll-p :keyboard-selection-p :active-p)
		   ((real 0 1) :percent-visible)
		   ((is-a-p opal:color) :foreground-color)
		   ((or null function symbol) :selection-function))
	    (:maybe-constant :left :top :width :height :val-1 :val-2 :scr-incr
			     :page-incr :scr-trill-p :percent-visible :scroll-p
			     :foreground-color :visible))
   (:left 0)(:top 0)(:width 20)(:height 200)
   (:val-1 0) (:val-2 100)
   (:scr-incr 1) (:page-incr 5)
   (:scr-trill-p T)
   (:percent-visible .5)
   (:scroll-p T)
   (:keyboard-selection-p NIL)
   (:foreground-color opal:MOTIF-GRAY)
   (:value (o-formula (inter:Clip-and-Map (second (gvl :indicator :box))
					  (gvl :bound-top)
					  (- (gvl :bound-bottom)
					     (gvl :indicator :height) 2)
					  (gvl :val-1) (gvl :val-2))))
   
   (:bottom (o-formula (+ (gvl :top) (gvl :height))))
   (:up-arrow-top (o-formula (+ 5 (gvl :top))))
   (:down-arrow-top (o-formula (- (gvl :bottom) (gvl :bound-width) 6)))

   (:bound-left (o-formula (+ 4 (gvl :left))))
   (:bound-top (o-formula (+ 5 (gvl :top) (if (gvl :scr-trill-p)
					      (gvl :bound-width) 0))))
   (:bound-width (o-formula (- (gvl :width) 8)))
   (:bound-height (o-formula (- (gvl :bound-bottom) (gvl :bound-top))))
   (:bound-bottom (o-formula (- (gvl :bottom) (if (gvl :scr-trill-p)
						  (gvl :width) 5))))
   (:active-p T)

   ;; All auxiliary color slots are defined in MOTIF-GADGET-PROTOTYPE
   
   (:parts
    `((:BORDER ,MOTIF-BOX
            (:constant (:depressed-p))
	    (:left ,(o-formula (+ 2 (gv (kr-path 0 :parent) :left))))
	    (:top ,(o-formula (+ 2 (gv (kr-path 0 :parent) :top))))
	    (:width ,(o-formula (- (gv (kr-path 0 :parent) :width) 4)))
	    (:height ,(o-formula (- (gv (kr-path 0 :parent) :height) 4)))
	    (:depressed-p T))

      (:BOUNDING-AREA ,BOUND-BOX
	    (:hit-threshold 0) (:line-style NIL) (:filling-style NIL))

      (:INDICATOR ,MOTIF-BOX
            (:constant (:depressed-p))
	    (:box (0 0 0 0))
	    (:left ,(o-formula (gv (kr-path 0 :parent) :bound-left)))
	    (:top ,(o-formula (let* ((p (kr-path 0 :parent))
				     (val-1 (gv p :val-1))
				     (val-2 (gv p :val-2))
				     (bound-top (gv p :bound-top)))
				(if (/= val-1 val-2)
				    (inter:Clip-and-Map 
				     (gv p :value)
				     val-1 val-2
				     bound-top (- (gv p :bound-bottom)
						  (gvl :height)))
				    (opal:gv-center-y-is-center-of (gv p))))))
	    (:width ,(o-formula (gv (kr-path 0 :parent) :bound-width)))
	    (:height ,(o-formula (MAX 6 (let ((p (kr-path 0 :parent)))
					  (round (* (gv p :bound-height)
						    (MIN 1 (gv p :percent-visible)))))))))
      (:UP-ARROW ,MOTIF-V-SCROLL-BAR-UP-ARROW)
      (:DOWN-ARROW ,MOTIF-V-SCROLL-BAR-DOWN-ARROW)
      (:SEL-BOX ,MOTIF-SELECTION-BOX
                (:obj-over ,(o-formula (gvl :parent))))))

   (:interactors
    `((:SLIDE ,SLIDE-INTER
              (:active ,(o-formula (let ((p (gvl :operates-on)))
				     (and (gvl :window)
					  (gv p :scroll-p) (gv p :active-p)))))
              (:running-where T))
      (:JUMP ,MOTIF-JUMP
	     (:final-function
	      ,#'(lambda (interactor obj)
		   (MOTIF-JUMP-FN interactor (inter:event-y inter:*current-event*)
				  (g-value interactor :operates-on :indicator :top))
		   (SLIDE-FINAL-FN interactor obj))))
      ;; WHEEL is like KEY only it uses the scrollwheel events.
      (:WHEEL ,inter:scroll-wheel-interactor
	      (:active ,(o-formula (let ((p (gvl :operates-on)))
				     (and (gvl :window) 
					  (gv p :scroll-p) (gv p :active-p)))))
	      (:window ,(o-formula (gv-local :self :operates-on :window)))
	      (:continuous nil)
	      (:start-where ,(o-formula (list :in-box (gvl :operates-on :bounding-area))))
	      (:start-event (:upscrollup :downscrollup))
	      (:final-function MOTIF-KEY-TRILL-FN))
      (:KEY ,inter:button-interactor
	    (:active ,(o-formula (let ((p (gvl :operates-on)))
				   (and (gvl :window) 
					(gv p :scroll-p) (gv p :active-p)
					(gv p :keyboard-selection-p)))))
	    (:window ,(o-formula (gv-local :self :operates-on :window)))
	    (:continuous NIL)
	    (:start-where T)
	    (:start-event (:uparrow :downarrow))
	    (:final-function MOTIF-KEY-TRILL-FN)))))
			 


;;;  DEMO FUNCTION
;;;

#+garnet-test
(defun MOTIF-V-SCROLL-GO (&key dont-enter-main-event-loop
			       not-double-buffered-p)
  (create-instance 'MOTIF-V-SCROLL-WIN inter:interactor-window
     (:double-buffered-p (not not-double-buffered-p))
     (:title "Motif Vertical Scroll Bar")
     (:left 800)(:top 10)(:width 200)(:height 240))
  (s-value MOTIF-V-SCROLL-WIN
	   :aggregate
	   (create-instance 'MOTIF-V-SCROLL-TOP-AGG opal:aggregate))
  (create-instance 'DEMO-MOTIF-V-SCROLL-BAR MOTIF-V-SCROLL-BAR
     (:left 90)(:top 20)
     (:keyboard-selection-p T)
     ;; Override interactors so that the scroll wheel can work in the
     ;; entire window.
     (:interactors
      `((:slide ,slide-inter
		(:active ,(o-formula (let ((p (gvl :operates-on)))
				       (and (gvl :window)
					    (gv p :scroll-p) (gv p :active-p))))))
	(:jump ,motif-jump
	       (:final-function
	      ,#'(lambda (interactor obj)
		   (MOTIF-JUMP-FN interactor (inter:event-y inter:*current-event*)
				  (g-value interactor :operates-on :indicator :top))
		   (SLIDE-FINAL-FN interactor obj))))
	(:WHEEL ,inter:button-interactor
		(:active ,(o-formula (let ((p (gvl :operates-on)))
				       (and (gvl :window) 
					    (gv p :scroll-p) (gv p :active-p)))))
		(:window ,(o-formula (gv-local :self :operates-on :window)))
		(:continuous NIL)
		(:start-where ,(o-formula (list :in motif-v-scroll-win)))
		(:start-event (:upscrollup :downscrollup))
		(:final-function MOTIF-KEY-TRILL-FN)))))

	
  (opal:add-components MOTIF-V-SCROLL-TOP-AGG
		       (create-instance NIL MOTIF-BACKGROUND)
		       DEMO-MOTIF-V-SCROLL-BAR)
  (opal:update MOTIF-V-SCROLL-WIN)
  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop)))
  
#+garnet-test
(defun MOTIF-V-SCROLL-STOP ()
  (opal:destroy MOTIF-V-SCROLL-WIN))
