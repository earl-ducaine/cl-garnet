;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;

;;; $Id$
;;


;;;  Motif Horizontal Scroll Bar
;; 
;;   Features and operation of the horizontal scroll bar:
;;      1)  Drag the indicator with the left mouse button.
;;      2)  Click the left mouse button in the scroll bar trough to cause
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
;;      2)  Scr-trill-p  --  Whether to have arrow trill boxes that increment
;;                           by :scr-incr
;;      3)  Val-1, Val-2  --  Range of values the indicator spans.
;;                            Val-1 corresponds to the left of the scroll bar.
;;      4)  Scr-Incr  --  Value to increment position by with the trill arrows.
;;      5)  Page-Incr  --  Value to increment postion by when mouse is clicked
;;                         in trough.
;;      6)  Percent-Visible -- Percent of the scrollbar that should be occupied
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
;;      in the window.  To run it, enter (GARNET-GADGETS:motif-h-scroll-go).
;;      To stop, enter (GARNET-GADGETS:motif-h-scroll-stop).
;; 
;;   Written by Andrew Mickish


;;;  CHANGE LOG:
;;   12/15/92  Andrew Mickish - Added type and parameter declarations
;;   06/24/92  Andrew Mickish - Changed :JUMP interactor to be MOTIF-JUMP,
;;               implemented auto-repeat for trill buttons
;;   02/11/92  Andrew Mickish - Added :maybe-constant list
;;   03/01/91  Andrew Mickish - Created


(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Motif-H-Scroll-Bar))
  #+garnet-test
  (export '(Motif-H-Scroll-Go Motif-H-Scroll-Stop
	    Demo-Motif-H-Scroll-Bar Motif-H-Scroll-Win Motif-H-Scroll-Top-Agg)))


(create-instance 'MOTIF-H-SCROLL-BAR-LEFT-ARROW opal:aggregadget
   (:left (o-formula (gv (kr-path 0 :parent) :left-arrow-left)))
   (:top (o-formula (gv (kr-path 0 :parent) :bound-top)))
   (:width (o-formula (gvl :height)))
   (:height (o-formula (gv (kr-path 0 :parent) :bound-height)))
   (:center-y (o-formula (+ (gvl :top) (floor (- (gvl :height) 2) 2))))
   (:right (o-formula (+ (gvl :left) (- (gvl :width) 2))))
   (:bottom (o-formula (+ (gvl :top) (- (gvl :height) 2))))
   (:inc-by (o-formula (gv (kr-path 0 :parent) :scr-incr)))
   (:interim-selected NIL)
   (:visible (o-formula (and (gv (kr-path 0 :parent) :scr-trill-p)
			     (gv (kr-path 0 :parent) :visible))))
   (:parts
    `((:bottom-line ,opal:polyline
       ;; :left, :top, :width, & :height formulas obviate looking at
       ;; the :line-style slot, which frequently changes.
       (:left ,(o-formula (gv (kr-path 0 :parent) :left)))
       (:top ,(o-formula (gv (kr-path 0 :parent) :top)))
       (:width ,(o-formula (gv (kr-path 0 :parent) :width)))
       (:height ,(o-formula (gv (kr-path 0 :parent) :height)))
       (:point-list ,(o-formula (let* ((p (kr-path 0 :parent))
				       (right (gv p :right)))
				  (list (gv p :left) (gv p :center-y)
					right (gv p :bottom)
					right (gv p :top)))))
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
      (:top-line ,opal:line
       ;; :left, :top, :width, & :height formulas obviate looking at
       ;; the :line-style slot, which frequently changes.
       (:left ,(o-formula (gvl :x1)))
       (:top ,(o-formula (gvl :y2)))
       (:width ,(o-formula (gv (kr-path 0 :parent) :width)))
       (:height ,(o-formula (gv (kr-path 0 :parent) :height)))
       (:x1 ,(o-formula (gv (kr-path 0 :parent) :left)))
       (:y1 ,(o-formula (gv (kr-path 0 :parent) :center-y)))
       (:x2 ,(o-formula (gv (kr-path 0 :parent) :right)))
       (:y2 ,(o-formula (gv (kr-path 0 :parent) :top)))
       (:line-style ,(o-formula
		      (let ((p (kr-path 0 :parent :parent)))
			(if (gv (kr-path 1 :parent) :interim-selected)
			    (gv p :shadow-line-style)
			    (gv p :highlight-line-style))))))))
   (:interactors
    `((:trill ,MOTIF-TRILL))))

(create-instance 'MOTIF-H-SCROLL-BAR-RIGHT-ARROW opal:aggregadget
   (:left (o-formula (gv (kr-path 0 :parent) :right-arrow-left)))
   (:top (o-formula (gv (kr-path 0 :parent) :bound-top)))
   (:width (o-formula (gvl :height)))
   (:height (o-formula (gv (kr-path 0 :parent) :bound-height)))
   (:center-y (o-formula (+ (gvl :top) (floor (gvl :height) 2))))
   (:right (o-formula (+ (gvl :left) (- (gvl :width) 1))))
   (:bottom (o-formula (+ (gvl :top) (- (gvl :height) 1))))
   (:inc-by (o-formula (gv (kr-path 0 :parent) :scr-incr)))
   (:interim-selected NIL)
   (:visible (o-formula (and (gv (kr-path 0 :parent) :scr-trill-p)
			     (gv (kr-path 0 :parent) :visible))))
   (:parts
    `((:top-line ,opal:polyline
       ;; :left, :top, :width, & :height formulas obviate looking at
       ;; the :line-style slot, which frequently changes.
       (:left ,(o-formula (gv (kr-path 0 :parent) :left)))
       (:top ,(o-formula (gv (kr-path 0 :parent) :top)))
       (:width ,(o-formula (gv (kr-path 0 :parent) :width)))
       (:height ,(o-formula (gv (kr-path 0 :parent) :height)))
       (:point-list ,(o-formula (let* ((p (kr-path 0 :parent))
				       (left (+ 1 (gv p :left))))
				  (list left (gv p :bottom)
					left (+ (gv p :top) 1)
					(gv p :right) (gv p :center-y)))))
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
      (:bottom-line ,opal:line
       ;; :left, :top, :width, & :height formulas obviate looking at
       ;; the :line-style slot, which frequently changes.
       (:left ,(o-formula (gvl :x1)))
       (:top ,(o-formula (gvl :y2)))
       (:width ,(o-formula (gv (kr-path 0 :parent) :width)))
       (:height ,(o-formula (gv (kr-path 0 :parent) :height)))
       (:x1 ,(o-formula (+ 1 (gv (kr-path 0 :parent) :left))))
       (:y1 ,(o-formula (gv (kr-path 0 :parent) :bottom)))
       (:x2 ,(o-formula (gv (kr-path 0 :parent) :right)))
       (:y2 ,(o-formula (gv (kr-path 0 :parent) :center-y)))
       (:line-style ,(o-formula
		      (let ((p (kr-path 0 :parent :parent)))
			(if (gv (kr-path 1 :parent) :interim-selected)
			    (gv p :highlight-line-style)
			    (gv p :shadow-line-style))))))))
   (:interactors
    `((:trill ,MOTIF-TRILL
       (:extra-function ,#'val-2-fn)))))


(create-instance 'MOTIF-H-SCROLL-BAR MOTIF-GADGET-PROTOTYPE
  :declare ((:parameters :left :top :width :height :val-1 :val-2 :scr-incr
			 :page-incr :scr-trill-p :percent-visible :scroll-p
			 :keyboard-selection-p :foreground-color :value
			 :active-p :selection-function :visible)
	    (:type (number :val-1 :val-2 :scr-incr :page-incr :value)
		   (kr-boolean :scr-trill-p :scroll-p :keyboard-selection-p :active-p)
		   ((real 0 1) :percent-visible)
		   ((is-a-p opal:color) :foreground-color)
		   ((or null function symbol) :selection-function))
	    (:maybe-constant :left :top :width :height :val-1 :val-2 :scr-incr
			     :page-incr :scr-trill-p :percent-visible :scroll-p
			     :foreground-color :visible))
   (:left 0)(:top 0)(:width 200)(:height 20)
   (:val-1 0) (:val-2 100)
   (:scr-incr 1) (:page-incr 5)
   (:scr-trill-p T)
   (:percent-visible .5)
   (:scroll-p T)
   (:keyboard-selection NIL)
   (:foreground-color opal:MOTIF-GRAY)
   (:value (o-formula (inter:Clip-and-Map (first (gvl :indicator :box))
					  (gvl :bound-left)
					  (- (gvl :bound-right)
					     (gvl :indicator :width) 2)
					  (gvl :val-1) (gvl :val-2))))

   (:right (o-formula (+ (gvl :left) (gvl :width))))
   (:left-arrow-left (o-formula (+ 5 (gvl :left))))
   (:right-arrow-left (o-formula (- (gvl :right) (gvl :bound-height) 6)))

   (:bound-left (o-formula (+ 5 (gvl :left) (if (gvl :scr-trill-p)
						(gvl :bound-height) 0))))
   (:bound-top (o-formula (+ 4 (gvl :top))))
   (:bound-width (o-formula (- (gvl :bound-right) (gvl :bound-left))))
   (:bound-height (o-formula (- (gvl :height) 8)))
   (:bound-right (o-formula (- (gvl :right) (if (gvl :scr-trill-p)
						(gvl :height) 5))))
   (:active-p T)
   
   (:parts
    `((:BORDER ,MOTIF-BOX
            (:constant (:depressed-p))
	    (:left ,(o-formula (+ 2 (gv (kr-path 0 :parent) :left))))
	    (:top ,(o-formula (+ 2 (gv (kr-path 0 :parent) :top))))
	    (:width ,(o-formula (- (gv (kr-path 0 :parent) :width) 4)))
	    (:height ,(o-formula (- (gv (kr-path 0 :parent) :height) 4)))
	    (:depressed-p T))

      (:BOUNDING-AREA ,garnet-gadgets::BOUND-BOX
	    (:hit-threshold 0) (:line-style NIL) (:filling-style NIL))

      (:INDICATOR ,MOTIF-BOX
            (:constant (:depressed-p))
	    (:box (0 0 0 0))
	    (:left ,(o-formula (let* ((p (kr-path 0 :parent))
				     (val-1 (gv p :val-1))
				     (val-2 (gv p :val-2))
				     (bound-left (gv p :bound-left)))
				(if (eq val-1 val-2)
				    bound-left
				    (inter:Clip-and-Map 
				     (gv p :value)
				     val-1 val-2
				     bound-left (- (gv p :bound-right)
						   (gvl :width)))))))
	    (:top ,(o-formula (gv (kr-path 0 :parent) :bound-top)))
	    (:width ,(o-formula (MAX 6 (let ((p (kr-path 0 :parent)))
					 (round (* (gv p :bound-width)
						   (MIN 1 (gv p :percent-visible))))))))
	    (:height ,(o-formula (gv (kr-path 0 :parent) :bound-height))))

      (:LEFT-ARROW ,MOTIF-H-SCROLL-BAR-LEFT-ARROW)
      (:RIGHT-ARROW ,MOTIF-H-SCROLL-BAR-RIGHT-ARROW)
      (:SEL-BOX ,MOTIF-SELECTION-BOX
                (:obj-over ,(o-formula (gvl :parent))))))

   (:interactors
    `((:SLIDE ,garnet-gadgets::SLIDE-INTER
              (:active ,(o-formula (let ((p (gvl :operates-on)))
				     (and (gvl :window)
					  (gv p :scroll-p) (gv p :active-p)))))
              (:attach-point :where-hit)
              (:running-where T))
      (:JUMP ,MOTIF-JUMP)
      ;; WHEEL is like KEY only uses the scroll wheel events.
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
	    (:start-event (:leftarrow :rightarrow))
	    (:final-function MOTIF-KEY-TRILL-FN)))))



#+garnet-test
(defun MOTIF-H-SCROLL-GO (&key dont-enter-main-event-loop
			       not-double-buffered-p)
  (create-instance 'MOTIF-H-SCROLL-WIN inter:interactor-window
     (:double-buffered-p (not not-double-buffered-p))
     (:title "Motif Horizontal Scroll Bar")
     (:left 750)(:top 10)(:width 240)(:height 200))
  (s-value MOTIF-H-SCROLL-WIN
	   :aggregate
	   (create-instance 'MOTIF-H-SCROLL-TOP-AGG opal:aggregate))
  (create-instance 'DEMO-MOTIF-H-SCROLL-BAR MOTIF-H-SCROLL-BAR
     (:left 20)(:top 90)
     (:keyboard-selection-p T))
  (opal:add-components MOTIF-H-SCROLL-TOP-AGG
		       (create-instance NIL MOTIF-BACKGROUND)
		       DEMO-MOTIF-H-SCROLL-BAR)
  (opal:update MOTIF-H-SCROLL-WIN)
  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop)))

#+garnet-test
(defun MOTIF-H-SCROLL-STOP ()
  (opal:destroy MOTIF-H-SCROLL-WIN))
