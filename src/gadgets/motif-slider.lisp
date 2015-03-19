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
;;;  Motif Slider
;;;
;;;  Features and operation of the slider:
;;;     1)  Drag the indicator with the left mouse button.
;;;     2)  Click the left mouse button in the slider trough to cause
;;;         the indicator to jump by :page-incr increments.
;;;     3)  Click the left mouse button in the trill boxes to move the
;;;         indicator by :scr-incr increments.
;;;     4)  The top level :value slot is the position of the indicator.
;;;         This slot may be set directly and formulae may depend on it.
;;;     5)  The function specified in :selection-function will be executed
;;;         when the :values slot changes.
;;;
;;;  Customizable slots:
;;;     1)  Left, top, height
;;;     2)  Trough-width
;;;     2)  Scr-trill-p  --  Whether to have single arrow trill boxes that
;;;                          increment by :scr-incr.
;;;     3)  Val-1, Val-2  --  Range of values the indicator spans.
;;;                           Val-1 corresponds to the top of the slider.
;;;     4)  Scr-Incr  --  Value to increment position by with the trill arrows.
;;;     5)  Page-Incr  --  Value to increment postion by when mouse is clicked
;;;                        in trough.
;;;     6)  Foreground-Color
;;;     7)  Indicator-Text-P -- Whether to have text beside the slider to
;;;                             report the current value.
;;;     8)  Indicator-Font -- The font for the indicator text.
;;;     9)  Text Offset -- The distance from the text to the slider.
;;;    10)  Value -- The current value chosen by the user.
;;;    11)  Scroll-p -- Whether to allow scrolling.
;;;    12)  Keyboard-selection-p -- Whether the keyboard interactor should
;;;            operate on the button (or button panel)
;;;    13)  Selection-function -- Function executed whenever :value changes.
;;;
;;;  NOTE:  This module requires schemata defined in Motif-Parts.
;;;
;;;  Motif Slider Demo:
;;;     This module contains a function which creates a window and a slider
;;;     in the window.
;;;     To run it, enter (GARNET-GADGETS:motif-slider-go).
;;;     To stop, enter (GARNET-GADGETS:motif-slider-stop).
;;;
;;;  Written by Andrew Mickish

;;;  CHANGE LOG:
;;;  05/20/93  Andrew Mickish - Added :format-string to :maybe-constant list
;;;  12/15/92  Andrew Mickish - Added type and parameter declarations
;;;  09/07/92  Andrew Mickish - Added :format-string
;;;  06/17/92  Andrew Mickish - Added :line-style to indicator text
;;;  02/11/92  Andrew Mickish - Added :maybe-constant list
;;;  03/01/91  Andrew Mickish - Created
;;;

(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Motif-Slider))
  #+garnet-test
  (export '(Motif-Slider-Go Motif-Slider-Stop
	    Demo-Motif-Slider Motif-Slider-Win Motif-Slider-Top-Agg)))


(create-instance 'MOTIF-SLIDER MOTIF-V-SCROLL-BAR
  :declare ((:parameters :left :top :height :trough-width :val-1 :val-2
			 :scr-incr :page-incr :scr-trill-p :text-offset
			 :scroll-p :indicator-text-p :keyboard-selection-p
			 :indicator-font :foreground-color :value :active-p
			 :format-string :selection-function :visible)
	    (:type (number :val-1 :val-2 :scr-incr :page-incr :value)
		   (integer :text-offset)
		   (kr-boolean :scr-trill-p :scroll-p :keyboard-selection-p
		    :active-p :indicator-text-p)
		   #+(or lucid allegro-V3.1) (number :percent-visible)
		   #-(or lucid allegro-V3.1) ((real 0 1) :percent-visible)
		   ((is-a-p opal:color) :foreground-color)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
		    :indicator-font)
		   (string :format-string)
		   ((or null function symbol) :selection-function))
	    (:maybe-constant :left :top :height :trough-width :val-1 :val-2
			     :scr-incr :page-incr :scr-trill-p :text-offset
			     :scroll-p :indicator-text-p :indicator-font
			     :foreground-color :format-string :visible))
   (:left 0)(:top 0)(:height 200)(:trough-width 16)
   (:val-1 0) (:val-2 100)
   (:scr-incr 1) (:page-incr 5)
   (:scr-trill-p NIL)
   (:text-offset 5)
   (:scroll-p T)
   (:keyboard-selection-p NIL)
   (:indicator-text-p T)
   (:indicator-font opal:default-font)
   (:format-string "~a")
   (:foreground-color opal:MOTIF-GRAY)
   (:value (o-formula (inter:Clip-and-Map (second (gvl :indicator :box))
					  (gvl :bound-top)
					  (- (gvl :bound-bottom)
					     (gvl :indicator :height) 2)
					  (gvl :val-1) (gvl :val-2))))

   (:widest-value-width
    (o-formula (max (opal:string-width (gvl :indicator-font)
				       (format NIL (gvl :format-string)
					       (gvl :val-1)))
		    (opal:string-width (gvl :indicator-font)
				       (format NIL (gvl :format-string)
					       (gvl :val-2))))))
   (:bound-left (o-formula (+ 4 (gvl :left)
			      (if (gvl :indicator-text-p)
				  (+ (gvl :text-offset)
				     (gvl :widest-value-width))
				  0))))
   (:bound-width (o-formula (- (gvl :trough-width) 4)))
   (:bound-bottom (o-formula (- (gvl :bottom) (if (gvl :scr-trill-p)
						  (gvl :trough-width) 5))))
				
   (:width (o-formula (+ 4 (gvl :trough-width)
			 (if (gvl :indicator-text-p)
			     (+ (gvl :widest-value-width) (gvl :text-offset))
			     0))))
   (:active-p T)
   (:indicator-height (o-formula (MAX 6 (round (* .25 (gvl :bound-height))))))
   (:indicator-center-y (o-formula (+ (gvl :indicator :top)
				      (floor (gvl :indicator-height) 2))))
   (:parts
    `((:border :modify
       (:left ,(o-formula (let ((p (kr-path 0 :parent)))
			    (+ 2 (gv p :left)
			       (if (gv p :indicator-text-p)
				   (+ (gv p :widest-value-width)
				      (gv p :text-offset)) 0)))))
       (:width ,(o-formula (gv (kr-path 0 :parent) :trough-width))))

      :bounding-area

      (:indicator :modify
       (:height ,(o-formula (gv (kr-path 0 :parent) :indicator-height)))
       (:parts
	(:top-box :bottom-box
	 (:gray-box :modify
		    (:filling-style ,opal:black-fill)))))

      (:indicator-highlight-bar ,opal:line
       (:x1 ,(o-formula (+ 2 (gv (kr-path 0 :parent) :bound-left))))
       (:y1 ,(o-formula (gv (kr-path 0 :parent) :indicator-center-y)))
       (:x2 ,(o-formula (- (+ (gv (kr-path 0 :parent) :bound-left)
			      (gv (kr-path 0 :parent) :bound-width)) 2)))
       (:y2 ,(o-formula (gvl :y1)))
       (:line-style ,(o-formula (gv (kr-path 0 :parent) :highlight-line-style))))

      (:indicator-shadow-bar ,opal:line
       (:x1 ,(o-formula (+ 2 (gv (kr-path 0 :parent) :bound-left))))
       (:y1 ,(o-formula (+ 2 (gv (kr-path 0 :parent) :indicator-center-y))))
       (:x2 ,(o-formula (- (+ (gv (kr-path 0 :parent) :bound-left)
			      (gv (kr-path 0 :parent) :bound-width)) 2)))
       (:y2 ,(o-formula (gvl :y1)))
       (:line-style ,(o-formula (gv (kr-path 0 :parent) :shadow-line-style))))

      (:text ,opal:text
       (:constant (:actual-heightp))
       (:left ,(o-formula (let ((p (gv (kr-path 0 :parent))))
			    (- (+ 2 (gv p :left) (gv p :widest-value-width))
			       (gvl :width)))))
       (:top ,(o-formula (- (gv (kr-path 0 :parent) :indicator-center-y)
			    (floor (gvl :height) 2))))
       (:height ,(o-formula (opal:string-height
			     (gv (kr-path 0 :parent) :indicator-font) "X")))
       (:string ,(o-formula (format NIL (gv (kr-path 0 :parent) :format-string)
				    (gv (kr-path 0 :parent) :value))))
       (:font ,(o-formula (gv (kr-path 0 :parent) :indicator-font)))
       (:visible ,(o-formula (gv (kr-path 0 :parent) :indicator-text-p)))
       (:line-style ,(o-formula
		      (let ((p (kr-path 0 :parent)))
			(if (gv p :active-p)
			    opal:default-line-style
			    (gv p :stippled-line-style))))))
      :up-arrow :down-arrow :sel-box)))



;;;
;;;  DEMO FUNCTION
;;;

#+garnet-test
(defun MOTIF-SLIDER-GO (&key dont-enter-main-event-loop not-double-buffered-p)
  (create-instance 'MOTIF-SLIDER-WIN inter:interactor-window
     (:double-buffered-p (not not-double-buffered-p))
     (:title "Motif Slider")
     (:left 800)(:top 10)(:width 200)(:height 240))
  (s-value MOTIF-SLIDER-WIN
	   :aggregate
	   (create-instance 'MOTIF-SLIDER-TOP-AGG opal:aggregate))
  (create-instance 'DEMO-MOTIF-SLIDER MOTIF-SLIDER
     (:left 90)(:top 20)
     (:keyboard-selection-p T))
  (opal:add-components MOTIF-SLIDER-TOP-AGG
		       (create-instance NIL MOTIF-BACKGROUND)
		       DEMO-MOTIF-SLIDER)
  (opal:update MOTIF-SLIDER-WIN)
  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop)))

  
#+garnet-test
(defun MOTIF-SLIDER-STOP ()
  (opal:destroy MOTIF-SLIDER-WIN))
