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


;;;  Vertical slider
;; 
;;   Features and operation of the vertical slider:
;;      1)  Drag the indicator with the left mouse button
;;      2)  Click the left mouse button on the slider shaft to cause the
;;          indicator to jump to mouse location
;;      3)  Click the left mouse button in the trill boxes to move the
;;          indicator by :scr-incr and :page-incr increments
;;      4)  Text above the slider shaft changes to reflect new indicator
;;          position.  This text may be edited after pressing on it with
;;          the left mouse button.
;;      5)  The top level :value slot is the position of the indicator.
;;          This slot may be set directly and formulae may depend on it.
;;      6)  The function specified in :selection-function will be executed
;;          whenever the value in :value changes.
;; 
;;   Customizable slots:
;;      1)  Left, top, height
;;      2)  Shaft-width
;;      3)  Scr-trill-p  --  Whether to have trills that incr by :scr-incr
;;      4)  Page-trill-p --  Whether to have trills that incr by :page-incr
;;      5)  Scr-Incr, Page-incr  --  Values to increment postion by in single
;;                                     and double arrow boxes, respectively
;;      6)  Val-1, Val-2  --  Range of values the indicator spans.
;;                            Val-1 corresponds to the top of the slider.
;;      7)  Value -- The value currently selected by the user.
;;      8)  Selection-function -- Function to be executed when :value changes.
;;      9)  Tic-marks-p -- Whether to put tic marks on the shaft
;;     10)  Enumerate-p -- Whether to enumerate the tic marks
;;     11)  Num-marks  --  The number of tic marks (including top and bottom)
;;     12)  Value-feedback-p  --  Whether to report indicator position above
;;                                the shaft
;;     13)  Scroll-p -- Whether to allow movement of indicator
;;     14)  Value-feedback-font -- Font to report indicator position with
;;     15)  Enum-font --  Font to enumerate tic marks with
;;     16)  Format-string -- Formatting string of indicator value
;;     17)  Enum-format-string -- Formatting string of enumeration
;; 
;;   NOTE:  This module requires schemata defined in GAD-scroll-parts,
;;          GAD-slider-parts, GAD-v-arrows, and GAD-v-boxes.
;; 
;;   Vertical slider demo:
;;      This module contains a function which creates a window and a slider
;;      in the window.  To run it, enter (GARNET-GADGETS:v-slider-go).
;;      To stop, enter (GARNET-GADGETS:v-slider-stop).
;; 
;;   Designed by Brad Myers
;;   Written by Andrew Mickish


;;;  CHANGE LOG:
;;   05/30/94  Marty Geier - changed window position in demo
;;   12/01/93 Andrew Mickish - Referenced parents' :visible slots in formulas
;;   05/26/93  Andrew Mickish - Fixed constant declarations for new aggrelists
;;   12/14/92  Andrew Mickish - Added type and parameter declarations
;;   09/17/92  Andrew Mickish - Added :left, :width, and :height values to
;;             V-SLIDER-INDICATOR to reduce reevaluations of top-level :width
;;   09/08/92  Mickish/Duchier - Rewrote :enum-width formula to look at every
;;             tic mark value.
;;   04/29/92  Andrew Mickish - Got enum-font from get-standard-font
;;   02/07/92  Andrew Mickish - Added :maybe-constant slots
;;   02/15/91  Andrew Mickish - Changed :string slot of TIC-MARKS's
;;   11/30/90  Pavan Reddy - made appropriate modifications so :format-string
;;             and :enum-format-string are used to format numbers in order to
;;             use of floats.
;;   11/21/90  Andrew Mickish - Moved indicator to be on top of tic marks
;;   06/18/90  Andrew Mickish - Moved "v-slider-parts.lisp" into this file
;;   01/18/90  Andrew Mickish - Added :scroll-p to V-SLIDER
;;             :item-prototype to eliminate round-off error.


(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(V-Slider))
  #+garnet-test
  (export '(V-Slider-Go V-Slider-Stop
	    V-Slider-Win V-Slider-Top-Agg V-Slider-Obj)))


(create-instance 'V-SLIDER-INDICATOR opal:arrowhead
   (:constant '(:length :diameter :line-style :width :height))
   (:box '(0 0 0 0))
   (:length 10) (:diameter 15)
   (:left (o-formula (gvl :head-x)))
   (:width 10) (:height 15)
   (:from-x (o-formula (gv (kr-path 0 :parent) :right)))
   (:from-y (o-formula (let ((p (kr-path 0 :parent)))
			 (inter:Clip-and-Map
			  (gv p :value) (gv p :val-1) (gv p :val-2)
			  (gv p :shaft-top) (gv p :shaft-bottom)))))
   (:head-x (o-formula (+ (gv (kr-path 0 :parent) :shaft-line-left) 2)))
   (:head-y (o-formula (gvl :from-y)))
   (:line-style NIL)
   (:filling-style opal:black-fill)
   (:fast-redraw-p T)
   (:draw-function :xor))

(create-instance 'V-SLIDER-VALUE-FEEDBACK opal:aggregadget
   (:left (o-formula (- (gv (kr-path 0 :parent) :shaft-line-left)
			(floor (gvl :width) 2))))
   (:top (o-formula (gv (kr-path 0 :parent) :top)))
   (:width (o-formula (gv (kr-path 0 :parent) :value-feedback-width)))
   (:height (o-formula (gv (kr-path 0 :parent) :value-feedback-height)))
   (:font (o-formula (gv (kr-path 0 :parent) :value-feedback-font)))
   (:visible (o-formula (if (gv (kr-path 0 :parent) :visible)
			    (gv (kr-path 0 :parent) :value-feedback-p))))
   (:parts
    `((:value-rect ,value-rect)
      (:value-text ,value-text
       (:cursor-index NIL))))
   (:interactors
    `((:value-inter ,value-inter))))

(create-instance 'V-SLIDER-TIC-MARKS opal:aggrelist
   (:constant '(:fixed-width-p :fixed-height-p :fixed-width-size
		:fixed-height-size :direction :h-spacing :v-spacing
		:indent :rank-margin :pixel-margin))
   (:left (o-formula (gvl :parent :left)))
   (:shaft-left (o-formula (gvl :parent :shaft-left)))
   (:top (o-formula (gvl :parent :shaft-top)))
   (:right (o-formula (gvl :parent :right)))
   (:bottom (o-formula (gvl :parent :shaft-bottom)))
   (:width (o-formula (- (gvl :right) (gvl :left))))
   (:mark-indent (o-formula (floor (gvl :parent :shaft-width) 4)))
   (:tic-visible (o-formula (gvl :parent :tic-marks-p)))
   (:num-visible (o-formula (gvl :parent :enumerate-p)))
   (:direction NIL)
   (:items (o-formula (gvl :parent :num-marks)))
   (:item-prototype
    `(,opal:aggregadget
      (:tic-value ,(o-formula (let ((p (kr-path 0 :parent :parent)))
				(+ (gv p :val-1)
				   (* (gvl :rank) (gv p :hash-inc))))))
      (:x1 ,(o-formula (+ (gv (kr-path 0 :parent) :shaft-left)
			  (gv (kr-path 0 :parent) :mark-indent))))
      (:y1 ,(o-formula
	     (let ((p0 (kr-path 0 :parent :parent))
		   (p1 (kr-path 1 :parent)))
	       (inter:Clip-and-map (gvl :tic-value)
				   (gv p0 :val-1) (gv p0 :val-2)
				   (gv p1 :top) (gv p1 :bottom)))))
      (:x2 ,(o-formula (- (gv (kr-path 0 :parent) :right)
			  (gv (kr-path 0 :parent) :mark-indent))))
      (:left ,(o-formula (gv (kr-path 0 :parent) :left)))
      (:width ,(o-formula (- (gvl :x2) (gvl :left))))
      (:string ,(o-formula (format NIL (gv (kr-path 0 :parent :parent)
					   :enum-format-string)
				   (gvl :tic-value))))
      (:font ,(o-formula (gv (kr-path 0 :parent :parent) :enum-font)))
      (:parts
       ((:mark ,opal:line
	       (:constant (:line-style))
	       (:x1 ,(o-formula (gv (kr-path 0 :parent) :x1)))
	       (:y1 ,(o-formula (gv (kr-path 0 :parent) :y1)))
	       (:x2 ,(o-formula (gv (kr-path 0 :parent) :x2)))
	       (:y2 ,(o-formula (gv (kr-path 0 :parent) :y1)))
	       (:visible ,(o-formula (if (gv (kr-path 0 :parent) :visible)
					 (gv (kr-path 1 :parent :parent)
					     :tic-visible)))))
	(:text ,opal:text
	       (:constant (:actual-heightp))
	       (:left ,(o-formula
			(- (+ (gv (kr-path 0 :parent) :left)
			      (gv (kr-path 1 :parent :parent :parent)
				  :enum-width))
			   (gvl :width))))
	       (:top ,(o-formula (- (gv (kr-path 0 :parent) :y1)
				    (floor (gvl :height) 2))))
	       (:string ,(o-formula (gv (kr-path 0 :parent) :string)))
	       (:font   ,(o-formula (gv (kr-path 0 :parent) :font)))
	       (:visible ,(o-formula (if (gv (kr-path 0 :parent) :visible)
					 (gv (kr-path 1 :parent :parent)
					 :num-visible))))))))))

(create-instance 'V-SLIDER-END-MARKS opal:aggregadget
   (:constant '(:line-style))
   (:left (o-formula (gv (kr-path 0 :parent) :shaft-left)))
   (:right (o-formula (gv (kr-path 0 :parent) :right)))
   (:top (o-formula (gv (kr-path 0 :parent) :shaft-top)))
   (:bottom (o-formula (gv (kr-path 0 :parent) :shaft-bottom)))
   (:width (o-formula (- (gvl :right) (gvl :left))))
   (:line-style opal:line-2)
   (:parts
    `((:top-mark ,opal:line
       (:x1 ,(o-formula (gv (kr-path 0 :parent) :left)))
       (:y1 ,(o-formula (gv (kr-path 0 :parent) :top)))
       (:x2 ,(o-formula (gv (kr-path 0 :parent) :right)))
       (:y2 ,(o-formula (gvl :y1)))
       (:line-style ,(o-formula (gv (kr-path 0 :parent)
				    :line-style))))
      (:bot-mark ,opal:line
       (:x1 ,(o-formula (gv (kr-path 0 :parent) :left)))
       (:y1 ,(o-formula (gv (kr-path 0 :parent) :bottom)))
       (:x2 ,(o-formula (gv (kr-path 0 :parent) :right)))
       (:y2 ,(o-formula (gvl :y1)))
       (:line-style ,(o-formula (gv (kr-path 0 :parent) :line-style)))))))

(create-instance 'V-SLIDER opal:aggregadget
   :declare ((:parameters :left :top :height :shaft-width :scr-incr :page-incr
			  :val-1 :val-2 :num-marks :scr-trill-p :page-trill-p
			  :tic-marks-p :enumerate-p :scroll-p :value-feedback-p
			  :format-string :enum-format-string
			  :value-feedback-font :enum-font :value
			  :selection-function :visible)
	     (:type ((integer 0) :shaft-width)
		    ((integer 2) :num-marks)
		    (number :val-1 :val-2 :page-incr :scr-incr)
		    (kr-boolean :scr-trill-p :page-trill-p :tic-marks-p
		     :enumerate-p :scroll-p :value-feedback-p)
		    (string :format-string :enum-format-string)
		    ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
		     :value-feedback-font :enum-font)
		    ((or null function symbol) :selection-function))
	     (:maybe-constant :left :top :height :shaft-width :scr-incr
			      :page-incr :val-1 :val-2 :num-marks :scr-trill-p
			      :page-trill-p :tic-marks-p :enumerate-p
			      :value-feedback-p :scroll-p :value-feedback-font
			      :enum-font :format-string :enum-format-string
			      :visible))
  
   ;; Customizable slots
   ;;
   (:left 0)(:top 0)(:height 250)
   (:shaft-width 20)
   (:scr-incr 1)
   (:page-incr 5)
   (:val-1 0)            ;; Range of values
   (:val-2 100)          ;;   slider may assume
   (:num-marks 11)       ;; Includes top and bottom marks
   (:scr-trill-p T)
   (:page-trill-p T)
   (:tic-marks-p T)      ;; Whether to put tic marks on the shaft
   (:enumerate-p T)      ;; Whether to add numbers to shaft or not
   (:value-feedback-p T) ;; Whether to report slider position above shaft
   (:scroll-p T)
   (:value-feedback-font opal:default-font)
   (:enum-font (opal:get-standard-font :fixed :roman :small))
   (:format-string "~a")
   (:enum-format-string "~a")
   (:selection-function NIL)

   ;; Generally non-customizable slots
   ;;
   (:value (o-formula (inter:Clip-and-Map (second (gvl :indicator :box))
					  (gvl :shaft-top)
					  (- (gvl :shaft-bottom) 1)
					  (gvl :val-1)
					  (gvl :val-2))))
   (:widest-value-width
    (o-formula (if (gvl :value-feedback-p)
		   (max (opal:string-width (gvl :value-feedback-font)
					   (format NIL (gvl :format-string)
						   (gvl :val-1)))
			(opal:string-width (gvl :value-feedback-font)
					   (format NIL (gvl :format-string)
						   (gvl :val-2))))
		   0)))
   (:value-feedback-width (o-formula (+ 6 (gvl :widest-value-width))))
   (:highest-value-height
    (o-formula (if (gvl :value-feedback-p)
		   (max (opal:string-height (gvl :value-feedback-font)
					    (format NIL (gvl :format-string)
						    (gvl :val-1))
					    :actual-heightp T)
			(opal:string-height (gvl :value-feedback-font)
					    (format NIL (gvl :format-string)
						    (gvl :val-2))
					    :actual-heightp T))
		   0)))
   (:value-feedback-height (o-formula (+ 7 (gvl :highest-value-height))))
   ;; Find the widest tic mark value by looking at each and every value
   (:enum-width
    (o-formula
     (do ((width 0 (max width (opal:string-width font (format NIL fmt num))))
	  (font (gvl :enum-font))
	  (fmt (gvl :enum-format-string))
	  (num (gvl :val-1) (+ num inc))
	  (bnd (gvl :val-2))
	  (inc (gvl :hash-inc)))
	 ((> num bnd) width))))
;;;	(:enum-width
;;;	 (o-formula (max (opal:string-width (gvl :enum-font)
;;;					    (format NIL (gvl :enum-format-string)
;;;						    (gvl :val-1)))
;;;			 (opal:string-width (gvl :enum-font)
;;;					    (format NIL (gvl :enum-format-string)
;;;						    (gvl :val-2))))))
   (:enum-offset (o-formula (if (gvl :enumerate-p)
				(+ 5 (gvl :enum-width))
				0)))
   (:shaft-left (o-formula (if (< (gvl :enum-offset)
				  (floor (- (gvl :value-feedback-width)
					    (gvl :shaft-width)) 2))
			       (+ (gvl :left)
				  (floor (- (gvl :value-feedback-width)
					    (gvl :shaft-width)) 2))
			       (+ (gvl :left) (gvl :enum-offset)))))
   (:shaft-top (o-formula (+ 5 (gvl :top-scr-top) (* (gvl :num-trills)
						     (gvl :trill-height)))))
   (:shaft-bottom (o-formula (- (gvl :bottom) 5 
				(* (gvl :num-trills)
				   (gvl :trill-height)))))
   (:shaft-height (o-formula (- (gvl :shaft-bottom) (gvl :shaft-top))))
   (:shaft-line-left (o-formula (+ (gvl :shaft-left)
				  (floor (gvl :shaft-width) 2))))
   (:bound-left (o-formula (gvl :shaft-left)))
   (:bound-top (o-formula (gvl :shaft-top)))
   (:bound-width (o-formula (gvl :shaft-width)))
   (:bound-height (o-formula (gvl :shaft-height)))
   (:bottom (o-formula (+ (gvl :top) (gvl :height))))
   (:right (o-formula (+ (gvl :shaft-left) (gvl :shaft-width))))

   (:trill-height (o-formula (gvl :shaft-width)))
   (:num-trills (o-formula (+ (if (gvl :scr-trill-p) 1 0)
			     (if (gvl :page-trill-p) 1 0))))
   (:trill-box-left (o-formula (gvl :shaft-left)))
   (:top-scr-top (o-formula (if (gvl :value-feedback-p)
				(+ 5 (gvl :value-feedback-height) (gvl :top))
				(gvl :top))))
   (:top-page-top (o-formula (if (gvl :scr-trill-p)
				(+ (gvl :top-scr-top) (gvl :trill-height))
				(gvl :top-scr-top))))
   (:bot-page-top (o-formula (+ 5 (gvl :shaft-bottom))))
   (:bot-scr-top (o-formula (if (gvl :page-trill-p)
				  (+ (gvl :bot-page-top) (gvl :trill-height))
				  (gvl :bot-page-top))))
   (:hash-inc (o-formula (/ (- (gvl :val-2) (gvl :val-1)) ; There are
			    (- (gvl :num-marks) 1))))     ; num-marks - 1
                                                          ; intervals

   (:parts
    `((:BOUNDING-AREA ,bound-box
       (:line-style NIL))

      (:SHAFT ,opal:line
       (:constant (:line-style))
       (:x1 ,(o-formula (+ (gv (kr-path 0 :parent) :shaft-left)
			   (floor (gv (kr-path 0 :parent) :shaft-width) 2))))
       (:y1 ,(o-formula (gv (kr-path 0 :parent) :shaft-top)))
       (:x2 ,(o-formula (gvl :x1)))
       (:y2 ,(o-formula (gv (kr-path 0 :parent) :shaft-bottom))))

      (:VALUE-FEEDBACK ,v-slider-value-feedback)
      (:TIC-MARKS ,v-slider-tic-marks)
      (:END-MARKS ,v-slider-end-marks)
      (:INDICATOR ,v-slider-indicator)
      (:TOP-SCR-TRILL ,top-scr-trill)
      (:BOT-SCR-TRILL ,bot-scr-trill)
      (:TOP-PAGE-TRILL ,top-page-trill)
      (:BOT-PAGE-TRILL ,bot-page-trill)))

   (:interactors
      `((:SLIDE ,slide-inter                ; Indicator follows mouse
		(:attach-point :n))
	(:JUMP ,jump-inter                  ; Indicator jumps to mouse position
	       (:attach-point :n))
	(:WHEEL-UP ,wheel-up-inter)
	(:WHEEL-DOWN ,wheel-down-inter
		     (:extra-function ,#'val-2-WHEEL-fn)))))


;;;    (s-value V-SLIDER :notice-items-changed #'opal:tic-marks-changed)


;;;  DEMO FUNCTION
;;

#+garnet-test (defparameter v-slider-win NIL)
#+garnet-test (defparameter v-slider-top-agg NIL)
#+garnet-test (defparameter v-slider-obj NIL)

#+garnet-test
(defun V-Slider-Go (&optional constant)

  (create-instance 'v-slider-win inter:interactor-window
     (:left 500) (:top 50) (:width 200) (:height 300))

  (s-value v-slider-win
	   :aggregate
	   (create-instance 'v-slider-top-agg opal:aggregate
			    (:overlapping T)))
				  
  (create-instance 'v-slider-obj v-slider
    (:constant constant)
    (:left 60) (:top 10))
  (opal:add-components v-slider-top-agg v-slider-obj)

  (format t "1)  Drag the indicator with the left mouse button.~%")
  (format t "2)  Click the left mouse button on the slider shaft to cause~%")
  (format t "    the indicator to jump to mouse location.~%")
  (format t "3)  Click the left mouse button in the trill boxes to move the~%")
  (format t "    indicator by :scr-incr and :page-incr increments.~%")
  (format t "4)  Text above the shaft changes to reflect new indicator position.~%")

  (opal:update v-slider-win))


#+garnet-test
(defun V-Slider-Stop ()
  (opal:destroy v-slider-win))
