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
;;  Horizontal slider
;;
;;  Features and operation of the horizontal slider:
;;     1)  Drag the indicator with the left mouse button
;;     2)  Click the left mouse button on the slider shaft to cause the
;;         indicator to jump to mouse location
;;     3)  Click the left mouse button in the trill boxes to move the
;;         indicator by :scr-incr and :page-incr increments
;;     4)  Text above the slider shaft changes to reflect new indicator
;;         position.  This text may be edited directly after pressing on it
;;         with the left mouse button.
;;     5)  The top level :value slot is the position of the indicator.
;;         This slot may be set directly and formulae may depend on it.
;;     6)  The function specified in :selection-function will be executed
;;         whenever the value in :value changes.
;;
;;  Customizable slots:
;;     1)  Left, top, width
;;     2)  Shaft-height
;;     3)  Scr-trill-p  --  Whether to have trills that incr by :scr-incr
;;     4)  Page-trill-p --  Whether to have trills that incr by :page-incr
;;     5)  Scr-Incr, Page-incr  --  Values to increment position by in single
;;                                    and double arrow boxes, respectively
;;     6)  Val-1, Val-2  --  Range of values the indicator spans.
;;                           Val-1 corresponds to the left of the slider.
;;     7)  Value -- The current value selected by the user.
;;     8)  Selection-funciton -- Function to be executed when :value changes
;;     9)  Tic-marks-p -- Whether to put tic marks on the shaft
;;    10)  Enumerate-p -- Whether to enumerate the tic marks
;;    11)  Num-marks  --  The number of tic marks (including top and bottom)
;;    12)  Value-feedback-p  --  Whether to report indicator position to the
;;                               left of the shaft
;;    13)  Value-feedback-font -- Font to report indicator position with
;;    14)  Enum-font --  Font to enumerate tic marks with
;;    15)  Format-string -- Formatting string of indicator value
;;    16)  Enum-format-string -- Formatting string of enumeration
;;
;;  NOTE:  This module requires schemata defined in GAD-scroll-parts,
;;         GAD-slider-parts, GAD-h-arrows, and GAD-h-boxes.
;;
;;  Horizontal slider demo:
;;     This module contains a function which creates a window and a slider
;;     in the window.  To run it, enter (GARNET-GADGETS:h-slider-go).
;;     To stop, enter (GARNET-GADGETS:h-slider-stop).
;;
;;  Designed by Brad Myers
;;  Written by Andrew Mickish


(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(H-Slider))
  #+garnet-test
  (export '(H-Slider-Go H-Slider-Stop
	    H-Slider-Win H-Slider-Top-Agg H-Slider-Obj)))

;;; Instances.
(create-instance 'H-SLIDER-VALUE-FEEDBACK opal:aggregadget
   (:left (o-formula (gv (kr-path 0 :parent) :left)))
   (:top (o-formula (gv (kr-path 0 :parent) :value-feedback-top)))
   (:height (o-formula (gv (kr-path 0 :parent) :value-feedback-height)))
   (:width (o-formula (gv (kr-path 0 :parent) :value-feedback-width)))
   (:font (o-formula (gv (kr-path 0 :parent) :value-feedback-font)))
   (:visible (o-formula (if (gv (kr-path 0 :parent) :visible)
			    (gv (kr-path 0 :parent) :value-feedback-p))))
   (:parts
    `((:value-rect ,value-rect)
      (:value-text ,value-text)))
   (:interactors
    `((:value-inter ,value-inter))))

(create-instance 'H-SLIDER-TIC-MARKS opal:aggrelist
   (:constant '(:fixed-width-p :fixed-height-p :fixed-width-size
		:fixed-height-size :direction :h-spacing :v-spacing
		:indent :rank-margin :pixel-margin))
   (:left (o-formula (gv (kr-path 0 :parent) :shaft-left)))
   (:top (o-formula (gv (kr-path 0 :parent) :shaft-top)))
   (:right (o-formula (gv (kr-path 0 :parent) :shaft-right)))
   (:height (o-formula (let ((comp (car (gvl :components))))
			 (max 0 (- (+ (gv comp :top)
				      (gv comp :height))
				   (gvl :top))))))
   (:shaft-bottom (o-formula (gv (kr-path 0 :parent) :shaft-bottom)))
   (:mark-indent (o-formula (round (gv (kr-path 0 :parent) :shaft-height) 4)))
   (:tic-visible (o-formula (gv (kr-path 0 :parent) :tic-marks-p)))
   (:num-visible (o-formula (gv (kr-path 0 :parent) :enumerate-p)))
   (:direction NIL)
   (:items (o-formula (gv (kr-path 0 :parent) :num-marks)))
   (:item-prototype
    `(,opal:aggregadget
      (:tic-value ,(o-formula (let ((p (kr-path 0 :parent :parent)))
				(+ (gv p :val-1)
				   (* (gvl :rank) (gv p :hash-inc))))))
      (:x1 ,(o-formula
	     (let ((p0 (kr-path 0 :parent :parent))
		   (p1 (kr-path 1 :parent)))
	       (inter:Clip-And-Map (gvl :tic-value)
				   (gv p0 :val-1) (gv p0 :val-2)
				   (gv p1 :left) (gv p1 :right)))))
      (:parts
       ((:mark ,opal:line
	       (:constant (:line-style))
	       (:x1 ,(o-formula (gv (kr-path 0 :parent) :x1)))
	       (:y1 ,(o-formula (+ (gv (kr-path 0 :parent :parent) :top)
				   (gv (kr-path 0 :parent :parent)
				       :mark-indent))))
	       (:x2 ,(o-formula (gvl :x1)))
	       (:y2 ,(o-formula (- (gv (kr-path 0 :parent :parent)
				       :shaft-bottom)
				   (gv (kr-path 0 :parent :parent)
				       :mark-indent))))
	       (:visible ,(o-formula (if (gv (kr-path 0 :parent) :visible)
					 (gv (kr-path 1 :parent :parent)
					     :tic-visible)))))
	(:text ,opal:text
	       (:constant (:actual-heightp))
	       (:left ,(o-formula (- (gv (kr-path 0 :parent) :x1)
				     (round (gvl :width) 2))))
	       (:top ,(o-formula (+ 1 (gv (kr-path 0 :parent :parent)
					  :shaft-bottom))))
	       (:string ,(o-formula
			  (let ((p (kr-path 1 :parent :parent :parent)))
			    (format NIL (gv p :enum-format-string)
				    (gv (kr-path 2 :parent) :tic-value)))))
	       (:font ,(o-formula (gv (kr-path 0 :parent :parent :parent)
				      :enum-font)))
	       (:visible ,(o-formula (if (gv (kr-path 0 :parent) :visible)
					 (gv (kr-path 1 :parent :parent)
					     :num-visible))))))))))

(create-instance 'H-SLIDER-END-MARKS opal:aggregadget
   (:constant '(:line-style))
   (:top (o-formula (gv (kr-path 0 :parent) :shaft-top)))
   (:height (o-formula (gv (kr-path 0 :parent) :shaft-height)))
   (:line-style opal:line-2)
   (:parts
    `((:top-mark ,opal:line
       (:x1 ,(o-formula (gv (kr-path 0 :parent :parent) :shaft-left)))
       (:y1 ,(o-formula (gv (kr-path 0 :parent) :top)))
       (:x2 ,(o-formula (gvl :x1)))
       (:y2 ,(o-formula (gv (kr-path 0 :parent :parent) :shaft-bottom)))
       (:line-style ,(o-formula (gv (kr-path 0 :parent) :line-style))))
      (:bot-mark ,opal:line
       (:x1 ,(o-formula (gv (kr-path 0 :parent :parent) :shaft-right)))
       (:y1 ,(o-formula (gv (kr-path 0 :parent) :top)))
       (:x2 ,(o-formula (gvl :x1)))
       (:y2 ,(o-formula (gv (kr-path 0 :parent :parent) :shaft-bottom)))
       (:line-style ,(o-formula (gv (kr-path 0 :parent) :line-style)))))))

(create-instance 'H-SLIDER-INDICATOR opal:arrowhead
   (:constant '(:length :diameter :line-style :width :height))
   (:box '(0 0 0 0))
   (:length 10) (:diameter 15)
   (:top (o-formula (- (gvl :head-y) (gvl :height))))
   (:width 15) (:height 10)
   (:from-x (o-formula (let ((p (kr-path 0 :parent)))
			 (inter:Clip-And-Map (gv p :value)
					     (gv p :val-1) (gv p :val-2)
					     (gv p :shaft-left)
					     (gv p :shaft-right)))))
   (:from-y (o-formula (gv (kr-path 0 :parent) :top)))
   (:head-x (o-formula (gvl :from-x)))
   (:head-y (o-formula (- (gv (kr-path 0 :parent) :shaft-line-top) 2)))
   (:line-style NIL)
   (:filling-style opal:black-fill)
   (:fast-redraw-p T)
   (:draw-function :xor))

(create-instance 'H-SLIDER opal:aggregadget
   :declare ((:parameters :left :top :width :shaft-height :scr-incr :page-incr
			  :val-1 :val-2 :num-marks :scr-trill-p :page-trill-p
			  :tic-marks-p :enumerate-p :scroll-p :value-feedback-p
			  :format-string :enum-format-string
			  :value-feedback-font :enum-font :value
			  :selection-function :visible)
	     (:type ((integer 0) :shaft-height)
		    ((integer 2) :num-marks)
		    (number :val-1 :val-2 :page-incr :scr-incr)
		    (kr-boolean :scr-trill-p :page-trill-p :tic-marks-p
		     :enumerate-p :scroll-p :value-feedback-p)
		    (string :format-string :enum-format-string)
		    ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
		     :value-feedback-font :enum-font)
		    ((or null function symbol) :selection-function))
	     (:maybe-constant :left :top :width :shaft-height :scr-incr
			      :page-incr :val-1 :val-2 :num-marks :tic-marks-p
			      :enumerate-p :scr-trill-p :page-trill-p :scroll-p
			      :value-feedback-p :value-feedback-font :enum-font
			      :format-string :enum-format-string :visible))

   ;; Customizable slots
   ;;
   (:left 0)(:top 0)(:width 300)
   (:shaft-height 20)
   (:scr-incr 1)
   (:page-incr 5)
   (:val-1 0)				; Range of values
   (:val-2 100)				; slider may assume
   (:num-marks 11)			; Includes left and right end marks
   (:tic-marks-p T)			; Whether to put tic marks on the shaft
   (:enumerate-p T)			; Whether to add numbers to shaft or not
   (:scr-trill-p T)
   (:page-trill-p T)
   (:scroll-p T)
   (:value-feedback-p T)		; Whether to report slider position beside shaft
   (:value-feedback-font opal:default-font)
   (:enum-font (opal:get-standard-font NIL NIL :small))
   (:format-string "~a")
   (:enum-format-string "~a")
   (:selection-function NIL)
   (:value (o-formula (inter:Clip-And-Map (first (gvl :indicator :box))
					  (gvl :shaft-left)
					  (- (gvl :shaft-right) 1)
					  (gvl :val-1)
					  (gvl :val-2))))

   ;; Generally non-customizable slots
   ;;
   ;; The width and height of the value feedback box is determined by the width
   ;; and height of the widest and tallest value(s) to be displayed.
   (:widest-value-width
    (o-formula (max (opal:string-width (gvl :value-feedback-font)
				       (format NIL (gvl :format-string)
					       (gvl :val-1)))
		    (opal:string-width (gvl :value-feedback-font)
				       (format NIL (gvl :format-string)
					       (gvl :val-2))))))
   (:value-feedback-width (o-formula (if (gvl :value-feedback-p)
					 (+ 6 (gvl :widest-value-width))
					 0)))
   (:highest-value-height
    (o-formula (max (opal:string-height (gvl :value-feedback-font)
					(format NIL (gvl :format-string)
						(gvl :val-1))
					:actual-heightp T)
		    (opal:string-height (gvl :value-feedback-font)
					(format NIL (gvl :format-string)
						(gvl :val-2))
					:actual-heightp T))))
   (:value-feedback-height (o-formula (if (gvl :value-feedback-p)
					  (+ 7 (gvl :highest-value-height))
					  0)))

   ;; This formula, along with the formula in :shaft-top, guarantees that the
   ;; the value feedback box is centered vertically with the shaft, while the
   ;; feedback box does not extend above the top of the shaft.
   (:value-feedback-top
    (o-formula (if (> (gvl :value-feedback-height)
		      (gvl :shaft-height))
		   (if (< (round (gvl :value-feedback-height) 2) 12)
		       (+ (gvl :top)
			  (- 12 (round (gvl :value-feedback-height) 2)))
		       (gvl :top))
		   (- (gvl :shaft-line-top)
		      (round (gvl :value-feedback-height) 2)))))
   (:highest-enum-height
    (o-formula (if (gvl :enumerate-p)
		   (max (opal:string-height (gvl :enum-font)
					    (format NIL
						    (gvl :enum-format-string)
						    (gvl :val-1)))
			(opal:string-height (gvl :enum-font)
					    (format NIL
						    (gvl :enum-format-string)
						    (gvl :val-2))))
		   0)))
   (:right (o-formula (+ (gvl :left) (gvl :width))))
   (:trill-width (o-formula (gvl :shaft-height)))
   (:num-trills (o-formula (+ (if (gvl :scr-trill-p) 1 0)
			      (if (gvl :page-trill-p) 1 0))))
   (:trill-box-top (o-formula (gvl :shaft-top)))
   (:left-scr-left (o-formula (if (gvl :value-feedback-p)
				  (+ 5 (gvl :value-feedback-width)
				     (gvl :left))
				  (gvl :left))))
   (:left-page-left (o-formula (if (gvl :scr-trill-p)
				(+ (gvl :left-scr-left) (gvl :trill-width))
				(gvl :left-scr-left))))
   (:right-page-left (o-formula (+ 5 (gvl :shaft-right))))
   (:right-scr-left (o-formula (if (gvl :page-trill-p)
				  (+ (gvl :right-page-left) (gvl :trill-width))
				  (gvl :right-page-left))))

   
   ;; If the value feedback box and/or the shaft is too short, then the arrow-
   ;; head may extend above the :top without this complicated formula for
   ;; :shaft-top.  The formula considers every possible relationship between
   ;; these heights and guarantees that the indicator does not overshoot :top.
   ;; The distance that the arrowhead extends above the shaft line is hard-
   ;; coded as 12; the :length of the arrowhead is 10, and it floats 2 pixels
   ;; above the shaft line.
   (:shaft-top
    (o-formula (if (> (gvl :value-feedback-height)
		      (gvl :shaft-height))
		   ;; Then put the value feedback box at the top and center
		   ;; the shaft accordingly.
		   (if (< (round (gvl :value-feedback-height) 2) 12)
		       (+ (gvl :top)
			  (- 12 (round (gvl :value-feedback-height)
				       2)))
		       (- (+ (gvl :top)
			     (round (gvl :value-feedback-height) 2))
			  (round (gvl :shaft-height) 2)))
		   ;; Else, put the shaft at the top.  Make sure that the
		   ;; indicator does not extend above the top.
		   (if (< (round (gvl :shaft-height) 2) 12)
		       (+ (gvl :top)
			  (- 12 (round (gvl :shaft-height) 2)))
		       (gvl :top)))))
   
   (:shaft-bottom (o-formula (+ (gvl :shaft-top) (gvl :shaft-height))))
   (:shaft-line-top (o-formula (+ (gvl :shaft-top)
				  (round (gvl :shaft-height) 2))))
   (:shaft-left (o-formula (+ 5 (gvl :left-scr-left) (* (gvl :num-trills)
						       (gvl :trill-width)))))
   (:shaft-right (o-formula (- (gvl :right) 5
			       (* (gvl :num-trills) (gvl :trill-width)))))
   (:shaft-width (o-formula (- (gvl :shaft-right) (gvl :shaft-left))))
   (:bound-left (o-formula (gvl :shaft-left)))
   (:bound-top (o-formula (gvl :shaft-top)))
   (:bound-width (o-formula (gvl :shaft-width)))
   (:bound-height (o-formula (gvl :shaft-height)))

   (:hash-inc (o-formula (/ (- (gvl :val-2) (gvl :val-1)) ; There are
			    (- (gvl :num-marks) 1))))     ; num-marks - 1
                                                          ; intervals
   
   (:parts
    `((:BOUNDING-AREA ,bound-box        ; Area interactors work in
		      (:line-style NIL))

      (:SHAFT ,opal:line
       (:constant (:line-style))
       (:x1 ,(o-formula (gv (kr-path 0 :parent) :shaft-left)))
       (:y1 ,(o-formula (+ (gv (kr-path 0 :parent) :shaft-top)
			   (round (gv (kr-path 0 :parent) :shaft-height) 2))))
       (:x2 ,(o-formula (gv (kr-path 0 :parent) :shaft-right)))
       (:y2 ,(o-formula (gvl :y1))))

      (:VALUE-FEEDBACK ,h-slider-value-feedback)
      (:TIC-MARKS ,h-slider-tic-marks)
      (:END-MARKS ,h-slider-end-marks)
      (:INDICATOR ,h-slider-indicator)
      (:LEFT-SCR-TRILL ,left-scr-trill)
      (:RIGHT-SCR-TRILL ,right-scr-trill)
      (:LEFT-PAGE-TRILL ,left-page-trill)
      (:RIGHT-PAGE-TRILL ,right-page-trill)))

   (:interactors
      `((:SLIDE ,slide-inter                ; Indicator follows mouse
		(:attach-point :w))         ; ***Arrowhead/Interactors hack
	(:JUMP ,jump-inter                  ; Indicator jumps to mouse position
	       (:attach-point :w))
	(:WHEEL-UP ,wheel-up-inter)
	(:WHEEL-DOWN ,wheel-down-inter
		     :extra-function ,#'val-2-WHEEL-fn))))


;;;     (s-value H-SLIDER :notice-items-changed #'opal:tic-marks-changed)



;;;  DEMO FUNCTION
;;
;;
#+garnet-test (defparameter h-slider-win NIL)
#+garnet-test (defparameter h-slider-top-agg NIL)
#+garnet-test (defparameter h-slider-obj NIL)

#+garnet-test
(defun H-Slider-Go (&optional constant)

  (create-instance 'h-slider-win inter:interactor-window
     (:left 680) (:top 10) (:width 330) (:height 200))

  (s-value h-slider-win
	   :aggregate
	   (create-instance 'h-slider-top-agg opal:aggregate
			    (:overlapping T)))
				  
  (create-instance 'h-slider-obj h-slider
    (:constant constant)
    (:left 10) (:top 50))

  (opal:add-components h-slider-top-agg h-slider-obj)

  (format t "1)  Drag the indicator with the left mouse button.~%")
  (format t "2)  Click the left mouse button on the slider shaft to cause~%")
  (format t "    the indicator to jump to mouse location.~%")
  (format t "3)  Click the left mouse button in the trill boxes to move the~%")
  (format t "    indicator by :scr-incr and :page-incr increments.~%")
  (format t "4)  Text beside the shaft changes to reflect new slider position.~%")

  (opal:update h-slider-win))

#+garnet-test
(defun H-Slider-Stop ()
  (opal:destroy h-slider-win))
