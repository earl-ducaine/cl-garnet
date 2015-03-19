;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-SCROLLBAR; Base: 10 -*-
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;
;;          The Garnet User Interface Development Environment.       ;;
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;
;;;
;;; $Id$	
;;


;;;  This file contains a top level function that creates a window 
;;   and four scroll bars -- Mac, Openlook, NeXT, and Motif.  Each
;;   module is included in this single file, but a demo for each 
;;   scroll bar may be run independently.
;;
;;   ** (demo-scrollbar:do-go) to begin, **
;;   ** (demo-scrollbar:do-stop) to stop **
;;
;;   Features:
;;     1)  Leftdown in a trill box moves the indicator
;;     2)  Leftdown in the background moves the indicator
;;     3)  Leftdown + Drag in the indicator moves the indicator
;;
;;   Each scroll bar has a demo function included in this file --
;;   MAC-GO, OPEN-GO, NEXT-GO, and MOTIF-GO, to display macintosh, 
;;   openlook, Next, and Motif scroll bars independently.
;;
;; -------------------------------------------------------------------------
;; Designed and implemented by Brad Vander Zanden


(in-package :DEMO-SCROLLBAR)

(defvar DEMO-SCROLLBAR-INIT
  (dolist (file '("motif-v-scroll-loader"))
    (common-lisp-user::garnet-load (concatenate 'string "gadgets:" file))))

(declaim (special OPEN-INTER OPEN-UP-ARROW OPEN-END-RECT OPEN-TRILL-BOX
		  OPEN-DOWN-ARROW OPEN-SCROLLBAR OPEN-TOP-AGG OPEN-VP OPEN-TEXT

		  LIGHT-BLUE-GRAY-FILL BLUE-GRAY-FILL 

		  NEXT-DOWN-ARROW NEXT-TOP-AGG NEXT-SCROLLBAR NEXT-VP NEXT-TEXT
		  NEXT-SHADOW-BOX NEXT-INTER NEXT-UP-ARROW

		  MOTIF-VP MOTIF-SCROLLBAR MOTIF-TOP-AGG MOTIF-TEXT MOTIF-VP
		  SB-OPEN-OBJ SB-OPEN-TEXT SB-NEXT-TEXT
		  SB-MOTIF-BACKGROUND SB-MOTIF-OBJ SB-MOTIF-TEXT))

(defvar *color-p* (g-value opal:color :color-p))


;;; MACINTOSH SCROLL BAR MODULE
;;
;;
;; This module contains demo code for showing a macintosh-like scroll bar.
;; When loaded, it creates a window that contains a scroll-bar that can be
;; operated with the left mouse button
;;
;; This module is intended for use with the demo-scrollbar demo, which
;; dispays mac, next, openlook, and motif scroll bars.
;;
;; There is also a demo function at the end of this file to display just the
;; macintosh scroll bar:  (demo-scrollbar:mac-go) to begin.
;;
;; Features:
;;   1)  Leftdown in a trill box moves the indicator
;;   2)  Leftdown in the background moves the indicator
;;   3)  Leftdown + Drag in the indicator moves the indicator
;;

;; function to create lines for the arrows. 
(defun MAC-create-arrow-line (obj init-x1 init-y1 init-x2 init-y2 x-offset y-offset)
    (create-instance NIL opal:line
	(:constant T)
	(:obj obj)
	(:x-offset x-offset)
	(:y-offset y-offset)
	(:x1 (o-formula (gvl :obj :x2) init-x1))
	(:y1 (o-formula (gvl :obj :y2) init-y1))
	(:x2 (o-formula (+ (gvl :x1) (gvl :x-offset)) init-x2))
	(:y2 (o-formula (+ (gvl :y1) (gvl :y-offset)) init-y2))))


;; function to create a scroll bar
(defun MAC-make-scroll-bar (viewport top-agg scroll-bar-name left top width height)

  (let (slider trill-box-incr trill-box-decr slider-shell indicator)

    ;; create the scroll bar and add it to top-agg
    (opal:add-component top-agg
	     (setf scroll-bar-name (create-instance NIL opal:aggregate)))

    ;; create all the objects I need for a slider
    (setf slider (create-instance NIL opal:aggregate))
    (setf slider-shell (create-instance NIL opal:rectangle))
    (setf indicator (create-instance NIL opal:rectangle))

    ;; create the slot definitions for the scroll box
    (s-value scroll-bar-name :left left)
    (s-value scroll-bar-name :top top)
    (s-value scroll-bar-name :width width)
    (s-value scroll-bar-name :height height)

    ;; create the slot definitions for the slider
    (s-value slider :indicator indicator)

    ;; create the slot definitions for the slider shell
    (s-value slider-shell :scroll-bar-name scroll-bar-name)
    (s-value slider-shell :left (o-formula (gvl :scroll-bar-name :left)))
    (s-value slider-shell :top (o-formula (gvl :scroll-bar-name :top)))
    (s-value slider-shell :width (o-formula (gvl :scroll-bar-name :width)))
    (s-value slider-shell :height (o-formula (gvl :scroll-bar-name :height)))
    (s-value slider-shell :bottom (o-formula (+ (gvl :top) (gvl :height))))
    (s-value slider-shell :filling-style 
			  (opal:halftone 50 :foreground-color
				(if *color-p* opal:red opal:black)))

    ;; create the slot definitions for the indicator
    (s-value indicator :box '(0 80 10 10)) ; set by mouse interactor
    (s-value indicator :slider-shell slider-shell)
    (s-value indicator :percent (o-formula (inter:Clip-and-Map (second (gvl :box))
						    (gvl :slider-shell :top)
						    (- (gvl :slider-shell :bottom)
						       (gvl :height) 2)
						    0 100)))
    (s-value indicator :left (o-formula (+ 1 (gvl :slider-shell :left))))
    (s-value indicator :top (o-formula (1+ (inter:Clip-and-Map (gvl :percent)
						   0 100
						   (gvl :slider-shell :top)
						   (- (gvl :slider-shell :bottom)
						      (gvl :height) 2)))))
    (s-value indicator :filling-style (opal:halftone 0))
    (s-value indicator :width (o-formula (- (gvl :slider-shell :width) 2)))
    (s-value indicator :height (o-formula (gvl :width)))

    ;; add the components to the slider
    (opal:add-component slider slider-shell)
    (opal:add-component slider indicator)

    ;; create the increment and decrement trill boxes
    (setf trill-box-incr (MAC-make-incr-box viewport 'trill-box-incr1 indicator
			   (formula `(gv ',scroll-bar-name :left))
			   (formula `(- (gv ',scroll-bar-name :top)
					    (gvl :height)))
			   (formula `(gv ',scroll-bar-name :width))
			   20))

    (setf trill-box-decr (MAC-make-decr-box viewport 'trill-box-decr1 indicator
			   (formula `(gv ',scroll-bar-name :left))
			   (formula `(+ (gv ',scroll-bar-name :top)
					    (gv ',scroll-bar-name :height)))
			   (formula `(gv ',scroll-bar-name :width))
			   20))

    ;; add the slider and the trill boxes to the scroll bar
    (opal:add-components scroll-bar-name slider trill-box-incr trill-box-decr)
    (s-value scroll-bar-name :slider slider)

    ;; create the scroll bar interactor
    (create-instance NIL inter:Move-Grow-Interactor
      (:window viewport)
      (:continuous t)
      (:start-event :leftdown)
      (:start-where (list :in-box indicator))
      (:running-where (list :in-box slider-shell))
      (:outside :last)
      (:obj-to-be-moved indicator)
      (:feedback-obj NIL)
      (:attach-point :center)
      (:grow-p NIL)
      (:waiting-priority inter:high-priority-level))

    (create-instance NIL inter:Move-Grow-Interactor
       (:window viewport)
       (:continuous T)
       (:start-event :leftdown)
       (:start-where (list :in-box slider-shell))
       ;; The function checks whether the mouse event (leftdown) occured above
       ;; or below the indicator box, and moves the indicator up or down
       ;; by 5 appropriately.
       (:start-action
	#'(lambda (interactor final-obj-over point-list)
	    (declare (ignore final-obj-over point-list))
	    (let ((obj-to-be-moved (g-value interactor :obj-to-be-moved)))
	      (if (< (* (g-value interactor :y-off) 100)
		     (* (g-value obj-to-be-moved :percent)
			(g-value interactor :obj-being-changed :height)))
		  (if (>= (g-value obj-to-be-moved :percent) 5)
		      (decf (g-value obj-to-be-moved :percent) 5))
		  (if (<= (g-value obj-to-be-moved :percent) 95)
		      (incf (g-value obj-to-be-moved :percent) 5))))))
       (:running-where (list :in-box slider-shell))
       (:outside NIL)
       (:obj-to-be-moved indicator)
       (:grow-p NIL))

    ;; This adds scroll wheel support. Scroll wheels are treated
    ;; like button clicks.
    (create-instance NIL inter:Button-Interactor
      (:window viewport)
      (:continuous nil)
      (:feedback-obj NIL)
      (:start-where (list :in-box slider-shell))
      (:exception NIL)			; no exceptions
      (:start-event (list :downscrollup :upscrollup))
      (:final-function
       #'(lambda (interactor final-obj-over)
	   (declare (ignore final-obj-over))
	   (if (>= (g-value indicator :percent) 1)
	       (incf (g-value indicator :percent)
		     (if (eq (inter:event-char inter:*current-event*)
			     (first (g-value interactor :start-event)))
			 1
			 -1)))))))
  scroll-bar-name)


;; function to create the increment trill box
(defun MAC-make-incr-box (viewport trill-box-incr indicator left top width height)
  (let (incr-box up-arrow up-arrow-line1 up-arrow-line2 up-arrow-line3
		   up-arrow-line4 up-arrow-line5 up-arrow-line6 up-arrow-line7)
 
    ;; create the objects I need to make an increment trill box
    (setf trill-box-incr (create-instance NIL opal:aggregate))
    (setf incr-box (create-instance NIL opal:rectangle))
    (setf up-arrow (create-instance NIL opal:aggregate))
    (setf up-arrow-line1 (create-instance NIL opal:line))

    ;; create the slot definitions for the trill box
    (s-value trill-box-incr :left left)
    (s-value trill-box-incr :top top)
    (s-value trill-box-incr :width width)
    (s-value trill-box-incr :height height)

    ;; create the slot definitions for the increment box
    (s-value incr-box :trill-box-incr trill-box-incr)
    (s-value incr-box :left (o-formula (gvl :trill-box-incr :left)))
    (s-value incr-box :top (o-formula (gvl :trill-box-incr :top)))
    (s-value incr-box :width (o-formula (gvl :trill-box-incr :width)))
    (s-value incr-box :height (o-formula (gvl :trill-box-incr :height)))

    ;; create the slot definitions for the arrow lines
    (s-value up-arrow-line1 :incr-box incr-box)
    (s-value up-arrow-line1 :x1 (o-formula (+ (gvl :incr-box :left) 6)))
    (s-value up-arrow-line1 :y1 (o-formula (+ (gvl :incr-box :top) 17)))
    (s-value up-arrow-line1 :x2 (o-formula (+ (gvl :x1) 8)))
    (s-value up-arrow-line1 :y2 (o-formula (gvl :y1)))

    ;; create the other six lines used in the up arrow
    (setf up-arrow-line2 (MAC-create-arrow-line up-arrow-line1 114 165 114 159 0 -6))
    (setf up-arrow-line3 (MAC-create-arrow-line up-arrow-line2 114 159 119 159 5 0))
    (setf up-arrow-line4 (MAC-create-arrow-line up-arrow-line3 119 159 110 150 -9 -9))
    (setf up-arrow-line5 (MAC-create-arrow-line up-arrow-line4 110 150 101 159 -9 9))
    (setf up-arrow-line6 (MAC-create-arrow-line up-arrow-line5 101 159 106 159 5 0))
    (setf up-arrow-line7 (MAC-create-arrow-line up-arrow-line6 106 159 106 165 0 6))

    ;; add the arrow lines to the up arrow aggregate
    (opal:add-component up-arrow up-arrow-line1)
    (opal:add-component up-arrow up-arrow-line2)
    (opal:add-component up-arrow up-arrow-line3)
    (opal:add-component up-arrow up-arrow-line4)
    (opal:add-component up-arrow up-arrow-line5)
    (opal:add-component up-arrow up-arrow-line6)
    (opal:add-component up-arrow up-arrow-line7)

    ;; add the increment box and up arrow aggregate to the trill box aggregate
    (opal:add-component trill-box-incr incr-box)
    (opal:add-component trill-box-incr up-arrow)

    ;; create the interactor for the increment trill box
    (create-instance NIL inter:Button-Interactor
		       (:window viewport)
		       (:continuous t)
		       (:feedback-obj NIL)
		       (:start-where (list :element-of trill-box-incr))
		       (:exception NIL)  ; no exceptions
		       (:start-event :leftdown)
		       (:stop-event :leftup)
		       (:stop-action #'(lambda (interactor final-obj-over)
					 (declare (ignore interactor final-obj-over))
					 (if (>= (g-value indicator :percent) 1)
					     (decf (g-value indicator :percent)))))))

  trill-box-incr)



;; function to create the decrement trill box
(defun MAC-make-decr-box (viewport trill-box-decr indicator left top width height)
  (let (decr-box down-arrow down-arrow-line1 down-arrow-line2 down-arrow-line3
		   down-arrow-line4 down-arrow-line5 down-arrow-line6 
		   down-arrow-line7)
 
    ;; create the objects I need to make a decrement trill box
    (setf trill-box-decr (create-instance NIL opal:aggregate))
    (setf decr-box (create-instance NIL opal:rectangle))
    (setf down-arrow (create-instance NIL opal:aggregate))
    (setf down-arrow-line1 (create-instance NIL opal:line))

    ;; create the slot definitions for the trill box
    (s-value trill-box-decr :left left)
    (s-value trill-box-decr :top top)
    (s-value trill-box-decr :width width)
    (s-value trill-box-decr :height height)

    ;; create the slot definitions for the decrement box
    (s-value decr-box :trill-box-decr trill-box-decr)
    (s-value decr-box :left (o-formula (gvl :trill-box-decr :left)))
    (s-value decr-box :top (o-formula (gvl :trill-box-decr :top)))
    (s-value decr-box :width (o-formula (gvl :trill-box-decr :width)))
    (s-value decr-box :height (o-formula (gvl :trill-box-decr :height)))

    ;; create the slot definitions for the first down arrow line
    (s-value down-arrow-line1 :decr-box decr-box)
    (s-value down-arrow-line1 :x1 (o-formula (+ (gvl :decr-box :left) 6) 142))
    (s-value down-arrow-line1 :y1 (o-formula (+ (gvl :decr-box :top) 2) 150))
    (s-value down-arrow-line1 :x2 (o-formula (+ (gvl :x1) 8) 150))
    (s-value down-arrow-line1 :y2 (o-formula (gvl :y1) 150))

    ;; create the other six lines used in the down arrow
    (setf down-arrow-line2 (MAC-create-arrow-line down-arrow-line1 150 150 150 156 0 6))
    (setf down-arrow-line3 (MAC-create-arrow-line down-arrow-line2 150 156 155 156 5 0))
    (setf down-arrow-line4 (MAC-create-arrow-line down-arrow-line3 155 156 146 165 -9 9))
    (setf down-arrow-line5 (MAC-create-arrow-line down-arrow-line4 146 165 137 156 -9 -9))
    (setf down-arrow-line6 (MAC-create-arrow-line down-arrow-line5 137 156 142 156 5 0))
    (setf down-arrow-line7 (MAC-create-arrow-line down-arrow-line6 142 156 142 150 0 -6))

    ;; add the arrow lines to the down arrow aggregate
    (opal:add-component down-arrow down-arrow-line1)
    (opal:add-component down-arrow down-arrow-line2)
    (opal:add-component down-arrow down-arrow-line3)
    (opal:add-component down-arrow down-arrow-line4)
    (opal:add-component down-arrow down-arrow-line5)
    (opal:add-component down-arrow down-arrow-line6)
    (opal:add-component down-arrow down-arrow-line7)

    ;; add the decrement box and down arrow aggregate to the trill box aggregate
    (opal:add-component trill-box-decr decr-box)
    (opal:add-component trill-box-decr down-arrow)

    ;; create the interactor decrement trill box
    (create-instance NIL inter:button-interactor
		       (:window viewport)
		       (:feedback-obj NIL)
		       (:continuous t)
		       (:start-where (list :element-of trill-box-decr))
		       (:exception NIL)  ; no exceptions
		       (:start-event :leftdown)
		       (:stop-event :leftup)
		       (:stop-action #'(lambda (interactor final-obj-over)
					 (declare (ignore interactor final-obj-over))
					 (if (<= (g-value indicator :percent) 99)
					     (incf (g-value indicator :percent)))))))

  trill-box-decr)


;; function to create text that displays
;; current value of the scroll bar
(defun MAC-make-meter-text (top-agg meter-name scroll-bar-name left top)

  ;; create the meter and add it to top-agg
  (opal:add-component top-agg
		      (setf meter-name (create-instance NIL opal:text)))

  ;; create the slot definitions for the meter
  (s-value meter-name :left left)
  (s-value meter-name :top top)
  (s-value meter-name :scroll-bar-name scroll-bar-name)
  (s-value meter-name 
	   :string (o-formula (prin1-to-string
			       (gvl :scroll-bar-name :slider :indicator :percent)) "4"))
  (s-value meter-name :font (create-instance NIL opal:font
			      (:size :small)))
  meter-name)


;;;  DEMO FUNCTION:  MAC-GO
;;

(defparameter MAC-obj NIL)
(defparameter MAC-meter NIL)
(defparameter MAC-VP NIL)
(defparameter MAC-top-agg NIL)

(defun MAC-Go ()

  ;; create a viewport
  (setq MAC-VP (create-instance NIL inter:interactor-window
		 (:left 100) (:top 10) (:width 200) (:height 300)
		 (:title "GARNET Mac")
		 (:icon-title "Mac")
		 (:aggregate
		  (setq MAC-top-agg
			(create-instance NIL opal:aggregate)))))

  ;; create the scroll bar and meter text and display them
  (setq MAC-obj (MAC-make-scroll-bar MAC-VP MAC-top-agg 'MAC-obj 64 48 21 200))
  (setq MAC-meter
	(MAC-make-meter-text MAC-top-agg 'MAC-meter MAC-obj
			     (formula `(- (+ (gv ',MAC-obj :slider :indicator :left)
					     (floor (gv ',MAC-obj :slider :indicator :width) 2))
					  (floor (gvl :width) 2)))

			     (formula `(- (+ (gv ',MAC-obj :slider :indicator :top)
					     (floor (gv ',MAC-obj :slider :indicator :height) 2))
					  (floor (gvl :height) 2)))))
  (opal:update MAC-VP))

(defun MAC-Stop ()
  (opal:destroy MAC-VP))



;;; OPENLOOK SCROLL BAR MODULE
;;

;;  This module contains demo code for showing a openlook-like scroll bar.
;;  When the demo is executed, it creates a window that contains a scroll-bar
;;  that can be operated with the left mouse button
;;
;;  This module is intended for use with the demo-scrollbar demo, which
;;  dispays mac, next, openlook, and motif scroll bars.
;;
;;  There is also a demo function at the end of this file to display just the
;;  openlook scroll bar:  (demo-scrollbar:open-go) to begin.
;;
;;  Features:
;;    1)  Leftdown in a trill box moves the indicator
;;    2)  Leftdown in the background moves the indicator
;;    3)  Leftdown + Drag in the indicator moves the indicator
;;

(create-instance 'OPEN-INTER inter:button-interactor
   (:window (o-formula (gv-local :self :operates-on :window)))
   (:start-where (o-formula (list :in-box (gvl :operates-on) :box)))
   (:waiting-priority inter:high-priority-level))


(create-instance 'OPEN-END-RECT opal:aggregadget
   (:parts
    `((:box ,opal:rectangle
	    (:left ,(o-formula (gvl :parent :parent :left)))
	    (:top ,(o-formula (gvl :parent :top)))
	    (:width ,(o-formula (gvl :parent :parent :bound-width)))
	    (:height ,(o-formula (gvl :parent :parent :end-rect-height))))))
   (:interactors
    `((:jump ,OPEN-INTER
	     (:final-function ,(o-formula (gvl :operates-on :final-fn)))))))


(create-instance 'OPEN-UP-ARROW opal:polyline
   (:left (o-formula (+ 2 (gvl :parent :box :left))))
   (:right (o-formula (- (+ (gvl :parent :box :left) (gvl :parent :box :width))
			 4)))
   (:center-x (o-formula (+ (gvl :left)
			    (floor (- (gvl :right) (gvl :left)) 2))))
   (:top (o-formula (+ 4 (gvl :parent :box :top))))
   (:bottom (o-formula (- (+ (gvl :parent :box :top) (gvl :parent :box :height))
			     8)))
   (:point-list (o-formula (list (gvl :center-x) (gvl :top)
				 (gvl :right) (gvl :bottom)
				 (gvl :left) (gvl :bottom)
				 (gvl :center-x) (gvl :top))))
   (:filling-style opal:green-fill))


(create-instance 'OPEN-DOWN-ARROW OPEN-UP-ARROW
   (:point-list (o-formula (list (gvl :center-x) (gvl :bottom)
				 (gvl :left) (gvl :top)
				 (gvl :right) (gvl :top)
				 (gvl :center-x) (gvl :bottom)))))


(create-instance 'OPEN-TRILL-BOX opal:rectangle
   (:left (o-formula (gvl :parent :parent :parent :left)))
   (:top (o-formula (gvl :parent :top)))
   (:width (o-formula (gvl :parent :parent :parent :bound-width)))
   (:height (o-formula (gvl :width)))
   (:filling-style opal:white-fill))


(create-instance 'OPEN-SCROLLBAR opal:aggregadget
   (:left 50)
   (:top 50)
   (:width 21)
   (:height 200)
   (:end-rect-height 5)
   (:shadow-offset 3)

   (:value (o-formula (inter:Clip-and-Map (second (gvl :indicator :box))
					  (gvl :bound-top)
					  (- (gvl :bound-bottom)
					     (gvl :indicator :height) 2)
					  0 100)))
   (:bound-top (o-formula (+ 1 (gvl :top) (gvl :end-rect-height))))
   (:bound-width (o-formula (- (gvl :width) (gvl :shadow-offset))))
   (:bound-height (o-formula (- (gvl :height)
				(* 2 (gvl :end-rect-height)) 2)))
   (:bound-bottom (o-formula (+ (gvl :bound-top) (gvl :bound-height))))

   (:parts
    `((:bound-box ,opal:rectangle
		  (:left ,(o-formula (gvl :parent :left)))
		  (:top ,(o-formula (gvl :parent :bound-top)))
		  (:width ,(o-formula (gvl :parent :bound-width)))
		  (:height ,(o-formula (gvl :parent :bound-height)))
		  (:line-style NIL))
      (:line ,opal:line
	     (:x1 ,(o-formula (+ (gvl :parent :left)
				 (floor (gvl :parent :width) 2))))
	     (:y1 ,(o-formula (gvl :parent :bound-top)))
	     (:x2 ,(o-formula (gvl :x1)))
	     (:y2 ,(o-formula (gvl :parent :bound-bottom)))
	     (:line-style ,(create-instance NIL opal:line-style
			      (:foreground-color
				 (if *color-p* opal:green opal:black))
			      (:line-thickness 4)
			      (:stipple opal::gray-fill-bitmap))))
      (:top-end-rect ,OPEN-END-RECT
		     (:top ,(o-formula (gvl :parent :top)))
		     (:final-fn ,#'(lambda (interactor obj)
				  (declare (ignore obj))
				  (let ((slider (g-value interactor :operates-on
							 :parent)))
				    (s-value slider :value 0)))))
      (:bot-end-rect ,OPEN-END-RECT
		     (:top ,(o-formula (+ (gvl :parent :bound-bottom) 1)))
		     (:final-fn ,#'(lambda (interactor obj)
				  (declare (ignore obj))
				  (let ((slider (g-value interactor :operates-on
							 :parent)))
				    (s-value slider :value 100)))))
      (:indicator ,opal:aggregadget
	  (:box (0 75 0 0))   ;; Set by interactor
	  (:top ,(o-formula
		  ;; Use LET since :parent is used several times
		  (let ((parent (gvl :parent)))
		    (inter:Clip-and-Map (gv parent :value) 0 100
					(gv parent :bound-top)
					(- (gv parent :bound-bottom)
					   (gvl :height))))))
	  (:height ,(o-formula (+ (* 3 (gvl :parent :bound-width))
				  (gvl :parent :shadow-offset))))
	  (:parts
	   ((:shadow ,opal:rectangle
		 (:left ,(o-formula (+ (gvl :parent :parent :left)
				       (gvl :parent :parent :shadow-offset))))
		 (:top ,(o-formula (+ (gvl :parent :top)
				      (gvl :parent :parent :shadow-offset))))
		 (:width ,(o-formula (gvl :parent :parent :bound-width)))
		 (:height ,(o-formula (* 3 (gvl :width))))
		 (:filling-style ,opal:green-fill))
	     (:top-trill ,opal:aggregadget
		 (:top ,(o-formula (gvl :parent :top)))
		 (:parts
		  ((:box ,OPEN-TRILL-BOX)
		   (:arrow ,OPEN-UP-ARROW)))
		 (:interactors
		  ((:trill ,OPEN-INTER
			   (:final-function
			    ,#'(lambda (interactor obj)
			      (declare (ignore interactor))
			      (let* ((slider (g-value obj :parent :parent))
				     (value (g-value slider :value)))
				(when (> value 1)
				  (s-value slider :value (- value 1))))))))))
	     (:middle ,opal:rectangle
		      (:left ,(o-formula (gvl :parent :parent :left)))
		      (:top ,(o-formula (+ (gvl :parent :parent :bound-width)
					   (gvl :parent :top))))
		      (:width ,(o-formula (gvl :parent :parent :bound-width)))
		      (:height ,(o-formula (gvl :width)))
		      (:filling-style ,opal:white-fill))
	     (:bot-trill ,opal:aggregadget
		 (:top ,(o-formula (+ (gvl :parent :top)
				      (* (gvl :parent :parent :bound-width)
					 2))))
		 (:parts
		  ((:box ,OPEN-TRILL-BOX)
		   (:arrow ,OPEN-DOWN-ARROW)))
		 (:interactors
		  ((:trill ,OPEN-INTER
			   (:final-function
			    ,#'(lambda (interactor obj)
			      (declare (ignore interactor))
			      (let* ((slider (g-value obj :parent :parent))
				     (value (g-value slider :value)))
				(when (< value 100)
				  (s-value slider :value (+ value 1)))))))))))))))
   (:interactors
    `((:slide ,inter:move-grow-interactor
	      (:window ,(o-formula (gv-local :self :operates-on :window)))
	      (:continuous T)
	      (:start-where
	       ,(o-formula (list :in-box
				 (gvl :operates-on :indicator :middle))))
	      (:running-where ,(o-formula (list :in-box
						(gvl :operates-on :bound-box))))
	      (:outside NIL)
	      (:obj-to-change ,(o-formula (gvl :operates-on :indicator)))
	      (:obj-to-be-moved ,(o-formula (gvl :operates-on :indicator)))
	      (:feedback-obj ,(o-formula (gvl :operates-on :indicator)))
	      (:grow-p NIL)
	      (:attach-point :where-hit)
	      (:waiting-priority ,inter:high-priority-level))
      (:jump ,inter:Move-Grow-Interactor
	     (:window ,(o-formula (gv-local :self :operates-on :window)))
	     (:continuous T)
	     (:start-where ,(o-formula (list :in-box
					     (gvl :operates-on :bound-box))))
	     (:running-where ,(o-formula (list :in-box
					       (gvl :operates-on :bound-box))))
	     (:outside NIL)
	     (:obj-to-be-moved ,(o-formula (gvl :operates-on :indicator)))
	     (:grow-p NIL)
	     (:start-action
	      ,#'(lambda (interactor obj-over points-list)
		(declare (ignore obj-over points-list))
		(let* ((indicator (g-value interactor :obj-to-be-moved))
		       (slider (g-value indicator :parent))
		       (value (g-value slider :value)))
		  (if (> (g-value interactor :y-off) (g-value indicator :top))
		      (when (<= value 95) (s-value slider :value (+ value 5)))
		      (when (>= value 5) (s-value slider :value (- value 5)))
		      )))))
      (:wheel ,inter:Button-Interactor
	      (:window ,(o-formula (gv-local :self :operates-on :window)))
	      (:continuous NIL)
	      (:start-where ,(o-formula (list :in-box
					      (gvl :operates-on :bound-box))))
	      (:start-event (:downscrollup :upscrollup))
	      (:final-function
	       ,#'(lambda (interactor obj)
		    (declare (optimize (speed 1) (safety 3) (debug 3)))
		    (let* ((slider (g-value obj :parent))
			   (value (g-value slider :value)))
		      (if (eq (inter:event-char inter:*current-event*)
			      (first (g-value interactor :start-event)))
			  (when (< value 100)
			    (s-value slider :value (+ value 1)))
			  (when (> value 0)
			    (s-value slider :value (- value 1)))))))))))


;;; DEMO FUNCTION:  OPEN-GO
;;

(defun OPEN-Go ()

  ;; Create a window
  (create-instance 'OPEN-vp inter:interactor-window
    (:left 300) (:top 10) (:width 200) (:height 300)
    (:title "GARNET OpenLook") (:icon-title "OpenLook")
    (:aggregate
     (create-instance 'OPEN-top-agg opal:aggregate)))

  ;; Add the scroll bar to the window and display it
  (opal:add-component OPEN-top-agg OPEN-SCROLLBAR)
  (opal:update OPEN-vp)

  ;; Create a text object to display the scrollbar's value
  (create-instance 'OPEN-TEXT opal:text
    (:constant T :except :string)
    (:left 150) (:top 140)
    (:string (o-formula (prin1-to-string (gv OPEN-SCROLLBAR :value))))
    (:font opal:default-font))
  (opal:add-component OPEN-top-agg OPEN-TEXT)
  (opal:update OPEN-vp))


(defun OPEN-Stop ()
  ;; Remove scrollbar first so it won't be destroyed with the window
  (opal:remove-component OPEN-top-agg OPEN-SCROLLBAR)
  (opal:destroy OPEN-vp))




;;; NeXT SCROLL BAR MODULE *
;;

;;  This file contains demo code for showing a NeXT-like scroll bar.
;;  When loaded, it creates a window that contains a scroll-bar that can be
;;  operated with the left mouse button
;; 
;;  This module is intended for use with the demo-scrollbar demo, which
;;  dispays mac, next, openlook, and motif scroll bars.
;; 
;;  There is also a demo function at the end of this file to display just the
;;  NeXT scroll bar:  (demo-scrollbar:next-go) to begin.
;; 
;;  Features:
;;    1)  Leftdown in a trill box moves the indicator
;;    2)  Leftdown in the background moves the indicator
;;    3)  Leftdown + Drag in the indicator moves the indicator

(create-instance 'NEXT-INTER inter:button-interactor
  (:window (o-formula (gv-local :self :operates-on :window)))
  (:start-where (o-formula (list :in-box (gvl :operates-on) :box)))
  (:waiting-priority inter:high-priority-level))


(create-instance 'NEXT-UP-ARROW opal:polyline
  (:left (o-formula (+ 4 (gvl :parent :box :box :left))))
  (:right (o-formula (- (+ (gvl :parent :box :box :left)
			   (gvl :parent :box :box :width))
			4)))
  (:center-x (o-formula (+ (gvl :left)
			   (floor (- (gvl :right) (gvl :left)) 2))))
  (:top (o-formula (+ 3 (gvl :parent :box :box :top))))
  (:bottom (o-formula (- (+ (gvl :parent :box :box :top)
			    (gvl :parent :box :box :height))
			 3)))
  (:point-list (o-formula (list (gvl :center-x) (gvl :top)
				(gvl :right) (gvl :bottom)
				(gvl :left) (gvl :bottom)
				(gvl :center-x) (gvl :top)))))

(create-instance 'NEXT-DOWN-ARROW NEXT-UP-ARROW
  (:point-list (o-formula (list (gvl :center-x) (gvl :bottom)
				(gvl :left) (gvl :top)
				(gvl :right) (gvl :top)
				(gvl :center-x) (gvl :bottom)))))


(create-instance 'light-blue-gray-fill opal:light-gray-fill
  (:foreground-color (if *color-p* opal:blue opal:black)))
(create-instance 'blue-gray-fill opal:gray-fill
  (:foreground-color (if *color-p* opal:blue opal:black)))

(create-instance 'NEXT-SHADOW-BOX opal:aggregadget
  (:left (o-formula (gvl :parent :parent :trill-left)))
  (:top (o-formula (gvl :parent :top)))
  (:width (o-formula (gvl :parent :parent :trill-width)))
  (:height (o-formula (gvl :parent :height)))
  (:shadow-width (o-formula (- (gvl :width) 2)))
  (:shadow-height (o-formula (- (gvl :height) 2)))
  (:parts
   `((:white-shadow ,opal:rectangle
		    (:left ,(o-formula (gvl :parent :left)))
		    (:top ,(o-formula (gvl :parent :top)))
		    (:width ,(o-formula (gvl :parent :width)))
		    (:height ,(o-formula (gvl :parent :height)))
		    (:filling-style ,opal:white-fill)
		    (:line-style NIL))
     (:black-shadow ,opal:rectangle
		    (:left ,(o-formula (+ 2 (gvl :parent :left))))
		    (:top ,(o-formula (+ 2 (gvl :parent :top))))
		    (:width ,(o-formula (gvl :parent :shadow-width)))
		    (:height ,(o-formula (gvl :parent :shadow-height)))
		    (:filling-style ,opal:blue-fill)
		    (:line-style NIL))
     (:box ,opal:rectangle
	   (:left ,(o-formula (+ 1 (gvl :parent :left))))
	   (:top ,(o-formula (+ 1 (gvl :parent :top))))
	   (:width ,(o-formula (- (gvl :parent :width) 3)))
	   (:height ,(o-formula (- (gvl :parent :height) 3)))
	   (:filling-style ,light-blue-gray-fill)
	   (:line-style NIL)))))

(create-instance 'NEXT-SCROLLBAR opal:aggregadget
  (:left 50)
  (:top 20)
  (:width 23)
  (:height 250)

  (:value (o-formula (inter:Clip-and-Map (second (gvl :indicator :box))
					 (gvl :bound-top)
					 (- (gvl :bound-bottom)
					    (gvl :indicator :height) 2)
					 0 100)))
  (:trill-left (o-formula (+ (gvl :left) 2)))
  (:trill-width (o-formula (- (gvl :width) 4)))
  (:bound-top (o-formula (+ 2 (gvl :top))))
  (:bound-height (o-formula (- (gvl :height) (* 2 (gvl :trill-width)) 8)))
  (:bound-bottom (o-formula (+ (gvl :bound-top) (gvl :bound-height))))

  (:parts
   ;; An invisible bounding box that the :slide interactor works in
   `((:bound-box ,opal:rectangle
		 (:left ,(o-formula (gvl :parent :left)))
		 (:top ,(o-formula (gvl :parent :bound-top)))
		 (:width ,(o-formula (gvl :parent :width)))
		 (:height ,(o-formula (gvl :parent :bound-height)))
		 (:line-style NIL))
     ;; The thin rectangle around the outside of the scroll bar
     (:outline ,opal:rectangle
	       (:left ,(o-formula (gvl :parent :left)))
	       (:top ,(o-formula (gvl :parent :top)))
	       (:width ,(o-formula (gvl :parent :width)))
	       (:height ,(o-formula (gvl :parent :height)))
	       (:filling-style ,opal:white-fill))
     ;; The gray background
     (:background ,opal:rectangle
		  (:left ,(o-formula (+ 2 (gvl :parent :left))))
		  (:top ,(o-formula (+ 2 (gvl :parent :top))))
		  (:width ,(o-formula (- (gvl :parent :width) 3)))
		  (:height ,(o-formula (- (gvl :parent :height) 3)))
		  (:line-style NIL)
		  (:filling-style ,blue-gray-fill))
     (:top-trill ,opal:aggregadget
		 (:top ,(o-formula (+ (gvl :parent :bound-bottom) 2)))
		 (:height ,(o-formula (gvl :parent :trill-width)))
		 (:parts
		  ((:box ,NEXT-SHADOW-BOX)
		   (:arrow ,NEXT-UP-ARROW)))
		 (:interactors
		  ((:trill ,NEXT-INTER
			   (:final-function
			    ,#'(lambda (interactor obj)
				 (declare (ignore interactor))
				 (let* ((slider (g-value obj :parent))
					(value (g-value slider :value)))
				   (when (> value 1)
				     (s-value slider :value (- value 1))))))))))
     (:bot-trill ,opal:aggregadget
		 (:top ,(o-formula (+ (gvl :parent :bound-bottom)
				      (gvl :parent :trill-width) 4)))
		 (:height ,(o-formula (gvl :parent :trill-width)))
		 (:parts
		  ((:box ,NEXT-SHADOW-BOX)
		   (:arrow ,NEXT-DOWN-ARROW)))
		 (:interactors
		  ((:trill ,NEXT-INTER
			   (:final-function
			    ,#'(lambda (interactor obj)
				 (declare (ignore interactor))
				 (let* ((slider (g-value obj :parent))
					(value (g-value slider :value)))
				   (when (< value 100)
				     (s-value slider :value (+ value 1))))))))))
     (:indicator ,opal:aggregadget
		 (:box (0 50 0 0)) ;; Set by interactor
		 (:left ,(o-formula (gvl :parent :trill-left)))
		 (:top ,(o-formula
			 ;; Use LET since :parent is used several times
			 (let ((parent (gvl :parent)))
			   (inter:Clip-and-Map (gv parent :value) 0 100
					       (gv parent :bound-top)
					       (- (gv parent :bound-bottom)
						  (gvl :height))))))
		 (:width ,(o-formula (gvl :parent :trill-width)))
		 (:height ,(o-formula (floor (gvl :parent :bound-height) 2)))
		 (:circle-left ,(o-formula (- (+ (gvl :left) (floor (gvl :width) 2))
					      5)))
		 (:circle-top ,(o-formula (- (+ (gvl :top) (floor (gvl :height) 2))
					     5)))
		 (:parts
		  ((:rect ,NEXT-SHADOW-BOX)
		   (:circ ,opal:circle
			  (:left ,(o-formula (gvl :parent :circle-left)))
			  (:top ,(o-formula (gvl :parent :circle-top)))
			  (:width 10) (:height 10)
			  (:filling-style ,opal:white-fill))
		   (:semi ,opal:arc
			  (:left ,(o-formula (+ 2 (gvl :parent :circle-left))))
			  (:top ,(o-formula (+ 2 (gvl :parent :circle-top))))
			  (:width 8) (:height 8)
			  (:angle1 ,(/ pi 4)) (:angle2 ,pi)))))))
  (:interactors
   `((:slide ,inter:move-grow-interactor
	     (:window ,(o-formula (gv-local :self :operates-on :window)))
	     (:continuous T)
	     (:start-where ,(o-formula (list :in-box
					     (gvl :operates-on :indicator))))
	     (:running-where ,(o-formula (list :in-box
					       (gvl :operates-on :bound-box))))
	     (:outside NIL)
	     (:obj-to-be-moved ,(o-formula (gvl :operates-on :indicator)))
	     (:feedback-obj ,(o-formula (gvl :operates-on :indicator)))
	     (:grow-p NIL)
	     (:attach-point :where-hit)
	     (:waiting-priority ,inter:high-priority-level))
     (:jump ,inter:Move-Grow-Interactor
	    (:window ,(o-formula (gv-local :self :operates-on :window)))
	    (:continuous T)
	    (:start-where ,(o-formula (list :in-box
					    (gvl :operates-on :bound-box))))
	    (:running-where ,(o-formula (list :in-box
					      (gvl :operates-on :bound-box))))
	    (:outside NIL)
	    (:obj-to-be-moved ,(o-formula (gvl :operates-on :indicator)))
	    (:grow-p NIL)
	    (:start-action
	     ,#'(lambda (interactor obj-over points-list)
		  (declare (ignore obj-over points-list))
		  (let* ((indicator (g-value interactor :obj-to-be-moved))
			 (slider (g-value indicator :parent))
			 (value (g-value slider :value)))
		    (if (> (g-value interactor :y-off) (g-value indicator :top))
			(when (<= value 95) (s-value slider :value (+ value 5)))
			(when (>= value 5) (s-value slider :value (- value 5)))
			)))))

     (:wheel ,inter:Button-Interactor
	     (:window ,(o-formula (gv-local :self :operates-on :window)))
	     (:continuous NIL)
	     (:start-event (:downscrollup :upscrollup))
	     (:start-where ,(o-formula (list :in-box
					     (gvl :operates-on :bound-box))))
	     (:outside NIL)
	     (:final-function
	      ,#'(lambda (interactor obj)
		   (declare (optimize (speed 1) (safety 3) (debug 3)))
		   (let* ((slider (g-value obj :parent))
			  (value (g-value slider :value)))
		     (if (eq (inter:event-char inter:*current-event*)
			     (first (g-value interactor :start-event)))
			 (when (< value 100)
			   (s-value slider :value (+ value 1)))
			 (when (> value 0)
			   (s-value slider :value (- value 1)))))))))))



;;;  DEMO FUNCTION: NEXT-GO
;;

(defun NEXT-Go ()

  ;; create a viewport
  (create-instance 'NEXT-vp inter:interactor-window
    (:left 500) (:top 10) (:width 200) (:height 300)
    (:title "GARNET NeXT") (:icon-title "NeXT")
    (:aggregate
     (create-instance 'NEXT-top-agg opal:aggregate)))

  ;; add the scrollbar to the window and display it
  (opal:add-component NEXT-top-agg NEXT-SCROLLBAR)
  (opal:update NEXT-vp)

  ;; Create a text object to display the scrollbar's value
  (create-instance 'NEXT-TEXT opal:text
    (:constant T :except :string)
    (:left 150) (:top 140)
    (:string (o-formula (prin1-to-string (gv NEXT-SCROLLBAR :value))))
    (:font opal:default-font))
  (opal:add-component NEXT-top-agg NEXT-TEXT)
  (opal:update NEXT-vp))


(defun NEXT-Stop ()
  ;; Remove the scrollbar so that it is not destroyed along with the window
  (opal:remove-component NEXT-top-agg NEXT-SCROLLBAR)
  (opal:destroy NEXT-vp))



;;; MOTIF SCROLL BAR MODULE 
;;

;;
;;  This file contains demo code for showing a Motif-like scroll bar.
;;  When loaded, it creates a window that contains a scroll-bar that can be
;;  operated with the left mouse button
;; 
;;  This module is intended for use with the demo-scrollbar demo, which
;;  dispays mac, next, openlook, and motif scroll bars.
;; 
;;  There is also a demo function at the end of this file to display just the
;;  Motif scroll bar:  (demo-scrollbar:motif-go) to begin.
;; 
;;  Features:
;;    1)  Leftdown in a trill box moves the indicator
;;    2)  Leftdown in the background moves the indicator
;;    3)  Leftdown + Drag in the indicator moves the indicator



;;; DEMO FUNCTION: MOTIF-GO
;;

(defun MOTIF-Go (&key double-buffered-p)

  ;; create a viewport
  (create-instance 'MOTIF-vp inter:interactor-window
    (:double-buffered-p double-buffered-p)
    (:left 700) (:top 10) (:width 200) (:height 300)
    (:title "GARNET Motif") (:icon-title "Motif"))
  (s-value MOTIF-vp
	   :aggregate
	   (create-instance 'MOTIF-top-agg opal:aggregate))

  (create-instance 'MOTIF-SCROLLBAR garnet-gadgets:Motif-V-Scroll-Bar
    (:constant T)
    (:left 75) (:top 20) (:height 250)
    (:foreground-color (if *color-p* opal:Motif-Blue opal:black)))

  ;; Create a text object to display the scrollbar's value
  (create-instance 'MOTIF-TEXT opal:text
    (:constant T :except :string)
    (:left 150) (:top 140)
    (:string (o-formula (prin1-to-string (gv MOTIF-SCROLLBAR :value))))
    (:font opal:default-font))

  (opal:add-components MOTIF-top-agg
		       ;; The motif scrollbar needs a gray background to be
		       ;; superimposed over
		       (create-instance NIL garnet-gadgets:Motif-Background
			 (:foreground-color
			  (if *color-p* opal:Motif-Blue opal:Black)))
		       ;; Now add the scrollbar and the text object
		       MOTIF-SCROLLBAR MOTIF-TEXT)

  (opal:update MOTIF-vp))


(defun MOTIF-Stop ()
  (opal:destroy MOTIF-vp))



;;; TOP LEVEL DEMO-SCROLLBAR DEMO FUNCTION
;;


(defparameter SB-VP NIL)
(defparameter SB-TOP-AGG NIL)

(defparameter SB-MAC-obj NIL)
(defparameter SB-MAC-meter NIL)

(defparameter SB-next-vp NIL)
(defparameter SB-next-agg NIL)
(defparameter SB-next-obj NIL)
(defparameter SB-next-incr-button NIL)
(defparameter SB-next-decr-button NIL)
(defparameter SB-next-meter NIL)
(defparameter SB-next-box NIL)
(defparameter SB-next-background NIL)

(defun DO-GO (&key dont-enter-main-event-loop double-buffered-p)

  ;; create a viewport
  (setq SB-VP (create-instance NIL inter:interactor-window
		 (:left 10) (:top 50) (:width 600) (:height 300)
		 (:title "GARNET Scroll Bars") (:icon-title "Scroll Bars")
                 (:double-buffered-p double-buffered-p)
		 (:aggregate
		  (setq SB-top-agg
			(create-instance NIL opal:aggregate)))))

  ;; If we get clobbered by the window manager, let the demos
  ;; controller know (if it's there).
  (when (fboundp 'common-lisp-user::Garnet-Note-Quitted)
    (pushnew
     #'(lambda (win)
	 (declare (ignore win))
	 (common-lisp-user::Garnet-Note-Quitted "DEMO-SCROLLBAR"))
     (g-value sb-vp :destroy-hooks)))
  
  ;; create the scroll bar and meter text and display them
  (setq SB-MAC-obj (MAC-make-scroll-bar SB-VP SB-top-agg 'SB-MAC-obj 64 48 21 200))
  (setq SB-MAC-meter (MAC-make-meter-text SB-top-agg 'SB-MAC-meter SB-MAC-obj
		 (formula `(- (+ (gv ',SB-MAC-obj :slider :indicator :left)
				     (floor (gv ',SB-MAC-obj :slider :indicator :width) 2))
				  (floor (gvl :width) 2)))

		 (formula `(- (+ (gv ',SB-MAC-obj :slider :indicator :top)
				     (floor (gv ',SB-MAC-obj :slider :indicator :height) 2))
				  (floor (gvl :height) 2)))))

  ;;  OPEN
  (opal:add-component SB-top-agg (create-instance 'SB-OPEN-OBJ OPEN-SCROLLBAR
				    (:left 200)))
  (opal:update SB-VP)

  ;; Create a text object to display the Openlook scrollbar's value
  (create-instance 'SB-OPEN-TEXT opal:text
     (:constant T :except :string)
     (:left 250) (:top 150)
     (:string (o-formula (prin1-to-string (gv SB-OPEN-OBJ :value))))
     (:font opal:default-font))
  (opal:add-component SB-top-agg SB-OPEN-TEXT)
  (opal:update SB-vp)

  ;;  NEXT
  (opal:add-component SB-top-agg (create-instance 'SB-NEXT-OBJ NEXT-SCROLLBAR
				    (:left 325)))
  (opal:update SB-VP)

  ;; Create a text object to display the NeXT scrollbar's value
  (create-instance 'SB-NEXT-TEXT opal:text
     (:constant T :except :string)
     (:left 380) (:top 130)
     (:string (o-formula (prin1-to-string (gv SB-NEXT-OBJ :value))))
     (:font opal:default-font))
  (opal:add-component SB-top-agg SB-NEXT-TEXT)
  (opal:update SB-vp)

  ;;  MOTIF

  ;; The motif scrollbar needs a gray background to be superimposed over
  (create-instance 'SB-MOTIF-BACKGROUND garnet-gadgets:Motif-Background
     (:left 460)(:top 15)(:width 30)(:height 260)
     (:foreground-color (if *color-p* opal:Motif-Green opal:Black)))
  (opal:add-component SB-top-agg SB-MOTIF-BACKGROUND)
  (opal:update SB-VP)

  ;; Create an instance of the motif scrollbar and add it to the window
  (create-instance 'SB-MOTIF-OBJ garnet-gadgets:Motif-V-Scroll-Bar
     (:constant T)
     (:left 465) (:top 20) (:height 250)
     (:foreground-color (if *color-p* opal:Motif-Green opal:Black)))
  (opal:add-component SB-top-agg SB-MOTIF-OBJ)
  (opal:update SB-VP)

  ;; Create a text object to display the Motif scrollbar's value
  (create-instance 'SB-MOTIF-TEXT opal:text
     (:constant T :except :string)
     (:left 520) (:top 110)
     (:string (o-formula (prin1-to-string (gv SB-MOTIF-OBJ :value))))
     (:font opal:default-font))
  (opal:add-component SB-top-agg SB-MOTIF-TEXT)
  (opal:update SB-vp)

  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop)))

(defun DO-STOP ()
  (opal:destroy SB-VP))
