;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-CLOCK; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; $Id$

;;; This is intended as a test and demonstration of the
;;; angle interactor as part of the Garnet project
;;;
;;; Designed and implemented by Ed Pervin


(in-package :DEMO-CLOCK)

(declaim (special HAND))

(defparameter *test-debug* NIL)

;------------------------------------------------------------

(defparameter clock-circle nil)
(defparameter ghost-circle nil)
(defparameter minute-hand nil)
(defparameter hour-hand nil)
(defparameter clock-agg nil)
(defparameter clock-window NIL)
(defparameter adjust-clock NIL)
(defconstant pi/6 (/ pi 6))
(defconstant pi/30 (/ pi 30))		; One minute intervals.
(defvar i)

(defun Do-Go (&key dont-enter-main-event-loop double-buffered-p)
  ;;; create a viewport
  (create-instance 'clock-window inter:interactor-window
		   (:left 200) (:top 50) (:width 400) (:height 400)
		   (:title "GARNET CLOCK") (:icon-title "Clock")
                   (:double-buffered-p double-buffered-p)
		   (:aggregate
		    (create-instance 'clock-agg opal:aggregate)))

  ;;; create outline of clock
  (create-instance 'clock-circle opal:circle
		   (:box '(30 30 160 160))
		   (:line-style opal:line-2)
		   (:filling-style opal:light-gray-fill)
		   (:left (o-formula (first (gvl :box))))
		   (:top (o-formula (second (gvl :box))))
		   (:width (o-formula (third (gvl :box))))
		   (:height (o-formula (fourth (gvl :box)))))
  
  ;;; create feedback object for moving clock
  (create-instance 'ghost-circle opal:circle
		   (:draw-function :xor)
		   (:left (o-formula (first (gvl :box))))
		   (:top (o-formula (second (gvl :box))))
		   (:width (o-formula (third (gvl :box))))
		   (:height (o-formula (fourth (gvl :box))))
		   (:visible (o-formula (gvl :obj-over)))
		   (:obj-over nil)
		   (:fast-redraw-p t)
		   (:box (list 0 0 0 0))
		   (:line-style opal:dashed-line))

  (create-instance 'hand opal:aggregadget
		   (:draw-function :xor)
		   (:fast-redraw-p t)
		   (:x1 (o-formula (opal:gv-center-x clock-circle)))
		   (:y1 (o-formula (opal:gv-center-y clock-circle)))
		   (:line-style opal:line-2)
		   (:x2 (o-formula
			 (floor (- (gvl :x1) (* (gvl :length)
						 (sin (gvl :angle)))))))
		   (:y2 (o-formula
			 (floor (- (gvl :y1) (* (gvl :length)
						 (cos (gvl :angle)))))))
		   (:parts
		    `((nil ,opal:line
			  (:x1 ,(o-formula (gvl :parent :x1)))
			  (:x2 ,(o-formula (gvl :parent :x2)))
			  (:y1 ,(o-formula (gvl :parent :y1)))
			  (:y2 ,(o-formula (gvl :parent :y2)))
			  (:line-style ,(o-formula (gvl :parent :line-style)))
			  (:length ,(o-formula (gvl :parent :length))))
		      (nil ,opal:arrowhead
			  (:from-x ,(o-formula (gvl :parent :x1)))
			  (:from-y ,(o-formula (gvl :parent :y1)))
			  (:head-x ,(o-formula (gvl :parent :x2)))
			  (:head-y ,(o-formula (gvl :parent :y2)))
			  (:line-style ,(o-formula (gvl :parent :line-style)))
			  (:length 10) (:diameter 15)
			  (:open-p t)))))

  ;;; create minute hand
  (create-instance 'minute-hand hand
		   (:angle 0.0)
		   (:length (o-formula
			     (floor (* 3 (gv clock-circle :width)) 8))))

  ;;; create hour hand
  (create-instance 'hour-hand hand
		   (:angle (o-formula (/ (gv minute-hand :angle) 12.0)))
		   (:length (o-formula
			     (floor (* 3 (gv clock-circle :width)) 16))))
  
  ;;; put clock aggregate together
  (opal:add-components clock-agg clock-circle minute-hand hour-hand ghost-circle)
  
  ;;; add numbers 1 to 12 and tick-marks to clock-agg
  (dotimes (i 12)
    (let ((cos30*i (cos (* pi/6 i)))
	  (sin30*i (sin (* pi/6 i)))
	  (offset (if (< 0 i 10) -4 -7)))
      (opal:add-components
       clock-agg
       (create-instance
	nil opal:text
	(:font (create-instance nil opal:font (:size :small)))
	(:string (if (< 0 i 10)
		     (make-string 1 :initial-element (code-char (+ 48 i)))
		     (case i (0 "12") (10 "10") (11 "11"))))
	(:left (formula `(+ (opal:gv-center-x clock-circle)
			    ,offset
			    (floor (* 3/5 (gv clock-circle :width)
				      ,sin30*i)))))
	(:top (formula `(+ (opal:gv-center-y clock-circle)
			   ,offset
			   (floor (* -3/5 (gv clock-circle :width)
				     ,cos30*i))))))
       (create-instance
	nil opal:line
	(:x1 (formula `(+ (opal:gv-center-x clock-circle)
			  (floor (* 9/20 (gv clock-circle :width)
				    ,sin30*i)))))
	(:y1 (formula `(+ (opal:gv-center-y clock-circle)
			  (floor (* 9/20 (gv clock-circle :width)
				    ,cos30*i)))))
	(:x2 (formula `(+ (opal:gv-center-x clock-circle)
			  (floor (* 1/2  (gv clock-circle :width)
				    ,sin30*i)))))
	(:y2 (formula `(+ (opal:gv-center-y clock-circle)
			  (floor (* 1/2  (gv clock-circle :width)
				    ,cos30*i)))))))))

  ;;; create interactor which moves clock (using left mouse button)
  (create-instance NIL inter:move-grow-interactor
		   (:window clock-window)
		   (:running-where T)
		   (:waiting-priority inter:high-priority-level)
		   (:outside :last)
		   (:start-where `(:in ,clock-circle))
		   (:obj-to-change nil)
		   (:feedback-obj ghost-circle)
		   (:attach-point :where-hit))
  
  ;;; create interactor whick adjusts time on clock (using right mouse button)
  (create-instance 'adjust-clock inter::angle-interactor
		   (:window clock-window)
		   (:continuous t)
		   (:start-where `(:in-box ,minute-hand))
		   (:start-event :rightdown)
		   (:obj-to-change minute-hand)
		   (:feedback-obj minute-hand)
		   (:visible t)
		   (:running-where t)
		   (:obj-being-rotated minute-hand)
		   (:center-of-rotation
		    (formula '(list (opal:gv-center-x clock-circle)
				    (opal:gv-center-y clock-circle))))
		   (:start-action
		    #'(lambda (dum-int dum-obj dum-angle)
			(declare (ignore dum-int dum-obj dum-angle))))
		   (:running-action
		    #'(lambda (dum-int dum-obj dum-ang delta)
			(declare (ignore dum-int dum-obj dum-ang))
			(s-value minute-hand :angle
				 (+ (g-value minute-hand :angle) delta))))
		   (:stop-action
		    #'(lambda (dum-int dum-obj dum-ang delta)
			(declare (ignore dum-int dum-obj dum-ang))
			(s-value minute-hand :angle
				 (+ (g-value minute-hand :angle) delta))
			(opal:update clock-window))))
		   
  ;; This interactor advances the clock 5 minutes when you hit SPACE
  (create-instance NIL inter:button-interactor
		   (:window clock-window)
		   (:continuous nil)
		   (:start-where t)
		   (:start-event #\space)
		   (:stop-action
		    #'(lambda (an-interactor final-obj-over)
			(declare (ignore an-interactor final-obj-over))
			(s-value minute-hand :angle 
				 (- (g-value minute-hand :angle) pi/6)))))

  ;; This interactor allows using the scroll wheel to adjust the clock.
  (create-instance NIL inter:scroll-wheel-interactor
    (:window clock-window)
    (:continuous nil)
    (:start-where `(:in ,clock-circle))
    (:stop-action
     #'(lambda (an-interactor final-obj-over)
	 (declare (ignore final-obj-over))
	 (s-value minute-hand :angle
		  (if (eq (inter:event-char inter:*current-event*)
			  (first (g-value an-interactor :start-event)))
		      (+ (g-value minute-hand :angle) pi/30)
		      (- (g-value minute-hand :angle) pi/30))))))
    
  (opal:update clock-window)
  ;;; print instructions
  (Format T "~%Demo-Clock:
  Press inside clock with left-button to move the entire clock.  Press with right
  button and move in a circle to change time (the minute hand will follow
  the mouse. If you have a scroll wheel you can adjust the clock by rolling the
  scroll wheel back and forth. Type space to advance clock by 5 minutes.~%")
  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))
  )

(defun Do-Stop ()
  (opal:destroy clock-window))
