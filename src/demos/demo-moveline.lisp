;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-MOVELINE; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; This file contains demo code for testing if the ends of lines can be
;;; changed with the mouse.
;;;
;;; This is intended as a test and demonstration of the move-grow
;;; interactor as part of the Garnet project.
;;; 
;;; ** Call (demo-moveline:Do-Go) to start and (demo-moveline:Do-Stop) to stop **
;;;
;;; Designed and implemented by Brad A. Myers
;;;
;;; 27-May-92 Pervin - The latest CMUCL requires that the
;;;                    argument to random be declared an integer.
;;; 09-Apr-92 Mickish - Changed create-instances of opal:default-line-styles to
;;;                     opal:line-styles.
;;; 13-Feb-92 Pervin - Merged demo-moveline and color-demo-moveline
;;;


(in-package :DEMO-MOVELINE)

(declaim (special MYLINE VP AGG FEEDBACK INTER1 INTER2 INTER3 INTER4 INTER5
		  INTER6 INTER7))

(defparameter *test-debug* NIL)

(defvar *color-p* (g-value opal:color :color-p))

(create-instance 'myline opal:line
		 (:points (list 0 0 0 0))
		 (:color 0)
		 (:x1 (o-formula (first (gvl :points))))
		 (:y1 (o-formula (second (gvl :points))))
		 (:x2 (o-formula (third (gvl :points))))
		 (:y2 (o-formula (fourth (gvl :points)))))

(if *color-p*
    (s-value myline :line-style
                    (o-formula (case (mod (gvl :color) 6)
				        ; thin-line
				   (0 (create-instance nil opal:line-style
					(:constant T)
					(:line-thickness 0)
					(:foreground-color opal:blue)))
				        ; line-2
				   (1 (create-instance nil opal:line-style
					(:constant T)
					(:line-thickness 2)
					(:foreground-color opal:green)))
				        ; line-4
				   (2 (create-instance nil opal:line-style
					(:constant T)
					(:line-thickness 4)
					(:foreground-color opal:yellow)))
				        ; line-8
				   (3 opal:line-8)
					; dotted-line
				   (4 (create-instance nil opal:line-style
					(:constant T)
					(:line-style :dash)
					(:line-thickness 1)
					(:dash-pattern '(1 1))
					(:foreground-color opal:blue)
					(:background-color opal:yellow)))
				   (5 opal:dashed-line))))
    (s-value myline :line-style
		    (o-formula (case (mod (gvl :color) 6)
				   (0 opal:thin-line)
				   (1 opal:line-2)
				   (2 opal:line-4)
				   (3 opal:line-8)
				   (4 opal:dotted-line)
				   (5 opal:dashed-line)))))
				

(defun create-lines (num agg)
  (let (obj
	(width (g-value agg :width))
	(height (g-value agg :height)))
    (dotimes (i num)
      (setq obj (create-instance NIL myline
				 (:points (list (random (the integer width))
						(random (the integer height))
						(random (the integer width))
						(random (the integer height))))
				 (:color i)))
      (when *test-debug*
	(format T "created line ~s: ~s (~s ~s ~s ~s) color = ~s~%"
		i obj (g-value obj :x1)(g-value obj :y1)(g-value obj :x2)
		(g-value obj :y2)(g-value obj :line-style)))
      (opal:add-component agg obj))))
				 
;;; ********************************************************************
;;; Main procedures
;;; ********************************************************************


(defun Do-Go (&key dont-enter-main-event-loop double-buffered-p)
  ;;; create a window
  (create-instance 'vp inter:interactor-window (:left 300) (:top 50)
			    (:width 300) (:height 200)(:title "GARNET Move-lines")
                            (:double-buffered-p double-buffered-p)
			    (:icon-title "Move-Lines"))

  ;;; create the top level aggregate in the window
  (create-instance 'agg opal:aggregate
		   (:left 0)(:top 0)(:width 300)(:height 200))
  (s-value vp :aggregate agg)

  (create-lines 6 agg)

  (create-instance 'feedback Opal:line
		   (:points (list 0 0 0 0))
		   (:x1 (o-formula (first (gvl :points))))
		   (:y1 (o-formula (second (gvl :points))))
		   (:x2 (o-formula (third (gvl :points))))
		   (:y2 (o-formula (fourth (gvl :points))))
		   (:obj-over NIL)
		   (:visible (o-formula (gvl :obj-over)))
		   (:line-style (if *color-p*
				  (create-instance nil opal:line-style
				    (:constant T)
				    (:line-style :dash)
				    (:dash-pattern '(4 4))
				    (:foreground-color opal:red))
				  opal:dashed-line))
		   (:draw-function :xor)
		   (:fast-redraw-p T))
  (opal:add-component agg feedback)
  (opal:update vp)

  (Create-Instance 'inter1 inter:move-grow-interactor
	(:window vp)
	(:min-length 40)
	(:start-where `(:element-of ,agg :type ,myline))
	(:attach-point :where-hit)
	(:feedback-obj feedback)
	(:grow-p t)
	(:line-p t))

  (Create-Instance 'inter2 inter:move-grow-interactor
	(:window vp)
	(:start-where `(:element-of ,agg :type ,myline))
	(:attach-point 1)
	(:line-p t)
	(:grow-p t)
        (:start-event :middledown))

  (Create-Instance 'inter3 inter:Move-Grow-Interactor
	(:window vp)
	(:start-where `(:element-of ,agg :type ,myline))
	(:attach-point 2)
	(:line-p t)
	(:grow-p t)
        (:start-event :rightdown))

  (Create-Instance 'inter4 inter:move-grow-interactor
        (:start-event :shift-leftdown)
	(:window vp)
	(:start-where `(:element-of ,agg :type ,myline))
	(:attach-point :where-hit)
	(:feedback-obj feedback)
	(:line-p t))

  (Create-Instance 'inter5 inter:move-grow-interactor
	(:window vp)
	(:start-where `(:element-of ,agg :type ,myline))
	(:attach-point 1)
	(:line-p t)
        (:start-event :shift-middledown))

  (Create-Instance 'inter6 inter:Move-Grow-Interactor
	(:window vp)
	(:start-where `(:element-of ,agg :type ,myline))
	(:attach-point 2)
	(:line-p t)
        (:start-event :shift-rightdown))

  (Create-Instance 'inter7 inter:Move-Grow-Interactor
	(:window vp)
	(:start-where `(:element-of ,agg :type ,myline))
	(:attach-point :Center)
	(:line-p t)
        (:start-event :control-leftdown))

  ;; ** Do-Go **
  (opal:update vp) 
  (Format T "~%Demo-Moveline: 
  Press on a line with the left button to cause the nearest end point to
     grow with the mouse.
  Press with middle button to move the first end point only.
  Press with right button to move the second end point only.
  Press with shift-left to move from where pressed.
  Press with shift-middle to move from first end point.
  Press with shift-right to move from second end point.
  Press with control-left to move from center.~%")

  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))

  )

;; ** STOP **
(defun Do-Stop ()
  (opal:destroy vp))

