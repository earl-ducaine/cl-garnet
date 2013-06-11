;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-TWOP; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file contains demo code for showing things that change size in a window
;;; When loaded, it creates a window that contains a number of graphical
;;; editors that can be
;;; operated with the left mouse button
;;;
;;; This is intended as a test and demonstration of the Two-point
;;; interactor as part of the Garnet project.
;;; 
;;; ** Call (demo-twop:Do-Go) to start and (demo-twop:Do-Stop) to stop **
;;;
;;; Designed and implemented by Brad A. Myers
;;; CHANGE LOG:
;;; 30-May-94 Marty Geier - Changed main window position in do-go
;;; 10-Feb-92 ECP Changed formula to o-formula
;;;  1-Aug-90 ECP Since width and height of windows are based on inside, not
;;;		  outside now, I had to change size of agg1

(in-package :DEMO-TWOP)

(defparameter *test-debug* NIL)


;;; ********************************************************************
;;; The main create procedure
;;; ********************************************************************

(defparameter agg1 NIL)
(defparameter *glo-tone* 0)
(defparameter twop-feed NIL)
(defparameter twop-line NIL)

(defun Create-New-Rect (interactor point-list)
  (declare (ignore interactor))
  (let (obj)
    (when *test-debug* (format T "creating rect; ~s~%" point-list))
    (setq obj (create-instance NIL opal:rectangle
				 (:left (first point-list))
				 (:top (second point-list))
				 (:width (third point-list))
				 (:height (fourth point-list))
				 (:filling-style
				  (case *glo-tone*
				    (0 opal:white-fill)
				    (1 opal:light-gray-fill)
				    (2 opal:gray-fill)
				    (3 opal:dark-gray-fill)))))
    (setq *glo-tone* (if (eq *glo-tone* 3) 0 (1+ *glo-tone*)))
    (opal:add-component agg1 obj :where :behind twop-feed)
    (when *test-debug* (format T "created ~s~%" obj))
    obj))

(defun Create-New-Line (interactor point-list)
  (declare (ignore interactor))
  (let (obj)
    (when *test-debug* (format T "creating line; ~s~%" point-list))
    (setq obj (create-instance NIL opal:line
				 (:x1 (first point-list))
				 (:y1 (second point-list))
				 (:x2 (third point-list))
				 (:y2 (fourth point-list))
				 ))
    (opal:add-component agg1 obj :where :behind twop-feed)
    (when *test-debug* (format T "created ~s~%" obj))
    obj))

;;; ********************************************************************
;;; Main procedures
;;; ********************************************************************


(defparameter vp NIL)
(defparameter inter1 NIL)
(defparameter inter2 NIL)

(defun Do-Go (&key dont-enter-main-event-loop double-buffered-p)
  ;;; create a viewport
  (setq vp (create-instance NIL inter:interactor-window
			    (:left 100) (:top 100) (:width 500) (:height 300)
                            (:double-buffered-p double-buffered-p)
			    (:title "GARNET 2 Points") (:icon-title "2 points")))

  ;;; create the top level aggregate in the window
  (setq agg1 (create-instance NIL opal:aggregate (:overlapping T)
		   (:left 10)(:top 10)(:width 480)(:height 280)))
  (s-value vp :aggregate agg1)
  (opal:add-component agg1 (create-instance NIL opal:rectangle (:left 10)
					    (:top 10)(:width 480)(:height
								  280)))
  (opal:add-component agg1
      (setq twop-feed (create-instance NIL opal:rectangle
			   (:fast-redraw-p T)
			   (:draw-function :xor)
			   (:name "Interim Rect feedback")
			   (:left (o-formula (first (gvl :box))))
			   (:top (o-formula (second (gvl :box))))
			   (:width (o-formula (third (gvl :box))))
			   (:height (o-formula (fourth (gvl :box))))
			   (:visible NIL)
			   (:box '(0 0 0 0))
			   (:line-style opal:dashed-line))))
  (opal:add-component agg1
      (setq twop-line (create-instance NIL opal:line
			   (:fast-redraw-p T)
			   (:draw-function :xor)
			   (:name "Interim Line feedback")
			   (:x1 (o-formula (first (gvl :points))))
			   (:y1 (o-formula (second (gvl :points))))
			   (:x2 (o-formula (third (gvl :points))))
			   (:y2 (o-formula (fourth (gvl :points))))
			   (:visible NIL)  
			   (:points '(0 0 0 0))
			   (:line-style opal:dashed-line))))

  (setq inter1 (create-instance NIL inter:Two-Point-Interactor
	(:Window vp)
	(:start-where `(:in ,agg1))
	(:final-function #'Create-New-Rect)
	(:line-p NIL)
	(:feedback-obj twop-feed)
	(:Min-width 20)
	(:Min-height 30)
	))
  (setq inter2 (create-instance NIL inter:Two-Point-Interactor
	(:Window vp)
	(:start-where `(:in ,agg1))
	(:start-event :control-leftdown)
	(:final-function #'Create-New-Line)
	(:line-p T)
	(:feedback-obj twop-line)
	(:Min-width NIL)
	(:Min-height NIL)
	(:abort-if-too-small NIL)
	))

  ;; ** Do-Go **
  (opal:update vp) 
  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))

  (format t "Demo-Twop:
   This is a demonstration of the two-point-interactor.  Click and drag with
   the left mouse button to create rectangles.~%")
  )

;; ** STOP **
(defun Do-Stop ()
  (opal:destroy vp))

