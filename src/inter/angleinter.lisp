;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;*******************************************************************;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;*******************************************************************;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;*******************************************************************;;

;;; $Id$
;;


;;; This file contains the mouse and keyboard interactors to rotate objects
;;  and get angular measurements.
;; 
;;  It should be loaded after Interactor.lisp and after movegrowinter.lisp
;; 
;;  Designed and implemented by Brad A. Myers


;;; Change log:
;;   4/1/92  Andrew Mickish   - kr::*debug-switch* ---> #-garnet-debug
;;  10/11/90 Brad Myers       - added explicit Stop-Interactor code
;;  10/5/89 Brad Myers        - Change :obj-to-rotate to :obj-to-change;
;; 			        Don't set :visible of feedback object (use :obj-over)
;; 			        Fixed some small errors,
;; 			        Add Final-Function
;;  10/04/89 Roger Dannenberg - Change debugging output
;;  09/22/89 Brad Myers       - Made more robust when :start-where = T
;;  08/14/89 Brad Myers       - Fixed for multiple priority levels
;;  06/26/89 Brad Myers       - Fixed to have quote for create-schema
;;  04/25/89 Ed Pervin        - updated to X11
;;  11/01/88 Brad Myers       - changed to return angle delta
;;  09/26/88 Brad Myers       - started 

(in-package "INTERACTORS")



;;; Angle-Interactor
;;


;; Helper procedures for the default procedures to go into the slots

(defun obj-or-feedback-rotate (feedback object-being-rotated angle inter)
  (if feedback
      (progn
	(dbprint-feed :angle feedback angle inter)
	(s-value feedback :angle angle))
      (when (schema-p object-being-rotated)
	(dbprint :angle object-being-rotated angle inter)
	(s-value object-being-rotated :angle angle))))

(defun angle-feedback-visible (an-interactor feedback object-being-rotated vis)
  "makes the feedback for interactor be visible if vis = T or invisible if
vis = NIL"
  #-garnet-debug (declare (ignore an-interactor))
  (when feedback
    (dbprint-feed :obj-over feedback (if vis object-being-rotated NIL)
		  an-interactor)
    (s-value feedback :obj-over (if vis object-being-rotated NIL))))
  

;;; Default Procedures to go into the slots
;;

(declaim (special Angle-Interactor))

(defun Angle-Interactor-Initialize (new-Angle-schema)
  (if-debug new-Angle-schema (format T "Angle initialize ~s~%" new-Angle-schema))
  (Check-Interactor-Type new-Angle-schema inter:Angle-Interactor)
  (Check-Required-Slots new-Angle-schema)
  (Set-Up-Defaults new-Angle-schema))

(defun Angle-Int-Start-Action (an-interactor object-being-rotated first-angle)
  (if-debug an-interactor
	    (format T "Angle int-start moving ~s firstangle=~s~%"
		    object-being-rotated first-angle))
  (s-value an-interactor :saved-original-angle
	   ;; ** BUG ** This won't work if angle clipped, or whatever.
	   ;; select-move gets around this problem by reading top,left
	   ;; rather than box, but that doesn't work here.
	   (g-value object-being-rotated :angle))
  ;;move feedback or object first so no flicker when turned visible
  (let ((feedback (g-value an-interactor :feedback-obj)))
    (obj-or-feedback-rotate feedback object-being-rotated first-angle an-interactor)
    (when feedback (angle-feedback-visible an-interactor feedback
					      object-being-rotated T))))

(defun Angle-Int-Running-Action (an-interactor object-being-rotated
					    new-angle angle-delta)
  (declare (ignore angle-delta))
  (if-debug an-interactor (format T "Angle int-running, obj = ~s, angle=~s~%"
				  object-being-rotated new-angle))
  (obj-or-feedback-rotate (g-value an-interactor :feedback-obj)
			object-being-rotated new-angle an-interactor))

(defun Angle-Int-Outside-Action (an-interactor outside-control
						  object-being-rotated) 
  (if-debug an-interactor (format T "Angle int-outside, obj = ~s~%"
				  object-being-rotated))
  (unless (eq :last outside-control)
    (let ((feedback (g-value an-interactor :feedback-obj))
	  val)
      (if feedback
	  (angle-feedback-visible an-interactor feedback object-being-rotated NIL)
	  (when (schema-p object-being-rotated)
	    (setq val (g-value an-interactor :saved-original-angle))
	    (dbprint :angle object-being-rotated val an-interactor)
	    (s-value object-being-rotated :angle val))))))

(defun Angle-Int-Back-Inside-Action (an-interactor outside-control
					       object-being-rotated
					       new-angle) 
  (if-debug an-interactor 
	    (format T "Angle int-back-inside, obj = ~s new angle=~s~%" 
		    object-being-rotated new-angle))
  ;; first move the feedback or object to the new position, and then make it
  ;; visible, if necessary
  (let ((feedback (g-value an-interactor :feedback-obj)))
    (obj-or-feedback-rotate feedback object-being-rotated
			    new-angle an-interactor)
    (when (and feedback (null outside-control))
      (angle-feedback-visible an-interactor feedback object-being-rotated T))))

(defun Angle-Int-Stop-Action (an-interactor object-being-rotated 
			      final-angle angle-delta)
  #-garnet-debug (declare (ignore angle-delta))
  (if-debug an-interactor (format T "Angle int-stop obj ~s final-pos=~s delta=~s~%"
				  object-being-rotated final-angle angle-delta))
  ;; turn off feedback
  (angle-feedback-visible an-interactor (g-value an-interactor :feedback-obj)
			  object-being-rotated NIL)
  ;; set object to final position
  (when (schema-p object-being-rotated)
    (dbprint :angle object-being-rotated final-angle an-interactor)
    (s-value object-being-rotated :angle final-angle))
  (KR-Send an-interactor :final-function an-interactor final-angle))

(defun Angle-Int-Abort-Action (an-interactor object-being-rotated)
  (if-debug an-interactor (format T "Angle int-abort moving ~s~%"
				  object-being-rotated))
  (let ((feedback (g-value an-interactor :feedback-obj))
	val)
    (if feedback
	(angle-feedback-visible an-interactor feedback object-being-rotated NIL)
	(when (schema-p object-being-rotated)
	  (setq val (g-value an-interactor :saved-original-angle))
	  (dbprint :angle object-being-rotated val an-interactor)
	  (s-value object-being-rotated :angle val)))))
  


;;; Go procedure helpers
;;

(defconstant 2PI (* 2 PI))

(defun Calc-Center (object)
  (list (+ (g-value object :left) (floor (g-value object :width) 2))
	(+ (g-value object :top) (floor (g-value object :height) 2))))

(defun Calc-Angle (an-interactor center x y)
  #-garnet-debug (declare (ignore an-interactor))
  (let ((angle (atan (- (second center) y) (- x (first center)))))
    (setq angle (if (< angle 0) (+ 2PI angle) angle))
    (if-debug an-interactor
	      (format T "Center=~s x=~s y=~s angle=~s~%" center x y angle))
    angle))

;; assume only small increments, so if too large, assume have crossed the zero
;; line (in general, don't really know which way the user has moved between points,
;; Could keep the old delta and check if in same direction, but that is
;; too hard.
(defun Calc-Delta (an-interactor oldangle newangle)
  #-garnet-debug (declare (ignore an-interactor))
  (let ((delta (- newangle oldangle)))
    (cond ((> delta PI)(setq delta (- delta 2PI)))
	  ((< delta (- PI))(setq delta (+ delta 2PI)))) 
    (if-debug an-interactor (format T "Old angle=~s, new=~s, delta angle= ~s~%"
				    oldangle newangle delta))
    delta))


;;; Go procedure utilities
;;

(defun Angle-do-start (an-interactor new-obj-over event)
  "if continuous: (remove from start level, add to stop and abort
 		   level, change state to running)
save object over, call start procedure."
  (if-debug an-interactor (format T "Angle starting over ~s~%" new-obj-over))
  ;; if obj-to-change supplied, then use that, otherwise use whatever was
  ;; under the mouse when started
  (let ((obj (or (g-local-value an-interactor :obj-to-change) new-obj-over))
	(x (event-x event))
	(y (event-y event))
	center angle)
    (s-value an-interactor :obj-being-rotated obj)
    (setq center (or (g-value an-interactor :center-of-rotation)
		     (Calc-Center obj)))
    (s-value an-interactor :center-to-use center)
    (setq angle (calc-angle an-interactor center x y))
    (if (g-value an-interactor :continuous)  
	;; then will go to running state
	(progn
	  (Fix-Running-Where an-interactor new-obj-over)
	  (s-value an-interactor :saved-last-angle angle)
	  (GoToRunningState an-interactor T)
	  (kr-send an-interactor :start-action an-interactor obj angle)
	  )
	;; else call stop-action
	(progn
	  (kr-send an-interactor :stop-action an-interactor obj angle)
	  (GoToStartState an-interactor NIL)))))

(defun Angle-do-outside (an-interactor)
  (if-debug an-interactor (format T "Angle outside~%"))
  (s-value an-interactor :current-state :outside)
  (kr-send an-interactor :outside-action an-interactor
	       (g-value an-interactor :outside)
	       (g-value an-interactor :obj-being-rotated)))
;; filtering based on :last is handled by the :outside-action procedure
;;;    (unless (eq :last (g-value an-interactor :outside))
;;;    (s-value an-interactor :remembered-last-object NIL)))

(defun Angle-do-back-inside (an-interactor obj event)
  #-garnet-debug (declare (ignore obj))
  (let ((x (event-x event))
	(y (event-y event)))
    (if-debug an-interactor (format T "Angle back-inside over ~s~%" obj))
    (s-value an-interactor :current-state :running)
    (let* ((moving-obj (g-value an-interactor :obj-being-rotated))
	   (angle (Calc-Angle an-interactor
			      (g-value an-interactor :center-to-use) x y)))
      (kr-send an-interactor :back-inside-action an-interactor
	       (g-value an-interactor :outside) moving-obj angle)
      (s-value an-interactor :saved-last-angle angle))))

(defun Angle-do-running (an-interactor obj event)
  #-garnet-debug (declare (ignore obj))
  (let ((x (event-x event))
	(y (event-y event)))
    (if-debug an-interactor (format T "Angle running over ~s~%" obj))
    (let* ((moving-obj (g-value an-interactor :obj-being-rotated))
	   (angle (Calc-Angle an-interactor
			      (g-value an-interactor :center-to-use) x y)))
      (kr-send an-interactor :running-action an-interactor
	       moving-obj angle
	       (Calc-Delta an-interactor
			   (g-value an-interactor :saved-last-angle) angle))
      (s-value an-interactor :saved-last-angle angle))))

(defun Angle-do-stop-helper (an-interactor angle angle-delta)
  (GoToStartState an-interactor T)
  (kr-send an-interactor :Stop-Action an-interactor 
	   (g-value an-interactor :obj-being-rotated) angle angle-delta))

(defun Angle-Explicit-Stop (an-interactor)
  (if-debug an-interactor (format T "Angle explicit stop~%"))
  (let ((angle (g-value an-interactor :saved-last-angle)))
    (Angle-do-stop-helper an-interactor angle 0))) ; angle delta is zero

(defun Angle-do-stop (an-interactor obj event)
  #-garnet-debug (declare (ignore obj))
  (let ((x (event-x event))
	(y (event-y event)))
    (if-debug an-interactor (format T "Angle stop over ~s at~%" obj))
    (let ((angle (Calc-Angle an-interactor
			     (g-value an-interactor :center-to-use) x y)))
      (Angle-do-stop-helper an-interactor angle
			    (Calc-Delta an-interactor
					(g-value an-interactor :saved-last-angle)
					angle)))))

(defun Angle-do-abort (an-interactor become-inactive event)
  (declare (ignore become-inactive event))
  (if-debug an-interactor (format T "Angle aborting~%"))
  (GoToStartState an-interactor T)
  (kr-send an-interactor :Abort-Action an-interactor
	   (g-value an-interactor :obj-being-rotated)))

;; check to see if need to stop or abort based on whether :outside = :last
(defun Angle-do-outside-stop (an-interactor event)
  (if-debug an-interactor (format T "Angle stop outside~%"))
  (if (eq :last (g-value an-interactor :outside))
      (Angle-do-stop-helper an-interactor
			    (g-value an-interactor :saved-last-angle) 0.0)
      (Angle-do-abort an-interactor NIL event)))


;;; Angle schema
;;

(Create-Schema 'inter:Angle-Interactor
	       (:is-a inter:Interactor)
	       (:name :First-Angle-interactor)
	       (:start-action 'Angle-Int-Start-Action)
	       (:running-action 'Angle-Int-Running-Action)
	       (:stop-action 'Angle-Int-Stop-Action)
	       (:abort-action 'Angle-Int-Abort-Action)
	       (:outside-action 'Angle-Int-Outside-Action)
	       (:back-inside-action 'Angle-Int-Back-Inside-Action)
	       (:obj-to-change NIL)			 ;supplied by application program
	       (:center-of-rotation NIL)		 ; where rotate around
	       (:saved-original-angle NIL)		 ; used for ABORT
	       (:saved-last-angle NIL)			 ; used if stop and outside and 
					                 ; outside control is :last
	       (:obj-being-rotated NIL)			 ; saved object under the mouse
	       (:Go 'General-Go)			 ; proc executed when events happen
	       (:Do-Start 'Angle-Do-Start)		 ; these are
	       (:Do-Running 'Angle-Do-Running)		 ; called by GO
	       (:Do-Explicit-Stop 'Angle-Explicit-Stop)	 ; for stop-interactor
	       (:Do-Stop 'Angle-Do-Stop)		 ; to do
	       (:Do-Abort 'Angle-Do-Abort)		 ; the real work.
	       (:Do-Outside 'Angle-Do-Outside)		 ; They call the
	       (:Do-Back-Inside 'Angle-Do-Back-Inside)	 ; appropriate
	       (:Do-Outside-Stop 'Angle-Do-Outside-Stop) ; -action
					                 ; procedures
	       (:initialize 'Angle-Interactor-Initialize))

