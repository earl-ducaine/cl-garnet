;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; This file contains the mouse and keyboard interactors to create new objects
;;; defined by two points (e.g., rectangles and lines).  It should be
;;; loaded after Interactor and after MoveGrowInter
;;;
;;; Designed and implemented by Brad A. Myers

#|
============================================================
Change log:
        6/4/92 Brad Myers - added :input-filter like movegrow
                                        - NIL for no filtering
                                        - number - grid by that amount
                                        - (xmod xorigin ymod yorigin)
                                        - Lambda(x y)
        4/1/92  Andrew Mickish - kr::*debug-switch* ---> #-garnet-debug
       10/11/90 Brad Myers - added explicit Stop-Interactor code
        10/5/89 Brad Myers - Add Final-Function, remove create-function
	10/4/89 Roger Dannenberg - Change debugging output
        8/14/89 Brad Myers - Fixed for multiple priority levels
				- added interactor as parameter to create-function
				- made create-function be called even if too small
        7/3/89  Brad Myers - Added minimum lengths for lines
        6/26/89  Brad Myers - Fixed to have quote for create-schema
        5/30/89  Brad Myers -  call-method -> kr-send;
			allow running-where to be set after initialized
        4/20/89  Brad Myers - schema-call -> call-method
        4/14/89  Brad Myers - fixed self-deactivate
        4/7/89 Brad Myers and Dario Giuse - fixed for new KR
        2/18/89 Lynn Baumeister -- changed x,y to event in func calls
	11/25/88 Brad Myers - started
============================================================
|#

(in-package "INTERACTORS")

;;;============================================================
;;;============================================================
;;;============================================================


;;;============================================================
;;; Helper procedures for the default procedures to go into the slots
;;;============================================================

(defparameter *glo-two-points* (list 0 0 0 0))  ; use this to avoid cons-ing

;;returns a list of x y w h or x1 y1 x2 y2 depending on whether is a
;; rectangle or a line.  or returns NIL if should abort
(defun TwoPCalcPoint (an-interactor whichpoint x y)
  (if (g-value an-interactor :line-p)
      (TwoPCalcLinePoint an-interactor whichpoint x y)
      (TwoPCalcRectPoint an-interactor whichpoint x y)))

(defun TwoPCalcRectPoint (an-interactor whichpoint x y)
  (let ((firstx (g-value an-interactor :first-x))
	(firsty (g-value an-interactor :first-y))
	(flip (g-value an-interactor :flip-if-change-side))
	domin abort-if-too-small minwidth minheight w h)
    (when (and (setq minwidth (g-value an-interactor :Min-width))
	     (setq minheight (g-value an-interactor :Min-height)))
	  (setq domin T)
	  (setq abort-if-too-small (g-value an-interactor :abort-if-too-small)))
    (case whichpoint
      (1 (setf (first *glo-two-points*) x)
	 (setf (second *glo-two-points*) y)
	 (setf (third *glo-two-points*) (if domin minwidth 0))
	 (setf (fourth *glo-two-points*) (if domin minheight 0))
	 (when abort-if-too-small
	   (if-debug an-interactor (format T "** rectangle too small~%"))
	   (return-from TwoPCalcRectPoint NIL)))
      (2 (if flip ; then make sure w and h are always positive
	     (progn (setq w (1+ (abs (- x firstx))))
	       (setq h (1+ (abs (- y firsty)))))
	     (progn (setq w (1+ (- x firstx)))
	       (setq h (1+ (- y firsty)))))
	 (if domin
	     (progn
	       (when (and abort-if-too-small (or (< w minwidth)(< h minheight)))
		 (if-debug an-interactor (format T "** rectangle too small~%"))
		 (return-from TwoPCalcRectPoint NIL))
	       (setq w (max minwidth w))
	       (setq h (max minheight h)))
	     (progn (setq w (max w 0))
	       (setq h (max h 0))))
	 (setf (third *glo-two-points*) w)
	 (setf (fourth *glo-two-points*) h)
	 (when flip  ; then might have to change first and second (left, top)
	   (setf (first *glo-two-points*)
		 (if (< x firstx)
		     (1+ (- firstx w))
		     firstx))
	   (setf (second *glo-two-points*)
		 (if (< y firsty)
		     (1+ (- firsty h))
		     firsty)))))
    *glo-two-points*))

(defun TwoPCalcLinePoint (an-interactor whichpoint x y)
  (let (minlength abort-if-too-small firstx firsty xdist ydist denom)
    (when (setq minlength (g-value an-interactor :Min-length))
      (setq abort-if-too-small (g-value an-interactor :abort-if-too-small))
      (setq firstx (g-value an-interactor :first-x))
      (setq firsty (g-value an-interactor :first-y)))
    (case whichpoint
      (1 (setf (first *glo-two-points*) x)
	 (setf (second *glo-two-points*) y)
	 (setf (third *glo-two-points*)
	       (if minlength (+ x (ceiling minlength sqrt2)) x))
	 (setf (fourth *glo-two-points*)
	       (if minlength (+ y (ceiling minlength sqrt2)) y))
	 (when abort-if-too-small
	   (if-debug an-interactor (format T "** line too small~%"))
	   (return-from TwoPCalcLinePoint NIL)))
      (2 (if minlength
	     (progn  ; time for expensive math
	       (setf xdist (- x firstx))
	       (setf ydist (- y firsty))
	       (setf denom (sqrt (+ (* xdist xdist)(* ydist ydist))))
	       (if (< denom minlength)
		   (progn
		     (when abort-if-too-small
		       (if-debug an-interactor (format T "** line too small~%"))
		       (return-from TwoPCalcLinePoint NIL))
		     (if (zerop denom) ; don't devide by zero
			 (progn
			   (setf (third *glo-two-points*)
				 (+ x (ceiling minlength sqrt2)))
			   (setf (fourth *glo-two-points*)
				 (+ y (ceiling minlength sqrt2))))
			 ; not zero, use calculated points
			 (progn
			   (setf (third *glo-two-points*)
				 (+ firstx (ceiling (* xdist minlength) denom)))
			   (setf (fourth *glo-two-points*)
				 (+ firsty (ceiling (* ydist minlength) denom))))))
		   ; else not less than minimum length
		   (progn
		     (setf (third *glo-two-points*) x)
		     (setf (fourth *glo-two-points*) y))))
		   ; else don't worry about minimum length
	     (progn
	       (setf (third *glo-two-points*) x)
	       (setf (fourth *glo-two-points*) y)))))
    *glo-two-points*))

;;;============================================================
;;; Default Procedures to go into the slots
;;;============================================================

(declaim (special Two-Point-Interactor))

(defun Two-Point-Interactor-Initialize (new-two-point-schema)
  (if-debug new-two-point-schema (format T "Two-point initialize ~s~%"
					 new-two-point-schema))
  (Check-Interactor-Type new-two-point-schema inter:Two-Point-Interactor)
  (Check-Required-Slots new-two-point-schema)
  (Set-Up-Defaults new-two-point-schema)
  ) ;end initialize procedure

;;x2 and y2 will be equal to x1 y1 (w, h = 0)
(defun Two-Point-Int-Start-Action (an-interactor first-points)
  (if-debug an-interactor (format T "Two-Point int-start first points=~s~%"
				  first-points))
  ;;change feedback or object first so no flicker when turned visible
  (let ((feedback (g-value an-interactor :feedback-obj)))
    (when (and feedback first-points)
      (set-obj-list4-slot feedback
			  (if (g-value an-interactor :line-p) :points :box)
			  first-points
			  an-interactor T)
      (dbprint-feed :visible feedback T an-interactor)
      (s-value feedback :visible T))))

(defun Two-Point-Int-Running-Action (an-interactor new-points)
  (if-debug an-interactor (format T "Two-Point int-running, points=~s~%"
				  new-points))
  (let ((feedback (g-value an-interactor :feedback-obj))
	 feedbackvis)
    (when feedback
      (setq feedbackvis (g-value feedback :visible))
      (if new-points
	  (progn
	    (set-obj-list4-slot feedback
				(if (g-value an-interactor :line-p)
				    :points :box)
				new-points an-interactor T)
	    (unless feedbackvis
	      (dbprint-feed :visible feedback T an-interactor)
	      (s-value feedback :visible T)))
	  (when feedbackvis
	    (dbprint-feed :visible feedback NIL an-interactor)
	    (s-value feedback :visible NIL))))))

(defun Two-Point-Int-Outside-Action (an-interactor outside-control)
  (if-debug an-interactor (format T "Two-Point int-outside~%"))
  (unless (eq :last outside-control)
    (let ((feedback (g-value an-interactor :feedback-obj)))
      (when feedback
	(dbprint-feed :visible feedback NIL an-interactor)
	(s-value feedback :visible NIL)))))

(defun Two-Point-Int-Back-Inside-Action (an-interactor outside-control
						    new-inside-points) 
  (declare (ignore outside-control))
  (if-debug an-interactor (format T "Two-Point int-back-in, new points=~s~%"
				  new-inside-points))
  ;;first change the feedback or object to the new position, and then make it
  ;; visible
  (let ((feedback (g-value an-interactor :feedback-obj)))
    (when feedback
      (if new-inside-points
	  (progn
	    (set-obj-list4-slot feedback
				(if (g-value an-interactor :line-p) :points :box)
				new-inside-points an-interactor T)
	    (dbprint-feed :visible feedback T an-interactor)
	    (s-value feedback :visible T))
	  (progn
	    (dbprint-feed :visible feedback NIL an-interactor)
	    (s-value feedback :visible NIL))))))

(defun Two-Point-Int-Stop-Action (an-interactor final-points)
  (if-debug an-interactor (format T "Two-Point int-stop, final-points=~s~%"
				  final-points))
  ;;turn off feedback
  (let ((feedback (g-value an-interactor :feedback-obj)))
    (when feedback
      (when final-points
	(set-obj-list4-slot feedback
			    (if (g-value an-interactor :line-p) :points :box)
			    final-points an-interactor T))
      (dbprint-feed :visible feedback NIL an-interactor)
      (s-value feedback :visible NIL)))
  (kr-send an-interactor :final-function an-interactor final-points))

(defun Two-Point-Int-Abort-Action (an-interactor)
  (if-debug an-interactor (format T "Two-Point int-abort~%"))
  (let ((feedback (g-value an-interactor :feedback-obj)))
    (when feedback
      (dbprint-feed :visible feedback NIL an-interactor)
      (s-value feedback :visible NIL))))
  
;;;============================================================
;;; Go procedure utilities
;;;============================================================


;;; if continuous: (remove from start level, add to stop and abort
;;; 		    level, change state to running)
;;; save object over, call start procedure.
(defun two-point-do-start (an-interactor new-obj-over event)
  (if-debug an-interactor (format T "Two-Point starting over ~s~%" new-obj-over))
  ;; get-filtered-input is defined in MoveGrowInter.lisp
  (multiple-value-bind (x y) (Get-Filtered-Input an-interactor event)
    (let ((points (TwoPCalcPoint an-interactor 1 x y)))
      (s-value an-interactor :x x)
      (s-value an-interactor :y y)
      (if (g-value an-interactor :continuous) ;then will go to running state
	  (progn
	    (Fix-Running-Where an-interactor new-obj-over);; not likely to be needed
	    (s-value an-interactor :first-x x)
	    (s-value an-interactor :first-y y)
	    (when (g-value an-interactor :outside) ;needed if stop while outside
	      (set-obj-list4-slot-no-db an-interactor :saved-last-points points))
	    (GoToRunningState an-interactor T)
	    (kr-send an-interactor :start-action an-interactor points))
	  ;; else call stop-action
	  (progn
	    (kr-send an-interactor :stop-action an-interactor points)
	    (GoToStartState an-interactor NIL))))))

(defun two-point-do-outside (an-interactor)
  (if-debug an-interactor (format T "Two-Point outside~%"))
  (s-value an-interactor :current-state :outside)
  (kr-send an-interactor :outside-action an-interactor
	       (g-value an-interactor :outside)))
;;;filtering based on :last is handled by the :outside-action procedure
;;;  (unless (eq :last (g-value an-interactor :outside))
;;;    (s-value an-interactor :remembered-last-object NIL)))

(defun two-point-do-back-inside (an-interactor obj event)
  #-garnet-debug (declare (ignore obj))
  (if-debug an-interactor (format T "Two-Point back-inside over ~s~%" obj))
  (multiple-value-bind (x y) (Get-Filtered-Input an-interactor event)
    (s-value an-interactor :x x)
    (s-value an-interactor :y y)
    (s-value an-interactor :current-state :running)
    (let* ((points (TwoPCalcPoint an-interactor 2 x y)))
      (when (g-value an-interactor :outside) ;needed if stop while outside
	(set-obj-list4-slot-no-db an-interactor :saved-last-points points))
      (kr-send an-interactor :back-inside-action an-interactor
		   (g-value an-interactor :outside)
		   points))))

(defun two-point-do-running (an-interactor obj event)
  #-garnet-debug (declare (ignore obj))
  (if-debug an-interactor (format T "Two-Point running over ~s~%" obj))
  (multiple-value-bind (x y) (Get-Filtered-Input an-interactor event)
    (s-value an-interactor :x x)
    (s-value an-interactor :y y)
    (let* ((points (TwoPCalcPoint an-interactor 2 x y)))
      (when (g-value an-interactor :outside) ;needed if stop while outside
	(set-obj-list4-slot-no-db an-interactor :saved-last-points points))
      (kr-send an-interactor :running-action an-interactor
		   points))))
  
;;; points is the final value Calculated
(defun two-point-do-stop-helper (an-interactor points)
  (GoToStartState an-interactor T)
  (kr-send an-interactor :Stop-Action an-interactor points))

(defun two-point-do-stop (an-interactor obj event)
  #-garnet-debug (declare (ignore obj))
  (if-debug an-interactor (format T "Two-Point stop over ~s~%" obj))
  (multiple-value-bind (x y) (Get-Filtered-Input an-interactor event)
    (s-value an-interactor :x x)
    (s-value an-interactor :y y)
    (two-point-do-stop-helper an-interactor (TwoPCalcPoint an-interactor 2 x y))))

;;; This is used if explicitly call Stop-Interactor.  It uses the last point
(defun two-point-explicit-stop (an-interactor)
  (if-debug an-interactor (format T "Two point explicit stop~%"))
  (let ((x (g-value an-interactor :x))
	(y (g-value an-interactor :y)))
    (two-point-do-stop-helper an-interactor (TwoPCalcPoint an-interactor 2 x y))))

(defun two-point-do-abort (an-interactor become-inactive event)
  (declare (ignore event become-inactive))
  (if-debug an-interactor (format T "Two-Point aborting~%"))
  (GoToStartState an-interactor T)
  (kr-send an-interactor :Abort-Action an-interactor))

;;;check to see if need to stop or abort based on whether :outside = :last
(defun two-point-do-outside-stop (an-interactor event)
  (if-debug an-interactor (format T "Two-Point stop outside~%"))
  (if (and (eq :last (g-value an-interactor :outside))
	   (car (g-value an-interactor :saved-last-points)))
      (two-point-do-stop-helper an-interactor
			   (g-value an-interactor :saved-last-points))
      (two-point-do-abort an-interactor NIL event)))

;;;============================================================
;;; Two-Point schema
;;;============================================================

(Create-Schema 'inter:Two-Point-Interactor
		     (:is-a inter:interactor)
		     (:name :First-Two-Point-interactor)
		     (:start-action 'Two-Point-Int-Start-Action)
		     (:running-action 'Two-Point-Int-Running-Action)
		     (:stop-action 'Two-Point-Int-Stop-Action)
		     (:abort-action 'Two-Point-Int-Abort-Action)
		     (:outside-action 'Two-Point-Int-Outside-Action)
		     (:back-inside-action 'Two-Point-Int-Back-Inside-Action)
		     (:line-p NIL) ; line T, or rectangle NIL
		     (:Min-width NIL); minimum allowed width and height
		     (:Min-height NIL)
		     (:Min-length NIL) ; minimum length for lines
		     (:input-filter NIL)
		     (:abort-if-too-small NIL)
		     (:flip-if-change-side T) ; for rectangles-change left,top
						; if move up left of it
		     (:saved-last-points NIL) ; used if stop and outside and
						; outside control is :last
		     (:Go 'General-Go)  ; proc executed when events happen
		     (:Do-Start 'Two-Point-Do-Start)     ; these are
		     (:Do-Running 'Two-Point-Do-Running) ;   called by GO
		     (:Do-Explicit-Stop 'Two-Point-Explicit-Stop) ;for stop-inter
		     (:Do-Stop 'Two-Point-Do-Stop)       ;   to do
		     (:Do-Abort 'Two-Point-Do-Abort)     ;   the real work.
		     (:Do-Outside 'Two-Point-Do-Outside) ;   They call the
		     (:Do-Back-Inside 'Two-Point-Do-Back-Inside)  ; appropriate
		     (:Do-Outside-Stop 'Two-Point-Do-Outside-Stop); -action
								     ; procedures
		     (:initialize 'Two-Point-Interactor-Initialize))
