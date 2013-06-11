;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;

;;; $Id$
;;


;;; This file contains the mouse and keyboard interactors to handle buttons.
;;  It should be loaded after Interactor.lisp and after menuinter.lisp
;; 
;;  Designed and implemented by Brad A. Myers


;;; Change log:
;;   7/16/92 Brad Myers - fixed small bug where :continuous NIL
;;                        meant couldn't destroy in final-function: Robert Schnelbach
;;   7/14/92 Brad Myers - Removed error "wrong object"
;;   5/28/92 Brad Myers - added timers
;;   4/1/92  Andrew Mickish - kr::*debug-switch* ---> #-garnet-debug
;;   1/29/92 Brad Myers - added :slots-to-set parameter
;;  10/11/90 Brad Myers - added explicit Stop-Interactor code
;;   9/21/90 Brad Myers - fixed final-feedback so works if :start-where
;;                        returns :none, and if :start-where is T
;;   7/23/90 Brad Myers - added new parameter to
;;                        Destroy-Extra-Final-Feedback-Objs
;;   7/11/90 Ed Pervin - new :destroy-me method
;;   6/14/90 Brad Myers - added destroy method
;;   6/8/90 Brad Myers - add final-feedback-objs
;;  12/5/89 Ed Pervin  - Removed extra `)'
;;  10/5/89 Brad Myers - Add Final-Function
;;  10/4/89 Roger Dannenberg - Change debugging output
;;   9/22/89 Brad Myers - Made more robust when :start-where = T
;;   8/14/89 Brad Myers - Fixed for multiple priority levels
;;   6/26/89 Brad Myers - Fixed to have quote for create-schema
;;   6/8/89  Brad Myers -  Fixed so how-set handled consistently
;;   5/30/89  Brad Myers - call-method -> kr-send;
;; 			   allow running-where to be set after initialized
;;   4/20/89  Brad Myers - schema-call -> call-method
;;   4/14/89  Brad Myers - fixed self-deactivate
;;   4/7/89 Dario Giuse and Brad Myers - changed to work with new KR
;;   2/15/89 Lynn Baumeister - changed x,y to event in func calls
;;   9/9/88 Brad Myers - started


(in-package "INTERACTORS")

;;;  Utilities for timer
;;

;; This is called by the timer process when the time is elapsed.  Do
;; the final-feedback appropriately.
(defun Button-Timer-Handler (inter)
  (if-debug inter (format T "Button TIMER Handler for ~s~%" inter))
  (case (g-value inter :timer-wait-status)
    (:should-start (s-value inter :timer-wait-status :started)
		   (launch-timer-process inter
					 (g-value inter :timer-initial-wait) T))
    (:started (s-value inter :timer-wait-status :repeating)
	      (launch-timer-process inter
				    (g-value inter :timer-repeat-wait)
				    NIL)) 
    (:repeating T)			; fine
    (T (error "Interactor ~s got timer event but status wrong" inter)))
  (let ((final-obj-over (g-value inter :remembered-last-object))
	(how-set (g-value inter :how-set))
	(main-agg (g-value inter :main-aggregate))
	(agg-sel-slot (agg-sel-slot inter))
	)
    (calc-set-obj-slot inter
		       final-obj-over how-set
		       (if (eq final-obj-over main-agg)
			   NIL 
			   (g-value main-agg agg-sel-slot)))
    (KR-Send inter :final-function inter final-obj-over)))

;;; Button-Interactor
;;

;;Turn on feedback
(defun Button-Turn-On-Feedback (an-interactor  new-obj-over)
  (let ((interim-sel-slot (interim-sel-slot an-interactor))
	(feedbackobj (g-value an-interactor :feedback-obj)))
    (when feedbackobj 
      (dbprint-feed :obj-over feedbackobj new-obj-over an-interactor)
      (s-value feedbackobj :obj-over new-obj-over))
    (when (and new-obj-over interim-sel-slot
	       (schema-p new-obj-over))
      (dbprint interim-sel-slot new-obj-over T an-interactor)
      (s-value new-obj-over interim-sel-slot T))
    ))

;;; Default Procedures to go into the slots
;;

(declaim (special Button-Interactor))

(defun Button-Interactor-Initialize (new-Button-schema)
  (if-debug new-Button-schema (format T "Button initialize ~s~%" new-Button-schema))
  (Check-Interactor-Type new-Button-schema inter:button-interactor)
  (Check-Required-Slots new-Button-schema)
  (check-slots-to-set new-Button-schema)
  (Set-Up-Defaults new-Button-schema)
  (s-value new-Button-schema :remembered-last-object NIL) ; this slot must be local
  )

;; This procedure not used for this type of interactor
(defun Button-Int-Running-Action (an-interactor prev-obj-over new-obj-over)
  #-garnet-debug (declare (ignore an-interactor
				  prev-obj-over new-obj-over))
  (if-debug an-interactor (format T "Button int-running, old = ~s, new= ~s~%"
				  prev-obj-over new-obj-over)))

(defun Button-Int-Start-Action (an-interactor obj-under-mouse)
  (if-debug an-interactor (format T "Button int-start over ~s~%"
				  obj-under-mouse))
  (Button-Turn-On-Feedback an-interactor obj-under-mouse)
  (when (g-value an-interactor :timer-repeat-p)
    (s-value an-interactor :timer-wait-status :should-start)
    (Button-Timer-Handler an-interactor))
  )

(defun Button-Int-Back-Inside-Action (an-interactor new-obj-over)
  (if-debug an-interactor
	    (format T "Button int-back-inside, obj= ~s~%" new-obj-over))
  (Button-Turn-On-Feedback an-interactor new-obj-over)
  (when (g-value an-interactor :timer-repeat-p)
    (s-value an-interactor :timer-wait-status :started)
    (Button-Timer-Handler an-interactor)
    ))

;;Turn off feedback
(defun Button-Int-Outside-Action (an-interactor prev-obj-over)
  (if-debug an-interactor (format T "Button int-outside, old = ~s~%" prev-obj-over))
  ;; *ignores :last  (unless (eq :last outside-control) ...)
  (let ((interim-sel-slot (interim-sel-slot an-interactor))
	(feedbackobj (g-value an-interactor :feedback-obj)))
    (when feedbackobj
      (when (eq prev-obj-over T) (error "new obj is T"))
      (dbprint-feed :obj-over feedbackobj NIL an-interactor)
      (s-value feedbackobj :obj-over NIL))
    (when (and interim-sel-slot prev-obj-over)
      (dbprint interim-sel-slot prev-obj-over NIL an-interactor)
      (s-value prev-obj-over interim-sel-slot NIL))
    (when (g-value an-interactor :timer-repeat-p)
      (Kill-Timer-Process an-interactor))))

(defun Button-Int-Stop-Action (an-interactor final-obj-over)
  (if-debug an-interactor (format T "Button int-stop over ~s~%" final-obj-over))
  (let ((feedbackobj (g-value an-interactor :feedback-obj))
	(how-set (g-value an-interactor :how-set))
	(main-agg (g-value an-interactor :main-aggregate))
	(interim-sel-slot (interim-sel-slot an-interactor))
	(agg-sel-slot (agg-sel-slot an-interactor))
	(obj-sel-slot (obj-sel-slot an-interactor)))
    (when feedbackobj
      (dbprint-feed :obj-over feedbackobj NIL an-interactor)
      (s-value feedbackobj :obj-over NIL))
    
    ;; always turn off interim-selected
    (when (and final-obj-over
	       (schema-p final-obj-over))
      (when interim-sel-slot
	(dbprint interim-sel-slot final-obj-over NIL an-interactor)
	(s-value final-obj-over interim-sel-slot NIL)))
    (if (g-value an-interactor :timer-repeat-p)
	(progn
	  ;; Don't do calc-set-obj-slot since done at beginning
	  ;; when using the timer.  But still have to do agg and
	  ;; final-obj-over.
	  (kill-timer-process an-interactor)
	  (s-value an-interactor :timer-wait-status :stopped))
	;; else set things normally
	(progn
	  (when (and final-obj-over
		     (schema-p final-obj-over))
	    (calc-set-obj-slot an-interactor
			 final-obj-over how-set
			 ;; old-object is the one that used to be selected,
			 ;; and get it from the aggregate, if any
			 (if (eq final-obj-over main-agg)
			     NIL
			     (g-value main-agg agg-sel-slot))))))
    ;; whether timer or not, do agg and final-feedback-obj here
    (if (eq :none final-obj-over)
	(Clear-All-Selected an-interactor main-agg)
	;; else handle the new object normally
	(when (and main-agg (schema-p main-agg))
	  (if (eq final-obj-over main-agg) ; if eq, then selected already set,
	      ;; but still need to do final-feedback-obj
	      (One-Final-Feedback-Obj an-interactor
			       (if (and obj-sel-slot
					(g-value final-obj-over obj-sel-slot))
				   final-obj-over NIL))
	      ;; else set the selected slot of the main-agg.  This procedure
	      ;; will also handle the final-feedback-obj
	      (Calc-set-agg-slot an-interactor main-agg final-obj-over
				 how-set))))
    (unless (g-value an-interactor :timer-repeat-p)
      (KR-Send an-interactor :final-function an-interactor final-obj-over))))

(defun Button-Int-Abort-Action (an-interactor final-obj-over)
  (if-debug an-interactor (format T "Button int-abort over ~s~%" final-obj-over))
  (when (and final-obj-over (not (eq final-obj-over :none)))
    (Button-Int-Outside-Action an-interactor final-obj-over)) ; turn off feedback
  (when (g-value an-interactor :timer-repeat-p)
    ;; Button-Int-Outside-Action will kill the process, but need to
    ;; reset the status variable
    (s-value an-interactor :timer-wait-status :stopped)))


;;; Go procedure utilities
;;

;; if continuous: (remove from start level, add to stop and abort
;; 		    levels, change state to running
;; 		    *ALSO* fix running where to be the object started over)
;; save object over, call start procedure.
(defun Button-do-start (an-interactor new-obj-over event)
  (declare (ignore event))
  (if-debug an-interactor (format T "Button starting over ~s~%" new-obj-over))
  (s-value an-interactor :main-aggregate
	   (get-gob-of-where (g-value an-interactor :start-where)))
  (s-value an-interactor :remembered-last-object new-obj-over)
  (Check-Start-Final-Feedback-Obj an-interactor)
  (if (g-value an-interactor :continuous)  ;then will go to running state
      (progn
	(Fix-Running-Where an-interactor new-obj-over)
	(GoToRunningState an-interactor T)
	(kr-send an-interactor :start-action an-interactor new-obj-over)
	)
      ;else call stop-action
      (progn
	(GoToStartState an-interactor NIL)
	(kr-send an-interactor :stop-action an-interactor new-obj-over)
	)))

;; remove from running level, put on start level, change state to
;; start, call abort procedure.    Become-inactive ignored because :active
;; set before this is called
(defun Button-do-abort (an-interactor become-inactive event)
  (declare (ignore event become-inactive))
  (if-debug an-interactor (format T "Button aborting~%"))
  (GoToStartState an-interactor T)
  (kr-send an-interactor :Abort-Action an-interactor
	       (if (eq (get-value an-interactor :current-state) :outside)
		   NIL			; pass NIL if now outside
		   (get-local-value an-interactor :remembered-last-object))))

(defun Button-do-outside (an-interactor)
  (if-debug an-interactor (format T "Button outside~%"))
  (s-value an-interactor :current-state :outside)
  (kr-send an-interactor :outside-action an-interactor
	       (g-value an-interactor :remembered-last-object)))

(defun Button-do-outside-stop (an-interactor event)
  (if-debug an-interactor (format T "Button stop outside~%"))
  (Button-do-abort an-interactor NIL event))

;; call back-inside procedure, change state to running
(defun Button-do-back-inside (an-interactor new-obj-over event)
  (declare (ignore event))
  (if-debug an-interactor (format T "Button back-inside over ~s~%" new-obj-over))
  (unless (eq new-obj-over
	      (get-local-value an-interactor :remembered-last-object))
    (setq new-obj-over
	     (get-local-value an-interactor :remembered-last-object)))
  (s-value an-interactor :current-state :running)
  (kr-send an-interactor :back-inside-action an-interactor
	       new-obj-over))

;; doesn't do anything
(defun Button-do-running (an-interactor new-obj-over event)
  #+garnet-debug (declare (ignore event))
  #-garnet-debug (declare (ignore an-interactor new-obj-over event))
  (if-debug an-interactor (format T "Button running over ~s~%" new-obj-over)))

;; Will be inside
;; Remove from running level, add to start level
;; unless :self-deactivate, change state to start, call stop procedure
(defun Button-do-stop (an-interactor new-obj-over event)
  (declare (ignore event))
  (if-debug an-interactor (format T "Button stop over ~s~%" new-obj-over))
  (unless (eq new-obj-over
	      (get-local-value an-interactor :remembered-last-object))
    (setq new-obj-over
	     (get-local-value an-interactor :remembered-last-object)))
  (GoToStartState an-interactor T)
  (kr-send an-interactor :Stop-Action an-interactor new-obj-over))

;; This is used if explicitly call Stop-Interactor.  It uses the last
;; selected object
(defun button-explicit-stop (an-interactor)
  (if-debug an-interactor (format T "Button explicit stop~%"))
  (let ((prev-obj-over (g-value an-interactor :remembered-last-object)))
    (GoToStartState an-interactor T)
    (kr-send an-interactor :Stop-Action an-interactor prev-obj-over)))

;;; Button schema
;;

(Create-Schema 'inter:button-interactor
	       (:is-a inter:interactor)
	       (:name :First-Button-interactor)
	       (:start-action 'Button-Int-Start-Action)
	       (:running-action 'Button-Int-Running-Action)
	       (:running-where '(:in *))
	       (:stop-action 'Button-Int-Stop-Action)
	       (:abort-action 'Button-Int-Abort-Action)
	       (:outside-action 'Button-Int-Outside-Action)
	       (:back-inside-action 'Button-Int-Back-Inside-Action)
	       (:how-set :list-toggle)
	       (:timer-repeat-p NIL)	  ; when T, then does timer
	       (:timer-initial-wait 0.75) ; seconds
	       (:timer-repeat-wait 0.05)  ; seconds
	       (:timer-handler 'Button-Timer-Handler)
	       (:slots-to-set '(:interim-selected :selected :selected))
					  ; slots: interim, in object, in aggregate
	       (:remembered-last-object NIL)
	       (:main-aggregate NIL)
	       (:Go 'General-Go)			    ; proc executed when events happen
	       (:Do-Start 'Button-Do-Start)		    ; these are
	       (:Do-Running 'Button-Do-Running)		    ; called by GO
	       (:Do-Explicit-Stop 'Button-Explicit-Stop)    ; for stop-interactor
	       (:Do-Stop 'Button-Do-Stop)		    ; to do the real work.
	       (:Do-Abort 'Button-Do-Abort)		    ; They call the
	       (:Do-Outside 'Button-Do-Outside)		    ; appropriate
	       (:Do-Back-Inside 'Button-Do-Back-Inside)	    ; -action procedures
	       (:Do-Outside-Stop 'Button-Do-Outside-Stop)   ; proc to call
	       (:initialize 'Button-Interactor-Initialize)) ; when created


;; Need special destroy to remove the extra final feedback objects that
;; may have been allocated
(define-method :destroy-me inter:button-interactor (an-interactor &optional (erase T))
  (if-debug an-interactor
	    (format T "Button special destroy ~s erase=~s~%" an-interactor erase))
  (when (g-value an-interactor :timer-repeat-p)
	(Kill-Timer-Process an-interactor))
  (Destroy-Extra-Final-Feedback-Objs an-interactor erase)
  (call-prototype-method an-interactor erase))
