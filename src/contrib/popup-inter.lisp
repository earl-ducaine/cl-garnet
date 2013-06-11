;;; -*- Mode: lisp; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : popupinter.lisp
;;;; Author          : Frank Ritter
;;;; Created On      : Mon Jan  7 18:36:18 1991
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Thu Apr 11 19:29:00 1991
;;;; Update Count    : 23
;;;; 
;;;; PURPOSE
;;;; This file contains an interactor to popup menus.
;;;; It should be loaded after Interactors.lisp and movegrow.lisp.
;;;;
;;;; TABLE OF CONTENTS
;;;;	i.	minor inits
;;;; 	ii.	Main Default Procedures to go into the slots
;;;;	iii.	go procedures
;;;; 	I.	popup-interactor schema
;;;; 
;;;; (C) Copyright 1990, Frank Ritter, all rights reserved.
;;;; The material in this file is made available according to the
;;;; terms of the GNU LGPL, modified by the Lisp LGPL preamble.  Both
;;;; of those documents are available in the doc subdirectory of the
;;;; Garnet distribution.

;;;; 21Jun04 - per agreement with Frank Ritter, the license terms of
;;;; this file are clarified to be LLGPL. [2004/06/21:rpg]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Designed and implemented by Brad A. Myers & Frank Ritter
#|


============================================================
Change log:
        [2004/06/16:rpg] Modified in-package for ANSI CL
        1/17/91  Frank Ritter - wrote a popup-interactor.
        ...
	8/8/88 Brad Myers - analogous code started
============================================================
|#

;;;
;;;	i.	minor inits
;;;

(in-package "INTERACTORS")
(export '(popup-interactor))
(proclaim '(special popup-Interactor))
;; requires move-grow interactors


;;;
;;; 	ii.	Main Default Procedures to go into the slots
;;;============================================================
;;;

(defun popup-Int-Start-Action (an-interactor object-being-changed
						  first-points)
  (if-debug an-interactor 
     (format T "Popup int-start moving ~s firstpoints=~s~%"
             object-being-changed first-points))
  )

(defun popup-Int-Running-Action (an-interactor object-being-changed
						    new-points)
  (if-debug an-interactor 
     (format T "Popup int-running, obj = ~s, popups=~s~%"
	      object-being-changed new-points))
  )

(defun popup-Int-Back-Inside-Action 
       (an-interactor outside-control object-being-changed new-inside-points)
  (declare (ignore outside-control))
  (if-debug an-interactor 
	    (format T "Popup int-back-in, obj = ~s, new popups=~s~%"
		    object-being-changed new-inside-points))
  )

(defun popup-Int-Stop-Action (an-interactor object-being-changed)
  (if-debug an-interactor (format T "Popup int-stop obj ~s "
				  object-being-changed ))
  ;;turn off feedback
  (KR-Send an-interactor :final-function an-interactor object-being-changed))

(defun popup-Int-Abort-Action (an-interactor object-being-changed)
  (if-debug an-interactor (format T "Popup int-abort moving ~s~%"
				  object-being-changed))
  )
  
(defun popup-Interactor-Initialize (new-Move-Grow-schema)
  (if-debug new-Move-Grow-schema (format T "Select change initialize ~s~%"
					 new-Move-Grow-schema))
  (Check-Interactor-Type new-Move-Grow-schema inter:popup-Interactor)
  (Check-Required-Slots new-Move-Grow-schema)
  (Set-Up-Defaults new-Move-Grow-schema)  )


;;;
;;;	iii.	go procedures
;;;============================================================
;;; Go procedure utilities
;;;============================================================

;;; if continuous: (remove from start level, add to stop and abort
;;; 		    level, change state to running)
;;; save object over, call start procedure.
(defun popup-do-start (an-interactor new-obj-over event)
  (if-debug an-interactor (format T "Popup starting over ~s~%" new-obj-over))
  ;; note where you came from for later use
  (s-value an-interactor :x (event-x event))
  (s-value an-interactor :y (event-y event))
  (if (g-value an-interactor :continuous)  ;then will go to running state
      (progn
        (GoToRunningState an-interactor T)
        (kr-send an-interactor :start-action an-interactor new-obj-over))
    ;else call stop-action
    (progn
      (kr-send an-interactor :stop-action an-interactor new-obj-over)
      (GoToStartState an-interactor NIL)))
 )


(defun popup-do-back-inside (an-interactor obj event)
  (if-debug an-interactor (format T "Popup back-inside over ~s at:~s~%"
				  obj event))
  )

(defun popup-do-running (an-interactor obj event)
  (if-debug an-interactor (format T "Popup running over ~s at:~s~%" obj event))
  )

(defun popup-do-stop (an-interactor obj event)
  (if-debug an-interactor (format T "Point stop over ~s at:~s~%" obj event))
)


;;;
;;; 	I.	popup schema
;;;============================================================

(Create-Schema 'inter:Popup-Interactor
     (:is-a inter:interactor)
     (:name :First-Move-Grow-interactor)
     (:start-action 'popup-Int-Start-Action)
     (:running-action 'popup-Int-Running-Action)
     (:stop-action 'popup-Int-Stop-Action)
     (:abort-action 'popup-Int-Abort-Action)
     (:outside-action 'popup-Int-Outside-Action)
     (:back-inside-action 'popup-Int-Back-Inside-Action)
     (:obj-to-change NIL)  ;supplied by application program
     ;; this slot seems unused...
     (:attach-popup :where-hit) ; where attach to object
     (:x-off 0) ; needed for :where-hit.  Offset from where
     (:y-off 0)    ;    hit to top left of object
     (:saved-original-points NIL) ; used for ABORT or outside
     (:saved-last-points NIL) ; used if stop and outside and
                              ; outside control is :last
     (:Go 'General-Go)  ; proc executed when events happen
     (:Do-Start 'popup-Do-Start)     ; these are
     (:Do-Running 'popup-Do-Running) ;   called by GO
     (:Do-Stop 'popup-Do-Stop)       ;   to do
     (:Do-Abort 'Move-Grow-Do-Abort)     ;   the real work.
     (:Do-Outside 'Move-Grow-Do-Outside) ;   They call the
     (:Do-Back-Inside 'popup-Do-Back-Inside)  ; appropriate
     (:Do-Outside-Stop 'Move-Grow-Do-Outside-Stop); -action
                                                  ; procedures
     (:initialize 'popup-Interactor-Initialize))
