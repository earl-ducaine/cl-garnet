;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-GROW; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id$	


;;; This file contains demo code for showing things that change size in a window.
;;;
;;; This is intended as a test and demonstration of the move-grow
;;; interactor and the Garnet-Gadgets graphics-selection object
;;; as part of the Garnet project.
;;;
;;; ** Call (demo-move:Do-Go) to start and (demo-move:Do-Stop) to stop **
;;;
;;; Designed and implemented by Brad A. Myers


(in-package :DEMO-GROW)

;;; Load GAD-button-parts unless already loaded
;;;
(unless (get :garnet-modules :graphics-selection)
  (common-lisp-user::garnet-load (concatenate 'string "gadgets:"
                                  "graphics-loader"))) 
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eliminate compiler warnings
(declaim (special WIN TOP-AGG SEL-OBJS-AGG MYSELECTION))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start test programs

(defun create-rects (num win agg)
  (let ((w (g-value win :width))
	(h (g-value win :height))
	width height obj)
    (dotimes (i num)
      (setq width (+ 25 (random (floor w 4))))
      (setq height (+ 25 (random (floor h 4))))
      (setq obj (create-instance NIL opal:rectangle
				 (:box (list (random (the integer (- w width)))
					     (random (the integer (- h height)))
					     width height))
				 (:left (o-formula (first (gvl :box))))
				 (:top (o-formula (second (gvl :box))))
				 (:width (o-formula (third (gvl :box))))
				 (:height (o-formula (fourth (gvl :box))))
				 (:line-p NIL)
				 (:filling-style
				  (case i
				    (0 opal:white-fill)
				    (1 opal:light-gray-fill)
				    (2 opal:gray-fill)
				    (3 opal:dark-gray-fill)
				    (t opal:no-fill)))))
      (opal:add-component agg obj))))


(defun create-lines (num win agg)
  (let ((w (g-value win :width))
	(h (g-value win :height))
	obj)
    (dotimes (i num)
      (setq obj (create-instance NIL opal:line
				 (:points
				  (list (random (the integer w))
					(random (the integer h))
					(random (the integer w))
					(random (the integer h))))
				 (:x1 (o-formula (first (gvl :points))))
				 (:y1 (o-formula (second (gvl :points))))
				 (:x2 (o-formula (third (gvl :points))))
				 (:y2 (o-formula (fourth (gvl :points))))
				 (:line-p T)
				 (:line-style
				  (case i
				    (0 opal:line-0)
				    (1 opal:line-2)
				    (2 opal:dotted-line)
				    (3 opal:line-4)
				    (t opal:dashed-line)))))
      (opal:add-component agg obj))))

(defun do-go (&key dont-enter-main-event-loop double-buffered-p)
  (create-instance 'win inter:interactor-window
		   (:double-buffered-p double-buffered-p)
		   (:left 130) (:top 100)
		   (:width 500) (:height 300)(:title "GARNET Move-Grow")
		   (:icon-title "Move-Grow")
		   (:aggregate
		    (create-instance 'top-agg Opal:aggregate)))
  (create-instance 'sel-objs-agg Opal:Aggregate)

  ;; If we get clobbered by the window manager, let the demos
  ;; controller know (if it's there).
  (when (fboundp 'common-lisp-user::Garnet-Note-Quitted)
    (pushnew
     #'(lambda (win)
	 (declare (ignore win))
	 (common-lisp-user::Garnet-Note-Quitted "DEMO-GROW"))
     (g-value win :destroy-hooks)))
		   
  (Opal:Add-Component top-agg sel-objs-agg)

  (create-rects 4 win sel-objs-agg)
  (create-lines 4 win sel-objs-agg)

  (create-instance 'myselection Garnet-Gadgets:Graphics-Selection
		   (:start-where (list :element-of-or-none sel-objs-agg))
		   (:selection-function
		    #'(lambda (toolkitobject newselection)
			(declare (ignore toolkitobject newselection))))
		   (:modify-function
		    #'(lambda (toolkitobject selectedobject new-points)
			(declare (ignore toolkitobject selectedobject new-points)))))

  (Opal:Add-Component top-agg myselection)
  (Opal:Update win)

  ;;; Some extra interactors

  (Create-Instance 'inter3 inter:Move-Grow-Interactor
	(:window win)
	(:continuous T)
	(:start-event :middledown)
	(:start-where `(:element-of ,sel-objs-agg))
	(:running-where `(:in-box ,sel-objs-agg))
	(:outside NIL) ; goes back to original position if go outside
	(:feedback-obj NIL)
	(:attach-point :where-hit)
	(:line-p (o-formula (gvl :first-obj-over :line-p)))
	(:grow-p NIL)
	(:start-action
	 #'(lambda (interactor objbeingchanged newsize)
	    ;;; turn off selection first
	     (s-value myselection :value NIL)
	     (Call-Prototype-Method interactor objbeingchanged newsize)
	     )))

  (Create-Instance 'inter4 inter:Move-Grow-Interactor
	(:window win)
	(:continuous T)
	(:start-event :rightdown)
	(:start-where `(:element-of ,sel-objs-agg))
	(:running-where `(:in-box ,sel-objs-agg))
	(:outside NIL) ; goes back to original position if go outside
	(:feedback-obj NIL)
	(:attach-point :where-hit)
	(:min-height 30)
	(:min-width 30)
	(:grow-p T)
	(:line-p (o-formula (gvl :first-obj-over :line-p)))
	(:start-action
	 #'(lambda (interactor objbeingchanged newsize)
	    ;;; turn off selection first
	    (s-value myselection :value NIL)
	    (Call-Prototype-Method interactor objbeingchanged newsize)
	    )))

  ;; ** Do-Go **
  (Format T "~%Demo-Grow: 
  Press on an object with the left button to cause it to be selected.
  Then, press on a white selection box with the left button to move the
     object and a black box to change the object's size.  
  Press on an object with the middle button to MOVE it from where you press.  
  Press on an object with the right button to GROW it from where you press.
  Press on the background (where there are no objects) with the left button
     to cause there to be no objects selected.~%")

  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))

  )


(defun Do-stop ()
  (Opal:Destroy win))


