;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-MODE; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file contains demo code for testing active and :list-element-of
;;; 
;;; ** Call (Do-Go) to start and (Do-Stop) to stop **
;;;
;;; Designed and implemented by Brad A. Myers

#|
============================================================
Change log:
         5/29/94 Marty Geier - Changed main window position in do-go
         7/17/92 Brad Myers - in garnet 2.0, you don't need to call
                              change-active
         8/31/89 Ed Pervin  -  Changed formulas to o-formulas, removed
                                defparameters, changed vp to win
         6/19/89 Brad Myers -  New return comment
         5/30/89 Brad Myers -  select-change -> Move-grow
	 4/13/89 Brad Myers - started
============================================================
|#


(in-package :DEMO-MODE)

(declaim (special WIN TOP-AGG OBJ RECT RECT1 RECT2 INTER1 INTER2 INTER3 INTER4
		  LINE-FEEDBACK TXT TXT2 FEEDBACK))
 
;;; ================================================================


(defun Do-Go (&key dont-enter-main-event-loop double-buffered-p)
  ;;; create a window
  (create-instance 'win inter:interactor-window (:left 150) (:top 40)
			 (:width 200) (:height 200)
                         (:double-buffered-p double-buffered-p)
 			 (:title "GARNET MODE") (:icon-title "Mode"))
  (s-value win :aggregate
	   (create-instance 'top-agg opal:aggregate
			    (:left 0) (:top 0) (:width 200) (:height 200)))
  
  (create-instance 'obj opal:text
		   (:string (o-formula (if (gvl :selected) "Move" "Select")))
		   (:left 10) (:top 100))

  (create-instance 'rect opal:rectangle
			     (:left (o-formula (gv obj :left)))
			     (:top (o-formula (gv obj :top)))
			     (:width (o-formula (gv obj :width)))
			     (:height (o-formula (gv obj :height))))

  (opal:add-components top-agg rect obj)

  (Create-Instance 'inter4 inter:Button-Interactor
	(:window win)
	(:continuous NIL)
	(:start-event #\t)
	(:start-where T)
	(:final-function
	 #'(lambda (interactor objUnderMouse)
	     (declare (ignore interactor objUnderMouse))
	     (s-value obj :selected (not (g-value obj :selected))))))

  (create-instance 'inter1 inter:button-interactor
				  (:start-where `(:in ,obj))
				  (:window win)
				  (:how-set :toggle))

 ; ----------------------------
  (create-instance 'line-feedback opal:rectangle
			   (:draw-function :xor)
			   (:left (o-formula (first (gvl :box))))
			   (:top (o-formula (second (gvl :box))))
			   (:width (o-formula (third (gvl :box))))
			   (:height (o-formula (fourth (gvl :box))))
			   (:visible (o-formula (gvl :obj-over)))
			   (:obj-over NIL)
			   (:box (list 0 0 0 0))
			   (:line-style opal:dashed-line)
			   (:filling-style NIL))

  (create-instance 'rect1 opal:rectangle
			   (:left (o-formula (first (gvl :box))))
			   (:top (o-formula (second (gvl :box))))
			   (:width (o-formula (third (gvl :box))))
			   (:height (o-formula (fourth (gvl :box))))
			   (:box (list 10 10 30 30))
			   (:name "Open rect")
			   (:line-style opal:line-2)
			   (:filling-style opal:white-fill))
  (create-instance 'rect2 opal:rectangle
			   (:left (o-formula (first (gvl :box))))
			   (:top (o-formula (second (gvl :box))))
			   (:width (o-formula (third (gvl :box))))
			   (:height (o-formula (fourth (gvl :box))))
			   (:box (list 50 10 30 30))
			   (:name "Grey rect")
			   (:line-style opal:line-1)
			   (:filling-style opal:light-gray-fill))
  (create-instance 'txt opal:text
			   (:string "text")
			   (:left (o-formula (first (gvl :box))))
			   (:name "Text string")
			   (:top (o-formula (second (gvl :box))))
			   (:box (list 90 10 30 30)))
  (create-instance 'txt2 opal:text
			   (:string (o-formula (gv top-agg :selected :name) "NONE"))
			   (:left 10)
			   (:top 150))
  (create-instance 'feedback opal:rectangle
			   (:draw-function :xor)
			   (:left (o-formula (gvl :obj-over :left)))
			   (:top (o-formula (gvl :obj-over :top)))
			   (:width (o-formula (gvl :obj-over :width)))
			   (:height (o-formula (gvl :obj-over :height)))
			   (:visible (o-formula (gvl :obj-over)))
			   (:obj-over NIL)
			   (:line-style NIL)
			   (:filling-style opal:black-fill))

  (opal:add-components top-agg txt2 rect1 rect2 txt feedback line-feedback)
 
  (s-value top-agg :lst3 (list rect1 rect2 txt))

  (Create-Instance 'inter2 inter:Move-Grow-Interactor
	(:window win)
	(:start-where `(:list-element-of ,top-agg :lst3))
	(:running-where T)
	(:feedback-obj line-feedback)
	(:attach-point :where-hit)
	(:grow-p NIL)
	(:active (o-formula (gv obj :selected))))

  (Create-Instance 'inter3 inter:Menu-Interactor
	(:window win)
	(:start-where `(:list-element-of ,top-agg :lst3 :type ,opal:rectangle))
	(:feedback-obj feedback)
	(:active (o-formula (not (gv obj :selected)))))



  (opal:update win)
  ;; return an explanatory string 
  (Format T "~%Demo-Mode:
  Use left button only.  When in *Select* mode the two rectangles can be selected.
  When in *Move* Mode, the two rects or the text string can be moved.
  Change modes by pressing on the Select/Move string or typing 't'.~%")
  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))
  )


(defun do-stop ()
  (opal:destroy win))

