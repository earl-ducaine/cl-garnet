;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;;___________________________________________________________________
;;; The Garnet User Interface Development Environment
;;; Copyright (c) 1989, 1990 Carnegie Mellon University
;;; All rights reserved.  The CMU software License Agreement specifies
;;; the terms and conditions for use and redistribution.
;;;
;;; If you want to use this code or anything developed as part of the
;;; Garnet project, please contact Brad Myers (Brad.Myers@CS.CMU.EDU).
;;;___________________________________________________________________
;;;
;;;  Labeled Box
;;;
;;;  Features and operation of the labeled box:
;;;    1)  The labeled-box object is a box with text inside and beside the box.
;;;    2)  Click the left mouse button on the framed text to edit it, and press
;;;        return to stop editing.
;;;    3)  The top level :value slot is the string currently appearing inside
;;;        the box.  This slot may be set directly and formulae may depend
;;;        on it.
;;;    4)  A function may be specified in the :selection-function slot to be
;;;        executed after the field text has changed (i.e., after the carriage
;;;        return).
;;;
;;;  Customizable slots:
;;;    1)  Left, top
;;;    2)  Label-offset -- The distance between the label and the box
;;;    3)  Field-offset -- The distance between the field text and the box
;;;    4)  Min-frame-width -- The minimum width of the frame around the text
;;;    5)  Label-string -- The string that will appear beside the box
;;;    6)  Value -- The string that will originally appear in the box and that
;;;                 will be changed
;;;    7)  Selection-Function -- Function to be executed after editing text
;;;    8)  Label-font -- The font of the string beside the box
;;;    9)  Field-font -- The font of the string inside the box
;;;
;;;  Labeled box demo:
;;;    This module contains a function which creates a window and a labeled box
;;;    in the window.  To run it, enter (GARNET-GADGETS:labeled-box-go).
;;;    To stop, enter (GARNET-GADGETS:labeled-box-stop).
;;;
;;;  Designed by Brad Myers
;;;  Written by Andrew Mickish

;;; Change log
;;;
;;;  2/27/91 moved field to be beneath the prompt, allowed user to type
;;;          CR to take default -fer
;;; 06/20/90 Andrew Mickish - Removed :text-height slot; now :frame-height
;;;            only depends on the height of the field-text.
;;; 06/25/90 Andrew Mickish - Changed :top slots of LABEL-TEXT and FRAME
;;;            to center according to maximum height
;;; 07/04/90 Andrew Mickish - Added :label-height and :field-height slots
;;;            to reduce the evaluation of the top-level :height formula

(in-package "GARNET-GADGETS" :use '("LISP" "KR"))

(export '(Labeled-Box query-with-labeled-box
	  Labeled-Box-Go Labeled-Box-Stop Labeled-Box-Obj))


;; swipped whole hog from error-gadget
(defun ADD-label-PRIORITY-LEVEL ()
  (unless (and (boundp 'label-PRIORITY-LEVEL)
	       (member LABEL-PRIORITY-LEVEL inter:priority-level-list))
    (push (create-instance 'LABEL-PRIORITY-LEVEL inter:priority-level)
	  inter:priority-level-list)))
(add-label-priority-level)


(defun query-with-labeled-box (box-gadget title string)
  "popup an labeled-box window."
  (if (g-value box-gadget :modal-p)
      (s-value label-PRIORITY-LEVEL :stop-when :always)
      (s-value label-PRIORITY-LEVEL :stop-when :if-any))
  ;; set up the box
  (s-value box-gadget :label-string title)
  (s-value box-gadget :value string)
  ;; Turn visibility on
  (let ((window (g-value box-gadget :window)))
    (s-value window :left (g-value window :left))
    (s-value window :top (g-value window :top))     ; Won't size correctly
    (s-value window :width (g-value window :width))   ;  without these lines
    (s-value window :height (g-value window :height))
    (s-value window :visible T)
    (opal:update window))
  (inter:beep)
    ;; make sure it gets clicked if really-modal-p
  (if (g-value box-gadget :really-modal-p)
      (prog ((sleep-time (g-value box-gadget :sleep-time))
	     (display (let ((win1 #-(and)(caar (opal::get-table-contents)) #+(and)(car opal::*garnet-windows*)))
	                (if win1
 			    (xlib:window-display win1)
                            opal::*default-x-display*))) )
	    start
	    ;; call the event handler to get anything
            (opal::default-event-handler display :timeout 0)
            (sleep sleep-time)
	    (if (g-value box-gadget :window :visible)
		(go start))
	    (return (g-value box-gadget :value))
	    ))
  )


(create-instance 'LABELED-BOX opal:aggregadget

   ; Customizable slots
   (:really-modal-p nil) 
   ;;for really-modal-p, time in sec to sleep between checking if done
   (:sleep-time 0.1)
   (:window-width (o-formula (+ 20 (gvl :width))))    ; 10 on each side
   (:window-height (o-formula (+ 40 (gvl :height))))  ; 20 on top, bottom
   
   (:left 0) (:top 0)
   (:label-offset 5)  ;now vertical displacement
   (:field-offset 6)
   (:min-frame-width 10)
   (:label-string "Label:")
   (:value "Field")
   (:selection-function NIL)
   (:field-font opal:default-font)
   (:label-font (create-instance NIL opal:font
		    (:face :bold)))

   ; Generally non-customizable slots
   (:field-height (o-formula (opal:string-height (gvl :field-font) "X")))
   (:label-height (o-formula (opal:string-height (gvl :label-font) "X")))

   (:frame-left (o-formula (- (gvl :left) 2)))
   (:frame-width (o-formula (max (+ (* 2 (gvl :field-offset))
				    (gvl :field-text :width))
				 (gvl :min-frame-width))))
   (:frame-height (o-formula (+ 4 (gvl :field-height))))
   (:width (o-formula (MAX (gvl :frame-width)
                           (gvl :label-text :width))))
   (:height (o-formula (+ (gvl :frame-height) (gvl :label-offset)
			  (gvl :label-text :height))))
   (:center-y (o-formula (+ (gvl :top) (floor (gvl :height) 2))))
   (:parts
    `((:LABEL-TEXT ,opal:text
		  (:left ,(o-formula (gvl :parent :left)))
		  (:top ,(o-formula (- (gvl :parent :center-y)
				       (floor (gvl :height) 2))))
		  (:string ,(o-formula (gvl :parent :label-string)))
		  (:font ,(o-formula (gvl :parent :label-font))))
      (:FIELD-TEXT ,opal:cursor-text
       		  (:left ,(o-formula (gvl :parent :left)))
		  (:top ,(o-formula (+ 2 (gvl :parent :label-text :top)
				       (gvl :parent :label-text :height))))
		  (:string ,(o-formula (let ((value (gvl :parent :value)))
					 (if value value ""))))
		  (:font ,(o-formula (gvl :parent :field-font))))
      (:FRAME ,opal:rectangle
	     (:left ,(o-formula (gvl :parent :frame-left)))
	     (:top ,(o-formula (- (gvl :parent :field-text :top)
				  2)))
	     (:width ,(o-formula (gvl :parent :frame-width)))
	     (:height ,(o-formula (gvl :parent :frame-height))))))
   (:interactors
    `((:fast-ok-inter ,inter:button-interactor
           (:start-where t)
           (:waiting-priority ,label-PRIORITY-LEVEL)
	   (:running-priority ,label-PRIORITY-LEVEL)       
	   (:window ,(o-formula (gv-local :self :operates-on :window)))
           (:start-event #\RETURN)
	   (:final-function
	    ,#'(lambda (interactor obj)
		 (declare (ignore obj))
		 ;; make it invisible if really-modal-p
		 (if (g-value interactor :operates-on :really-modal-p)
		     (s-value (g-value interactor :operates-on :window)
			      :visible nil))
		 ;; return preset value
		 (g-value interactor :operates-on :value)))
           (:continuous nil))
      (:TEXT-INTER ,inter:text-interactor
		   (:window ,(o-formula (gv-local :self :operates-on :window)))
		   (:start-where ,(o-formula (list :in-box (gvl :operates-on
								:frame))))
		   (:start-event :leftdown)
                   (:waiting-priority ,label-PRIORITY-LEVEL)
		   (:active T)  ;?
		   (:running-priority ,label-PRIORITY-LEVEL)
		   (:stop-event #\RETURN)
		   (:obj-to-change ,(o-formula (gvl :operates-on :field-text)))
		   (:final-function
		    ,#'(lambda (interactor obj event final-string x y)
			 (declare (ignore obj event x y))
			 (s-value (g-value interactor :operates-on)
				  :value
				  final-string)
			 ;; make it invisible if really-modal-p
			 (if (g-value interactor :operates-on :really-modal-p)
			     (s-value (g-value interactor :operates-on :window)
				      :visible nil))
			 ;; Execute global :selection-function
			 (kr-send (g-value interactor :operates-on)
				  :selection-function
				  (g-value interactor :operates-on)
				  final-string)))))))

;;;
;;;  DEMO FUNCTION
;;;

#+garnet-test
(defparameter Labeled-Box-win NIL)
#+garnet-test
(defparameter Labeled-Box-top-agg NIL)
#+garnet-test
(defparameter Labeled-Box-Obj NIL)

#+garnet-test
(defun Labeled-Box-Go ()

  (create-instance 'labeled-box-win inter:interactor-window
     (:height 360)(:width 350)(:top 5)(:left 650))
  (s-value Labeled-Box-win
	   :aggregate
	   (create-instance 'labeled-box-top-agg opal:aggregate
			    (:overlapping NIL)))

  (create-instance 'labeled-box-obj Labeled-Box
     (:left 50) (:top 50))
  (opal:add-components Labeled-Box-top-agg Labeled-Box-Obj)

  (opal:update Labeled-Box-win))


#+garnet-test
(defun Labeled-Box-Stop ()
  (opal:destroy Labeled-Box-win))

