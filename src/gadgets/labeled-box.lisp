;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;; 02/23/93 Andrew Mickish - Added :string-set-func
;;; 12/14/92 Andrew Mickish - Added type and parameter declarations
;;; 04/30/92 Andrew Mickish - Called get-standard-font for fonts
;;; 01/28/92 Ed Pervin - Had to wrap (function ...) around lambda for CMUCL.
;;; 01/10/92 Andrew Mickish - Added constant slots
;;; 07/04/90 Andrew Mickish - Added :label-height and :field-height slots
;;;            to reduce the evaluation of the top-level :height formula
;;; 06/25/90 Andrew Mickish - Changed :top slots of LABEL-TEXT and FRAME
;;;            to center according to maximum height
;;; 06/20/90 Andrew Mickish - Removed :text-height slot; now :frame-height
;;;            only depends on the height of the field-text.

(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Labeled-Box))
  #+garnet-test
  (export '(Labeled-Box-Go Labeled-Box-Stop
	    Labeled-Box-Win Labeled-Box-Top-Agg Labeled-Box-Obj)))

(create-instance 'LABELED-BOX opal:aggregadget
   :declare ((:parameters :left :top :min-frame-width :label-offset
			  :field-offset :label-string :value :field-font
			  :label-font :selection-function :visible)
	     (:type (integer :label-offset :field-offset)
		    ((integer 0) :min-frame-width)
		    (string :label-string :value)
		    ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
		     :field-font :label-font)
		    ((or null function symbol) :selection-function))
	     (:maybe-constant :left :top :label-offset :field-offset
			      :min-frame-width :label-string :field-font
			      :label-font :visible))
   ; Customizable slots
   (:left 0) (:top 0)
   (:label-offset 5)
   (:field-offset 6)
   (:min-frame-width 10)
   (:label-string "Label:")
   (:value "Field")
   (:selection-function NIL)
   (:field-font opal:default-font)
   (:label-font (opal:get-standard-font NIL :bold NIL))
   ; Generally non-customizable slots
   (:field-height (o-formula (opal:string-height (gvl :field-font) "X")))
   (:label-height (o-formula (opal:string-height (gvl :label-font) "X")))
   (:frame-left (o-formula (+ (gvl :left)
			      (gvl :label-text :width)
			      (gvl :label-offset))))
   (:frame-width (o-formula (max (+ (* 2 (gvl :field-offset))
				    (gvl :field-text :width))
				 (gvl :min-frame-width))))
   (:frame-height (o-formula (+ 4 (gvl :field-height))))
   (:height (o-formula (MAX (gvl :frame-height)
			    (gvl :label-height))))
   (:width (o-formula (+ (gvl :frame-width) (gvl :label-offset)
			 (gvl :label-text :width))))
   (:center-y (o-formula (+ (gvl :top) (floor (gvl :height) 2))))
   (:parts
    `((:LABEL-TEXT ,opal:text
                  (:constant (:actual-heightp))
		  (:left ,(o-formula (gvl :parent :left)))
		  (:top ,(o-formula (- (gvl :parent :center-y)
				       (floor (gvl :height) 2))))
		  (:string ,(o-formula (gvl :parent :label-string)))
		  (:font ,(o-formula (gvl :parent :label-font))))
      (:FIELD-TEXT ,opal:cursor-text
                  (:constant (:actual-heightp))
		  (:left ,(o-formula (+ (gvl :parent :frame-left)
					(gvl :parent :field-offset))))
		  (:top ,(o-formula (+ 2 (gvl :parent :frame :top))))
		  (:string ,(o-formula (let ((value (gvl :parent :value)))
					 (if value value ""))))
		  (:font ,(o-formula (gvl :parent :field-font))))
      (:FRAME ,opal:rectangle
	     (:left ,(o-formula (gvl :parent :frame-left)))
	     (:top ,(o-formula (- (gvl :parent :center-y)
				  (floor (gvl :height) 2))))
	     (:width ,(o-formula (gvl :parent :frame-width)))
	     (:height ,(o-formula (gvl :parent :frame-height))))))
   (:interactors
    `((:TEXT-INTER ,inter:text-interactor
		   (:window ,(o-formula (gv-local :self :operates-on :window)))
		   (:start-where ,(o-formula (list :in-box (gvl :operates-on
								:frame))))
		   (:start-event :leftdown)
		   (:stop-event #\RETURN)
		   (:obj-to-change ,(o-formula (gvl :operates-on :field-text)))
		   (:final-function
		    ,#'(lambda (interactor obj event final-string x y)
			 (declare (ignore obj event x y))
			 (s-value (g-value interactor :operates-on)
				  :value
				  final-string)
			 ;; Execute global :selection-function
			 (kr-send (g-value interactor :operates-on)
				  :selection-function
				  (g-value interactor :operates-on)
				  final-string)))))))

(define-method :string-set-func LABELED-BOX
    (gadget-obj str-obj final-event final-string)
  (declare (ignore final-event))
  (if (eq str-obj (g-value gadget-obj :label-text))
      ; then is label (title)
      (opal::set-one-value gadget-obj :label-string final-string)
      ; else return NIL
      NIL))

;;;
;;;  DEMO FUNCTION
;;;

#+garnet-test (defparameter Labeled-Box-win NIL)
#+garnet-test (defparameter Labeled-Box-top-agg NIL)
#+garnet-test (defparameter Labeled-Box-Obj NIL)

#+garnet-test
(defun Labeled-Box-Go ()
  (create-instance 'labeled-box-win inter:interactor-window
     (:height 100)(:width 350)(:top 5)(:left 650))
  (s-value Labeled-Box-win
	   :aggregate
	   (create-instance 'labeled-box-top-agg opal:aggregate
			    (:overlapping NIL)))
  (create-instance 'labeled-box-obj Labeled-Box
     (:left 50) (:top 40))
  (opal:add-components Labeled-Box-top-agg Labeled-Box-Obj)
  (opal:update Labeled-Box-win))

#+garnet-test
(defun Labeled-Box-Stop ()
  (opal:destroy Labeled-Box-win))
