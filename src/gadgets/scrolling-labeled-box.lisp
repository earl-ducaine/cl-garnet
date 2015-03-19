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
;;;
;;;  Scrolling-Labeled-Box   (scrollable string field in a box with a label)
;;;
;;;  Features and operation of the Scrolling-Labeled-Box:
;;;    ** Same as Scrolling-Input-String except that it has a box around
;;;        the string to be entered and there can be a label:
;;;
;;;    1)  The Scrolling-Labeled-Box allows left-right-scrollable text to be
;;; 		entered.
;;;    2)  Click the left mouse button on the string to edit it, and press
;;;        return to stop editing.
;;;    3)  The top level :value slot is the string currently appearing inside
;;;        the box.  This slot may be set directly and formulae may depend
;;;        on it.
;;;    4)  A function may be specified in the :selection-function slot to be
;;;        executed after the field text has changed (i.e., after the carriage
;;;        return).
;;;    5)  If the string gets to be too large to fit into the specified
;;;        Width, then the string inside is scrolled left and right so the
;;;        cursor is always visible
;;;    6)  Room is left on both sides of the string for a "..." symbol which
;;;        shows whether the string has been scrolled or not.  Therefore, the
;;;        string will not appear exactly at the :left or extend the full
;;;        :width (since room is left for the ...'s).
;;;
;;;  Customizable slots:
;;;    1)  Left, top
;;;    2)  Width - The width of the entire area in pixels.  THIS MUST BE
;;; 			BIG ENOUGH for the label and at least a few
;;; 			characters of the string
;;;    3)  Value -- The string that will originally appear in the box and that
;;;                 will be changed
;;;    4)  Selection-Function -- Function to be executed after editing text
;;;    5)  Field-font -- The font of the string **MUST BE FIXED WIDTH ***
;;;    6)  Label-font -- The font of the string beside the box
;;;    7)  Label-offset -- The distance between the label and the box
;;;    8)  Field-offset -- The distance between the field text and the box
;;;    9)  Label-string -- The string that will appear beside the box
;;;
;;;  Scrolling-Labeled-Box demo:
;;;    This module contains a function which creates a window and a
;;;    Scrolling-Labeled-Box in the window.  To run it, enter
;;;    (GARNET-GADGETS:Scrolling-Labeled-Box-go).
;;;    To stop, enter (GARNET-GADGETS:Scrolling-Labeled-Box-stop).
;;;
;;;  Designed and written by Brad Myers
#|
============================================================
Change log:
        02/23/93  Andrew Mickish - Added :string-set-fun
        12/14/92  Andrew Mickish - Added type and parameter declarations
        11/25/92  Andrew Mickish - Added :active-p
        04/30/92  Andrew Mickish - Called get-standard-font for fonts
         2/17/92  Andrew Mickish - Added :maybe-constant list
	 1/28/92  Ed Pervin - Must wrap ,# around lambda for CMUCL.
         7/29/91  Andrew Mickish - Changed some formulas to center the
                  label, field, and frame with each other
	 6/01/90  Brad Myers - created
============================================================
|#

(in-package "GARNET-GADGETS")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(Scrolling-Labeled-Box Scrolling-Labeled-Box-Win
	    Scrolling-Labeled-Box-Go Scrolling-Labeled-Box-Stop
	    Scrolling-Labeled-Box-Obj)))

(create-instance 'Scrolling-Labeled-Box opal:aggregadget
   :declare ((:parameters :left :top :width :label-string :value :label-offset
			  :field-offset :label-font :field-font :active-p
			  :selection-function :visible)
	     (:type (integer :label-offset :field-offset)
		    (string :label-string :value)
		    ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
		     :field-font :label-font)
		    (kr-boolean :active-p)
		    ((or null function symbol) :selection-function))
	     (:maybe-constant :left :top :width :label-offset :field-offset
			      :label-string :field-font :label-font :active-p
			      :visible))
   ; Customizable slots
   (:left 0) (:top 0)
   (:width 130)
   (:label-offset 5)
   (:field-offset 2)
   (:label-string "Label:")
   (:value "Field")
   (:selection-function NIL)
   (:field-font opal:default-font) ;;**Must be fixed width**
   (:label-font (opal:get-standard-font NIL :bold NIL))
   (:active-p T)

   ; Generally non-customizable slots

   ;; For field-height assume field-font is fixed-height, so any string
   ;; will do (don't use (gvl value) since it will change a lot and this
   ;; slot will be recomputed unnecessarily)
   (:field-height (o-formula (opal:string-height (gvl :field-font) "X")))
   (:label-height (o-formula (opal:string-height (gvl :label-font) "X")))
   (:frame-height (o-formula (MAX (+ 4 (gvl :field-height))
				  (gvl :label-height))))
   (:frame-left (o-formula (+ (gvl :left)
			      (gvl :label-text :width)
			      (gvl :label-offset))))
   (:frame-width (o-formula (- (gvl :width)
			       (gvl :label-text :width)
			       (gvl :label-offset))))
   (:field-width (o-formula (- (gvl :frame-width)
			       (* 2 (gvl :field-offset)))))
   (:field-left (o-formula (+ (gvl :frame-left) (gvl :field-offset))))
   (:height (o-formula (gvl :frame-height)))
   (:parts
    `((:LABEL-TEXT ,opal:text
       (:constant (:actual-heightp))
       (:left ,(o-formula (gvl :parent :left)))
       (:top ,(o-formula (let ((p (kr-path 0 :parent)))
			   (+ (gv p :top)
			      (floor (- (gv p :frame-height)
					(gvl :height)) 2)))))
       (:string ,(o-formula (gvl :parent :label-string)))
       (:font ,(o-formula (gvl :parent :label-font)))
       (:line-style ,(o-formula (if (gvl :parent :active-p)
				    opal:default-line-style
				    opal:gray-line))))
      (:FRAME ,opal:rectangle
       (:left ,(o-formula (gvl :parent :frame-left)))
       (:top ,(o-formula (gvl :parent :top)))
       (:width ,(o-formula (gvl :parent :frame-width)))
       (:height ,(o-formula (gvl :parent :frame-height)))
       (:line-style ,(o-formula (if (gvl :parent :active-p)
				    opal:default-line-style
				    opal:gray-line))))
      (:FIELD-TEXT ,garnet-gadgets:scrolling-input-string
       (:left ,(o-formula (gvl :parent :field-left)))
       (:top ,(o-formula (let ((p (kr-path 0 :parent)))
			   (+ (gv p :top)
			      (floor (- (gv p :frame-height)
					(gvl :height)) 2)))))
       (:width ,(o-formula (gvl :parent :field-width)))
       (:value ,(o-formula (gvl :parent :value)))
       (:font ,(o-formula (gvl :parent :field-font)))
       (:active-p ,(o-formula (gvl :parent :active-p)))
       (:selection-function
	,#'(lambda (obj final-value)
	     (let ((top-obj (g-value obj :parent)))
	       (s-value top-obj :value final-value)
	       (kr-send top-obj :selection-function top-obj
			final-value))))))))

(define-method :string-set-func SCROLLING-LABELED-BOX
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

#+garnet-test (defparameter Scrolling-Labeled-Box-win NIL)
#+garnet-test (defparameter Scrolling-Labeled-Box-top-agg NIL)
#+garnet-test (defparameter Scrolling-Labeled-Box-Obj NIL)

#+garnet-test
(defun Scrolling-Labeled-Box-Go ()

  (create-instance 'Scrolling-Labeled-Box-win inter:interactor-window
     (:height 100)(:width 350)(:top 15)(:left 640)(:title "Scrolling Box"))

  (s-value Scrolling-Labeled-Box-win
	   :aggregate
	   (create-instance 'Scrolling-Labeled-Box-top-agg opal:aggregate))

  (create-instance 'Scrolling-Labeled-Box-obj Scrolling-Labeled-Box
     (:left 50) (:top 50)
     (:selection-function #'(lambda(obj value)
			     (format T "Final string for ~s is ~s~%" obj value))))
  (opal:add-components Scrolling-Labeled-Box-top-agg Scrolling-Labeled-Box-Obj)

  (opal:update Scrolling-Labeled-Box-win))

#+garnet-test
(defun Scrolling-Labeled-Box-Stop ()
  (opal:destroy Scrolling-Labeled-Box-win))
