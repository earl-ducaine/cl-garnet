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
;;; This was hacked from the scrolling labeled box code to produce an
;;; unlabeled box.  Its sort of obvious, but I use it for the prompter
;;; stuff.  --Russell Almond (almond@statsci.com)
;;;
;;;  Scrolling-unLabeled-Box   (scrollable string field in a box
;;;  without the label)
;;;
;;;  Features and operation of the Scrolling-unLabeled-Box:
;;;    ** Same as Scrolling-Input-String except that it has a box around
;;;        the string to be entered and there is no label:
;;;
;;;    1)  The Scrolling-unLabeled-Box allows left-right-scrollable text to be
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
;;;  Scrolling-unLabeled-Box demo:
;;;    This module contains a function which creates a window and a
;;;    Scrolling-Input-String in the window.  To run it, enter
;;;    (GARNET-GADGETS:Scrolling-unLabeled-Box-go).
;;;    To stop, enter (GARNET-GADGETS:Scrolling-unLabeled-Box-stop).
;;;
;;;  Designed and written by Brad Myers
#|
============================================================
Change log:
;;; 10/2/03 RGA --- Moved to Garnet Gadgets.
         6/19/92 --RGA hacked from scrolling-labeled-box to remove
           label. 
        04/30/92 - Andrew Micksih - Called get-standard-font for fonts
         2/17/92  Andrew Mickish - Added :maybe-constant list
	 1/28/92  Ed Pervin - Must wrap ,# around lambda for CMUCL.
         7/29/91  Andrew Mickish - Changed some formulas to center the
                  label, field, and frame with each other
	 6/01/90  Brad Myers - created
============================================================
|#

;;(common-lisp-user::garnet-load "gadgets:scrolling-labeled-box-loader")

(in-package "GARNET-GADGETS")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(Scrolling-Unlabeled-Box))
  #+garnet-test
  (export '(Scrolling-Unlabeled-Box-Go Scrolling-Unlabeled-Box-Stop
	    Scrolling-Unlabeled-Box-Obj)))


(kr:create-instance 'Scrolling-Unlabeled-Box opal:aggregadget
  :declare
  ((:parameters :left :top :width :field-offset
		:field-font :value :visible)
   (:output :height :value)
   (:type (Fixnum :field-offset :field-height
		   :field-width :frame-height
		   :frame-left :frame-width
		   :field-left)
	  (Font :field-font)
	  (String :value))
   (:maybe-constant :left :top :width :field-offset
		    :field-font :visible))
   #+kr-doc
   (:documentation "A field for inputing text surrounded by a frame
with no label.")
   #+kr-doc
   (:slot-doc :left "Horizontal Position of Frame"
	      :top  "Vertical Position of Frame"
	      :width "Width of Frame"
	      :height "Vertical Extent of Frame (Read Only)"
	      :field-offset "Space between frame and text"
	      :field-font "Font for entering text; must be fixed width"
	      :value "Text returned (or default value)."
	      :visible "Draw this item?")
   ;; Customizable slots
   (:left 0) (:top 0)
   (:width 130)
   (:field-offset 2)
   (:value "Field")
   (:selection-function NIL)
   (:field-font opal:default-font)	; **Must be fixed width**

   ;; Generally non-customizable slots

   ;; For field-height assume field-font is fixed-height, so any string
   ;; will do (don't use (gvl value) since it will change a lot and this
   ;; slot will be recomputed unnecessarily)
   (:field-height (o-formula (opal:string-height (gvl :field-font) "X")))
   (:frame-height (o-formula (+ 4 (gvl :field-height))))
   (:frame-left (o-formula (gvl :left)))
   (:frame-width (o-formula (gvl :width)))
   (:field-width (o-formula (- (gvl :frame-width)
			       (* 2 (gvl :field-offset)))))
   (:field-left (o-formula (+ (gvl :frame-left) (gvl :field-offset))))
   (:height (o-formula (gvl :frame-height)))
   (:parts
    `((:FRAME ,opal:rectangle
	     (:left ,(o-formula (gvl :parent :frame-left)))
	     (:top ,(o-formula (gvl :parent :top)))
	     (:width ,(o-formula (gvl :parent :frame-width)))
	     (:height ,(o-formula (gvl :parent :frame-height))))
      (:FIELD-TEXT ,garnet-gadgets:scrolling-input-string
		    (:left ,(o-formula (gvl :parent :field-left)))
		    (:top ,(o-formula (let ((p (kr-path 0 :parent)))
					(+ (gv p :top)
					   (floor (- (gv p :frame-height)
						     (gvl :height)) 2)))))
		    (:width ,(o-formula (gvl :parent :field-width)))
		    (:value ,(o-formula (gvl :parent :value)))
		    (:font ,(o-formula (gvl :parent :field-font)))
		    (:selection-function
		     ,#'(lambda (obj final-value)
			  (let ((top-obj (g-value obj :parent)))
			    (s-value top-obj :value final-value)
			    (kr-send top-obj :selection-function top-obj
				     final-value))))))))


;;;  DEMO FUNCTION
;;

#+:garnet-test
(defparameter Scrolling-Unlabeled-Box-win NIL)
#+:garnet-test
(defparameter Scrolling-Unlabeled-Box-top-agg NIL)
#+:garnet-test
(defparameter Scrolling-Unlabeled-Box-Obj NIL)

#+:garnet-test
(defun Scrolling-Unlabeled-Box-Go ()

  (create-instance 'Scrolling-Unlabeled-Box-win inter:interactor-window
     (:height 100)(:width 350)(:top 15)(:left 640)(:title "Scrolling Box"))

  (s-value Scrolling-Unlabeled-Box-win
	   :aggregate
	   (create-instance 'Scrolling-Unlabeled-Box-top-agg opal:aggregate))

  (create-instance 'Scrolling-Unlabeled-Box-obj Scrolling-Unlabeled-Box
     (:left 50) (:top 50)
     (:selection-function #'(lambda(obj value)
			     (format T "Final string for ~s is ~s~%" obj value))))
  (opal:add-components Scrolling-Unlabeled-Box-top-agg Scrolling-Unlabeled-Box-Obj)

  (opal:update Scrolling-Unlabeled-Box-win))

#+:garnet-test
(defun Scrolling-Unlabeled-Box-Stop ()
  (opal:destroy Scrolling-Unlabeled-Box-win))

