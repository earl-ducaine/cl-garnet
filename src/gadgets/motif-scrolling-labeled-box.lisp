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
;;;  Motif-Scrolling-Labeled-Box   (scrollable string field in a box)
;;;
;;;  Features and operation of the Motif-Scrolling-Labeled-Box:
;;;
;;;    1)  The Motif-Scrolling-Labeled-Box allows left-right-scrollable text
;;;        to be entered.
;;;    2)  Click the left mouse button on the string to edit it, and press
;;;        return to stop editing.
;;;    3)  The top level :value slot is the string currently appearing inside
;;;        the box.  This slot may be set directly and formulas may depend
;;;        on it.
;;;    4)  A function may be specified in the :selection-function slot to be
;;;        executed after the field text has changed (i.e., after the carriage
;;;        return).
;;;    5)  If the string gets to be too large to fit into the specified
;;;        Width, then the string inside is scrolled left and right so the
;;;        cursor is always visible
;;;
;;;  Customizable slots:
;;;    1)  Left, top,
;;;    2)  Width - The width of the entire area in pixels.  THIS MUST BE
;;; 			BIG ENOUGH for at least a few characters of the string
;;;    3)  Field-offset -- The distance between the field text and the box
;;;    4)  Label-offset
;;;    5)  Label-string -- The text to appear beside the box
;;;    6)  Field-font -- The font of the field **MUST BE FIXED WIDTH ***
;;;    7)  Label-font -- The font for the label string
;;;    8)  Foreground-Color
;;;    9)  Keyboard-selection-p -- Whether to put a selection box around the
;;;                                entire box
;;;   10)  Active-p -- When NIL, text will appear "grayed-out"
;;;   11)  Value -- The string that will originally appear in the box and that
;;;                 will be changed
;;;   12)  Selection-function -- Function to be executed when return is
;;;                              pressed.  Parameters are the
;;;                              top-level GADGET and the VALUE.
;;;
;;;  Motif-Scrolling-Labeled-Box demo:
;;;    This module contains a function which creates a window and a
;;;    Motif-Scrolling-Labeled-Box in the window.  To run it, enter
;;;    (GARNET-GADGETS:Motif-Scrolling-Labeled-Box-go).
;;;    To stop, enter (GARNET-GADGETS:Motif-Scrolling-Labeled-Box-stop).
;;;
;;;  Written by Andrew Mickish
;;;  Adapted from Scrolling-Labeled-Box


;;;  CHANGE LOG:
;;;
;;;  04/19/93 Andrew Mickish - Moved :field-stippled-line-style to motif-parts
;;;  02/23/93 Andrew Mickish - Added :string-set-func
;;;  12/15/92 Andrew Mickish - Added type and parameter declarations
;;;  11/30/92 Andrew Mickish - Moved Insert-Text-Into-Box to s-i-s
;;;  11/25/92 Andrew Mickish - Moved code for :active-p into s-i-s
;;;  04/30/92 Andrew Mickish - Called get-standard-font for fonts
;;;  03/20/92 Ed Pervin - Eliminate #\control-tab completely.
;;;  02/11/92 Andrew Mickish - Added :maybe-constant list
;;;  10/09/91 Andrew Mickish - Added Insert-Text-Into-Box
;;;  10/07/91 Andrew Mickish - Added :fast-redraw-p slots
;;;  08/02/91 Andrew Mickish - Changed (:field-text :parts :string :width)
;;;             to be :max-width, according to change in scr-input-string
;;;  04/17/91 Andrew Mickish - Changed tab interactor to be an instance of
;;;             MOTIF-TAB-INTER
;;;  04/08/91 Edward Pervin - #\Control-Tab does not exist in KCL.
;;;  03/01/91 Andrew Mickish - Created
;;;


(in-package :GARNET-GADGETS)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Insert-Text-Into-Box Motif-Scrolling-Labeled-Box Motif-Scrolling-Unlabeled-Box))
  #+garnet-test
  (export '(Motif-Scrolling-Labeled-Box-Go Motif-Scrolling-Labeled-Box-Stop
	    Motif-Scrolling-Labeled-Box-Win Motif-Scrolling-Labeled-Box-Top-Agg
	    Demo-Motif-Scrolling-Labeled-Box)))

(create-instance 'MOTIF-SCROLLING-LABELED-BOX MOTIF-GADGET-PROTOTYPE
  :declare ((:parameters :left :top :width :field-offset :label-offset
			 :label-string :value :field-font :label-font
			 :foreground-color :keyboard-selection-p :active-p
			 :selection-function :visible)
	    (:type (integer :field-offset :label-offset)
		   (string :label-string :value)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
		    :field-font :label-font)
		   ((is-a-p opal:color) :foreground-color)
		   (kr-boolean :keyboard-selection-p :active-p)
		   ((or null function symbol) :selection-function))
	    (:maybe-constant :left :top :width :field-offset :label-offset
			     :label-string :field-font :label-font
			     :foreground-color :active-p :visible))
   ; Customizable slots
   (:left 0) (:top 0)
   (:width 135)
   (:field-offset 4)
   (:label-offset 5)
   (:label-string "Label:")
   (:value "Field")
   (:field-font opal:default-font) ;;**Must be fixed width**
   (:label-font (opal:get-standard-font NIL :bold NIL))
   (:foreground-color opal:MOTIF-GRAY)
   (:keyboard-selection-p NIL)
   (:keyboard-selection-obj (o-formula (gv :self)))
   (:selection-function NIL)
   (:active-p T)

   ; Generally non-customizable slots

   ;; For text-height assume field-font is fixed-height, so any string
   ;; will do (don't use (gvl value) since it will change a lot and this
   ;; slot will be recomputed unnecessarily)
   (:field-height (o-formula (opal:string-height (gvl :field-font) "X")))
   (:label-height (o-formula (opal:string-height (gvl :label-font) "X")))
   (:frame-left (o-formula (+ 2 (gvl :left)
			      (if (gvl :label-string)
				  (+ (gvl :label-text :width)
				     (gvl :label-offset)) 0))))
   (:frame-top (o-formula (- (gvl :center-y)
			     (floor (gvl :frame-height) 2))))
   (:frame-width (o-formula (- (gvl :width) 4
			       (if (gvl :label-string)
				   (+ (gvl :label-text :width)
				      (gvl :label-offset)) 0))))
   (:frame-height (o-formula (+ 4 (gvl :field-height))))
   (:field-left (o-formula (+ (gvl :frame-left) (gvl :field-offset))))
   (:field-width (o-formula (- (gvl :frame-width) (* 2 (gvl :field-offset)))))
   (:height (o-formula (+ (MAX (gvl :frame-height) (gvl :label-height)) 4)))
   (:center-y (o-formula (+ 2 (gvl :top)
			    (floor (MAX (gvl :frame-height)
					(gvl :label-height)) 2))))
   (:parts
    `((:LABEL-TEXT ,opal:text
                   (:constant (:actual-heightp))
                   (:left ,(o-formula (+ 2 (gvl :parent :left))))
                   (:top ,(o-formula (- (gvl :parent :center-y)
					(floor (gvl :height) 2))))
                   (:string ,(o-formula (gvl :parent :label-string)))
                   (:font ,(o-formula (gvl :parent :label-font)))
                   (:line-style ,(o-formula
				  (let ((p (kr-path 0 :parent)))
				    (if (gv p :active-p)
					opal:default-line-style
					(gv p :stippled-line-style))))))
      (:FRAME ,MOTIF-BOX
             (:constant (:depressed-p))
	     (:left ,(o-formula (gvl :parent :frame-left)))
	     (:top ,(o-formula (gvl :parent :frame-top)))
	     (:width ,(o-formula (gvl :parent :frame-width)))
	     (:height ,(o-formula (gvl :parent :frame-height)))
             (:depressed-p T))
      (:FIELD-TEXT ,garnet-gadgets:SCROLLING-INPUT-STRING
		    (:left ,(o-formula (gvl :parent :field-left)))
		    (:top ,(o-formula (+ 2 (gvl :parent :frame-top))))
		    (:width ,(o-formula (gvl :parent :field-width)))
		    (:value ,(o-formula (gvl :parent :value)))
		    (:font ,(o-formula (gvl :parent :field-font)))
                    (:active-p ,(o-formula (gvl :parent :active-p)))
                    (:line-style ,(o-formula
				   (let ((p (kr-path 0 :parent)))
				     (if (gv p :active-p)
					 opal:default-line-style
					 (gv p :field-stippled-line-style)))))
                    ;; Take out the dots
                    (:parts
		     ((:string :modify
			       (:left ,(o-formula (gvl :parent :left)))
			       (:max-width ,(o-formula (gvl :parent :width)))
			       (:fast-redraw-p :rectangle)
			       (:fast-redraw-filling-style
				,(o-formula (gvl :parent :parent :background-fill))))))

                    (:selection-function
		     ,#'(lambda (obj final-value)
			  (let ((top-obj (g-value obj :parent)))
			    (s-value top-obj :value final-value)
			    (kr-send top-obj :selection-function top-obj
				     final-value)))))
      (:SEL-BOX ,MOTIF-SELECTION-BOX))))


;;;  Change the key translation table of the interactor to make tab do nothing
;;;  instead of beep.
(inter:bind-key #\tab
		#'(lambda (inter obj ev) (declare (ignore inter obj ev)) NIL)
		(g-value MOTIF-SCROLLING-LABELED-BOX :field-text :text-edit))

(inter:bind-key :CONTROL-tab
		#'(lambda (inter obj ev) (declare (ignore inter obj ev)) NIL)
		(g-value MOTIF-SCROLLING-LABELED-BOX :field-text :text-edit))


(define-method :string-set-func MOTIF-SCROLLING-LABELED-BOX
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


#+garnet-test
(defun Motif-Scrolling-Labeled-Box-Go (&key dont-enter-main-event-loop
					    not-double-buffered-p)
  (create-instance 'Motif-Scrolling-Labeled-Box-Win inter:interactor-window
     (:double-buffered-p (not not-double-buffered-p))
     (:title "Motif Scrolling Labeled Box")
     (:left 650) (:top 10)
     (:width 235) (:height 100))
  (s-value Motif-Scrolling-Labeled-Box-Win
	   :aggregate
	   (create-instance 'Motif-Scrolling-Labeled-Box-Top-Agg opal:aggregate))
  (create-instance 'Demo-Motif-Scrolling-Labeled-Box Motif-Scrolling-Labeled-Box
     (:left 50) (:top 40)
     (:selection-function #'(lambda (obj value)
			      (format T "Final string for ~s is ~s~%"
				      obj value))))
  (opal:add-components Motif-Scrolling-Labeled-Box-Top-Agg
		       (create-instance NIL MOTIF-BACKGROUND)
		       Demo-Motif-Scrolling-Labeled-Box)
  (opal:update Motif-Scrolling-Labeled-Box-Win)
  (create-instance 'DEMO-MOTIF-SCROLLING-LABELED-BOX-INTER MOTIF-TAB-INTER
     (:window MOTIF-SCROLLING-LABELED-BOX-WIN)
     (:objects (list DEMO-MOTIF-SCROLLING-LABELED-BOX)))
  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))
  )

#+garnet-test
(defun Motif-Scrolling-Labeled-Box-Stop ()
  (opal:destroy Motif-Scrolling-Labeled-Box-win))



;;; Motif-Scrolling-Unlabeled-Box
;;

(create-instance 'MOTIF-SCROLLING-UNLABELED-BOX MOTIF-GADGET-PROTOTYPE
  :declare ((:parameters :left :top :width :field-offset
			 :value :field-font :foreground-color
			 :keyboard-selection-p :active-p
			 :selection-function :visible)
	    (:type (Fixnum :left :top :width :field-offset
			   :field-width :field-height :field-left
			   :frame-top :frame-width :frame-height :height :center-y)
		   (string :value)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
		    :field-font)
		   ((is-a-p opal:color) :foreground-color)
		   (kr-boolean :keyboard-selection-p :active-p)
		   ((or null function symbol) :selection-function))
	    (:maybe-constant :left :top :width :field-offset
			     :field-font :foreground-color :active-p :visible))
   ; Customizable slots
   (:left 0) (:top 0)
   (:width 135)
   (:field-offset 4)
   (:value "Field")
   (:field-font opal:default-font) ;;**Must be fixed width**
   (:foreground-color opal:MOTIF-GRAY)
   (:keyboard-selection-p NIL)
   (:keyboard-selection-obj (o-formula (gv :self)))
   (:selection-function NIL)
   (:active-p T)

   ; Generally non-customizable slots

   ;; For text-height assume field-font is fixed-height, so any string
   ;; will do (don't use (gvl value) since it will change a lot and this
   ;; slot will be recomputed unnecessarily)
   (:field-height (o-formula (opal:string-height (gvl :field-font) "X")))
   (:frame-left (o-formula (+ 2 (gvl-fixnum :left))))
   (:frame-top (o-formula (- (gvl-fixnum :center-y)
			     (floor (gvl-fixnum :frame-height) 2))))
   (:frame-width (o-formula (- (gvl-fixnum :width) 4)))
   (:frame-height (o-formula (+ 4 (gvl-fixnum :field-height))))
   (:field-left (o-formula (+ (gvl-fixnum :frame-left) (gvl-fixnum :field-offset))))
   (:field-width (o-formula (- (gvl-fixnum :frame-width) (* 2 (gvl-fixnum :field-offset)))))
   (:height (o-formula (+ (gvl-fixnum :frame-height) 4)))
   (:center-y (o-formula (+ 2 (gvl-fixnum :top) (floor (gvl-fixnum :frame-height) 2))))
   (:parts
    `((:FRAME ,MOTIF-BOX
             (:constant (:depressed-p))
	     (:left ,(o-formula (gvl :parent :frame-left)))
	     (:top ,(o-formula (gvl :parent :frame-top)))
	     (:width ,(o-formula (gvl :parent :frame-width)))
	     (:height ,(o-formula (gvl :parent :frame-height)))
             (:depressed-p T))
      (:FIELD-TEXT ,garnet-gadgets:SCROLLING-INPUT-STRING
		    (:left ,(o-formula (gvl :parent :field-left)))
		    (:top ,(o-formula (+ 2 (gvl-fixnum :parent :frame-top))))
		    (:width ,(o-formula (gvl :parent :field-width)))
		    (:value ,(o-formula (gvl :parent :value)))
		    (:font ,(o-formula (gvl :parent :field-font)))
                    (:active-p ,(o-formula (gvl :parent :active-p)))
                    (:line-style ,(o-formula
				   (let ((p (kr-path 0 :parent)))
				     (if (gv p :active-p)
					 opal:default-line-style
					 (gv p :field-stippled-line-style)))))
                    ;; Take out the dots
                    (:parts
		     ((:string :modify
			       (:left ,(o-formula (gvl :parent :left)))
			       (:max-width ,(o-formula (gvl :parent :width)))
			       (:fast-redraw-p :rectangle)
			       (:fast-redraw-filling-style
				,(o-formula (gvl :parent :parent :background-fill))))))

                    (:selection-function
		     ,#'(lambda (obj final-value)
			  (let ((top-obj (g-value obj :parent)))
			    (s-value top-obj :value final-value)
			    (kr-send top-obj :selection-function top-obj
				     final-value)))))
      (:SEL-BOX ,MOTIF-SELECTION-BOX))))
