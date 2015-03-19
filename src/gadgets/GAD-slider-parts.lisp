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
;;;  GAD-slider-parts
;;;
;;;  This module is a collection of schema definitions required by the trill
;;;  device and all sliders.
;;;
;;;  Written by Andrew Mickish

;;;  CHANGE LOG:
;;;  08/12/93  Rajan Parthasarathy - Checked for "" in read-from-string
;;;  11/22/91  Andrew Mickish - Made :left and :top formulas of VALUE-TEXT
;;;            more efficient.
;;;  01/18/90  Andrew Mickish - Changed :active slot of VALUE-INTER to consider
;;;            :scroll-p of top-level object
;;;  02/24/90  Andrew Mickish - Removed :visible slots from VALUE-RECT and
;;;            VALUE-TEXT
;;;  03/01/90  Andrew Mickish - Removed :draw-function slot from VALUE-TEXT
;;;  06/16/90  Andrew Mickish - Condensed SET-POSITION
;;;  07/01/90  Andrew Mickish - Removed :cursor-index from VALUE-TEXT and put
;;;            distinct values in instances.
;;;  11/30/90  Pavan Reddy - changed :string slot of VALUE-TEXT to support
;;;            floating-point numbers.

(in-package "GARNET-GADGETS")


;;;  Set-Position is used to update the top level :value slot when the
;;;  position of the slider has been changed by direct editing of the
;;;  value text.
;;;
(defun SET-POSITION (interactor cur-text-obj stop-event)
  (declare (ignore cur-text-obj stop-event))

  ; Turn off cursor
  (s-value (g-value (g-value interactor :operates-on) :value-text)
	   :cursor-index
	   NIL)

  ; Check to see that the string is convertable to a number.
  (let* ((parent (g-value interactor :operates-on :parent))
	 (string (g-value interactor :operates-on :value-text :string))
	 (symbol (unless (equal string "")
		   (read-from-string string)))
	 (number (when (numberp symbol) symbol)))

    ; If the string was a valid number, then set the :value slot to the
    ; value just calculated and execute the :selection-function.
    ; Else, mark the current :value as changed in order to cause
    ; the old value to be redisplayed and sound the buzzer.
    (if number
	(let* ((val-1 (g-value parent :val-1))
	       (val-2 (g-value parent :val-2))
	       (new-value (cond ((and val-1 val-2)
				 (inter:Clip-and-Map number val-1 val-2))
				((and val-1 (not val-2))
				 (if (> number val-1)
				     (if (integerp val-1) (round number) number)
				     val-1))
				((and (not val-1) val-2)
				 (if (< number val-2)
				     (if (integerp val-2) (round number) number)
				     val-2))
				(t number))))
	  (s-value parent :value new-value)
	  (kr-send parent :selection-function parent new-value))
	(inter::beep))
    (mark-as-changed parent :value)))

;;;
;;;  OBJECTS USED TO REPORT SLIDER POSITION
;;;

(create-instance 'VALUE-RECT opal:rectangle
   (:left (o-formula (gv (kr-path 0 :parent) :left)))
   (:top (o-formula (gv (kr-path 0 :parent) :top)))
   (:width (o-formula (gv (kr-path 0 :parent) :width)))
   (:height (o-formula (gv (kr-path 0 :parent) :height))))
      

(create-instance 'VALUE-TEXT opal:cursor-text
   (:string (o-formula (let ((p (kr-path 0 :parent :parent)))
			 (format NIL (gv p :format-string) (gv p :value)))))
   (:left (o-formula (+ (gv-fixnum (kr-path 0 :parent) :left)
			(floor (- (gv-fixnum (kr-path 0 :parent) :width)
				  (gvl-fixnum :width)) 2))))
   (:top (o-formula (+ (gv-fixnum (kr-path 0 :parent) :top)
		       (floor (- (gv-fixnum (kr-path 0 :parent) :height)
				 (gvl-fixnum :height)) 2))))
   (:font (o-formula (gv (kr-path 0 :parent) :font))))


(create-instance 'VALUE-INTER inter:Text-Interactor
   (:window (o-formula (gv-local :self :operates-on :window)))
   (:start-where (o-formula (list :in-box (gvl :operates-on :value-rect))))
   (:start-event :leftdown)
   (:stop-event #\RETURN)
   (:feedback-obj (o-formula (gvl :operates-on :value-text)))
   (:active (o-formula (and (gvl :operates-on :visible)
			    (gvl :operates-on :parent :scroll-p))))
   (:stop-action #'SET-POSITION))


;;  Tell the world that GAD-slider-parts has been loaded
;;
(setf (get :garnet-modules :GAD-slider-parts) T)

