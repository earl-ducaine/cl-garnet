;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -*- Mode: Lisp; Package: GARNET-GADGETS -*-
;;;
;;; This file contains code for handling the percent and offset
;;; buttons in the width and height constraint menus
;;;
;;; l-constraint-offset-action
;;; t-constraint-offset-action: allows the designer to input an absolute
;;;                             amount that should separate the left or
;;;                             tops of two objects
;;;
;;; l-constraint-percent-action
;;; t-constraint-percent-action: allows the designer to input a percentage
;;;                              amount that controls how one object is
;;;                              centered beneath another
;;;
;;; 
;;; offset-action
;;; w-constraint-offset-action
;;; h-constraint-offset-action: allows the designer to input an absolute
;;;                             amount by which the widths or heights of
;;;                             two objects should differ
;;;
;;; percent-action
;;; w-constraint-percent-action
;;; h-constraint-percent-action: allows the designer to input a percentage
;;;                              amount by which the widths or heights of
;;;                              two objects should differ
;;;
;;; apply-box-constraint-p: indicates if it is valid to apply a constraint
;;;
;;; deselect-constraint-button: deselects a constraint button
;;;
;;; Created by Brad Vander Zanden
;;;

#|
============================================================
Change log:
7/14/93  Andrew Mickish - Declared *Box-Constraint-Menu* special in
           L/T-Constraint-Offset-Action functions;  Ignored slot parameter
           in Offset-Action and Scale-Action
6/24/89  Brad Vander Zanden - created
============================================================
|#

(in-package "GARNET-GADGETS")


;;;============================================================
;;;
;;; determine whether the offset or scale of a constraint
;;; should be changed
;;;============================================================

(defun set-offset-p (slot)
  (declare (special *constraint-gadget*))
  (let ((p-selection (g-value *constraint-gadget* :obj-to-constrain))
	(s-selection (g-value *constraint-gadget* :obj-to-reference)))
    (or (and p-selection (null s-selection))
	(and p-selection s-selection
	     (depends-on-p p-selection s-selection slot)))))

;;;============================================================
;;;
;;; the next two functions allow the designer to input an absolute
;;; amount that should separate the left or tops of two objects.
;;; it insures that the designer inputs a number, and if
;;; there is a constraint between the selected objects,
;;; it incorporates this amount into the constraint.
;;;
;;;============================================================

(defun l-constraint-offset-action (interactor offset)
  (declare (special *box-constraint-menu*))
  (let* ((selected-constraint (g-value *box-constraint-menu* :box-agg 
				   :left-button-panel :buttons
				   :selected)))
    (offset-action interactor offset :left-offset :left 
		   (set-offset-p :left)
		 (or  (not selected-constraint)
		      (not (eq (g-value selected-constraint :known-as) 
			       :center))))))

(defun t-constraint-offset-action (interactor offset)
  (declare (special *box-constraint-menu*))
  (let* ((selected-constraint (g-value *box-constraint-menu* :box-agg 
				   :top-button-panel :buttons
				   :selected)))
  (offset-action interactor offset :top-offset :top 
		 (set-offset-p :top)
		 (or  (not selected-constraint)
		      (not (eq (g-value selected-constraint :known-as) 
			  :center))))))


;;;============================================================
;;;
;;; the next three functions allow the designer to input an absolute
;;; amount by which the width or height of two objects should differ.
;;; it insures that the designer inputs a number, and if
;;; there is a constraint between the selected objects,
;;; it incorporates this amount into the constraint.
;;;
;;;============================================================

(defun w-constraint-difference-action (interactor offset)
  (offset-action interactor offset :width-difference :width
		 (set-offset-p :width)))

(defun h-constraint-difference-action (interactor offset)
  (offset-action interactor offset :height-difference :height
		 (set-offset-p :height)))

;;;============================================================
;;; set-constraint-offset-p indicates whether the offset should
;;; be placed in the appropriate offset slot of the primary
;;; selection (if there is one). offset-p indicates whether
;;; the offset string should be treated as an offset or a
;;; percent
;;;============================================================

(defun offset-action (gadget offset-string offset-slot slot 
			     set-constraint-offset-p &optional (offset-p t))
  (declare (special *box-constraint-menu*)
	   (ignore slot))

  ;; insure that the input is an integer
  (when (not (valid-integer-p gadget offset-string))
	(return-from offset-action))

  (let ((offset (read-from-string offset-string)))

    ;; record the new offset in the appropriate place

    (if offset-p
	(setf offset (round offset))
	(setf offset (/ offset 100)))

    (s-value *box-constraint-menu* offset-slot offset)
    
    ;; if the offset should be placed in the appropriate offset slot
    ;; of the primary selection, do so

    (when set-constraint-offset-p
      (let ((p-selected (g-value *constraint-gadget* :obj-to-constrain)))
	(s-value p-selected offset-slot offset)))))

;;;============================================================
;;;
;;; the next three functions allow the designer to input a percentage
;;; amount by which the width or height of two objects should differ.
;;; it insures that the designer inputs a number, and if
;;; there is a constraint between the selected objects,
;;; it incorporates this amount into the constraint.
;;;
;;;============================================================

(defun w-constraint-scale-action (interactor scale)
  (declare (special *box-constraint-menu*))
  (scale-action interactor scale :width-scale :width
		(set-offset-p :width)))

(defun h-constraint-scale-action (interactor scale)
  (declare (special *box-constraint-menu*))
  (scale-action interactor scale :height-scale :height 
		(set-offset-p :height)))

(defun valid-number-p (gadget value)
  (let ((number (when (not (string= value "")) (read-from-string value))))
    (if (not (numberp number))
	(progn
	  (s-value gadget :value (g-value gadget :old-value))
	  (constraint-gadget-error "the value must be a number")
	  nil)
	(s-value gadget :old-value value))))

;;;============================================================
;;; set-constraint-scale-p indicates whether the scale should
;;; be placed in the appropriate scale slot of the primary
;;; selection (if there is one)
;;;============================================================

(defun scale-action (gadget scale-string scale-slot slot 
			    set-constraint-scale-p)
  (declare (special *box-constraint-menu*)
	   (ignore slot))

  ;; insure that the input is an integer
  (when (not (valid-number-p gadget scale-string))
	(return-from scale-action))

  (let ((scale (read-from-string scale-string)))

    ;; save the scale factor in the appropriate menu
    (s-value *box-constraint-menu* scale-slot scale)

    ;; if the scale should be placed in the appropriate scale slot
    ;; of the primary selection, do so

    (when set-constraint-scale-p
      (let ((p-selected (g-value *constraint-gadget* :obj-to-constrain)))
	(s-value p-selected scale-slot scale)))))

;;;============================================================
;;; the following five functions set the value of the 
;;; appropriate slot in the primary selection
;;;============================================================

(defun set-bbox-slot (gadget string slot)
  (declare (special *box-constraint-menu*))

  ;; insure that the input is an integer
  (when (not (valid-integer-p gadget string))
	(return-from set-bbox-slot))

  (let ((value (read-from-string string)))

    (when (and (g-value *constraint-gadget* :obj-to-constrain)
	       (null (g-value *constraint-gadget* :obj-to-reference)))
      (let ((obj (g-value *constraint-gadget* :obj-to-constrain)))

	;; certain slots should not be altered. if this slot is
	;; one of them, tell the user and do not proceed
	(when (member slot (g-value obj :do-not-alter-slots))
	      (constraint-gadget-error
	       (format nil "cannot change ~S's ~S slot" obj slot))
	      (return-from set-bbox-slot))

	(cg-destroy-constraint obj slot)
	(s-value obj slot value)))))

(defun set-left (inter string)
  (set-bbox-slot inter string :left))

(defun set-top (inter string)
  (set-bbox-slot inter string :top))

(defun set-width (inter string)
  (set-bbox-slot inter string :width))

(defun set-height (inter string)
  (set-bbox-slot inter string :height))

;; deselect a constraint button by setting the :selected slot of the
;; button in obj's :selected slot to nil

(defun deselect-constraint-button (obj)
  (let ((selected-obj (g-value obj :selected)))
    (when selected-obj
	  (when (schema-p selected-obj)
		(s-value selected-obj :selected nil))
	  (s-value obj :selected nil))))

