;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GILT; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file originally created by Gilt, then hacked to pieces
;;
;;; Must be loaded after line-prop.lisp and color-prop, because it uses some
;;; functions from those files.

#|
============================================================
Change log:
      3/4/93 Brad Myers - made work with multiple objects
    10/22/92 Andrew Mickish - Line-or-Fill-New-Style takes gq-type parameter;
               fixed Fill-Prop-OK to handle NIL filling-styles
     8/24/92 Brad Myers - make more robust when bad "other": fill-prop-ok
     4/13/92 Brad Vander Zanden - Added opal:white-fill check in conditional
             in Check-Colors-For-Fill.
     2/18/92 Brad Myers - put (constant T) in generated colors
     2/27/91 Brad Myers - added Motif colors
     2/20/91 Osamu Hashimoto - separated it from particular gadgets
     1/23/91 Andrew Mickish added '(or (null style) ..)' to NEW-COLOR
             definition in Show-Fill-Props-Dialog
     1/13/91 Brad Myers - fixed bug where color rectangle isn't being reset
    11/30/90 Brad Myers - made to work with Gilt
    11/15/90 Osamu Hashimoto - created
============================================================
|#

(in-package "GILT")

(declaim (special FILL-PROP))

;;; This is called when there is a new filling style, so the final filling
;;; style rectangle can be changed
(defun New-Filling-Style (&rest args)
  (declare (ignore args))
  (Line-or-Fill-New-Style fill-prop :filling-style :other-filling-style
		       (g-value fill-prop :fill-boxes)
		       opal:white-fill '(:stipple :fill-style)
		       opal:filling-style))

;;; This is called as the OK function when a new color is picked.  It changes
;;; the color slot and rectangles.
(defun Fill-Color-OK (new-color)
  (s-value fill-prop :new-color new-color)
  (use-color (g-value fill-prop :current-color) new-color)
  ; if the filling-style was :none or :white, change it to be black, since
  ; usually wouldn't change the color and not want to use it
  (let ((cur-fill (g-value fill-prop :fill-boxes :selected)))
    (when (or (eq cur-fill (g-value fill-prop :fill-boxes :NON-FILL-BOX))
	      (eq cur-fill (g-value fill-prop :fill-boxes :WHITE-FILL-BOX)))
      (Inter:SelectObj (g-value fill-prop :press)
		       (g-value fill-prop :fill-boxes :black-FILL-BOX))))
  (new-filling-style))

(defparameter Color-To-Fill-Mapping
  `((,opal:white . ,opal:white-fill)
    (,opal:black . ,opal:black-fill)
    (,opal:red . ,opal:red-fill)
    (,opal:motif-gray . ,opal:motif-gray-fill)
    (,opal:motif-blue . ,opal:motif-blue-fill)
    (,opal:motif-green . ,opal:motif-green-fill)
    (,opal:motif-orange . ,opal:motif-orange-fill)
    (,opal:green . ,opal:green-fill)
    (,opal:blue . ,opal:blue-fill)
    (,opal:yellow . ,opal:yellow-fill)
    (,opal:purple . ,opal:purple-fill)
    (,opal:cyan . ,opal:cyan-fill)
    (,opal:orange . ,opal:orange-fill)))
    
;; If the color is black, use the filling style specified.  Otherwise, if
;; the filling style is black-fill and there is a color, see if can use one
;; of the standard opal filling-styles.  Otherwise, create a new filling style
;; object.
(defun Check-Colors-For-Fill (new-fill-style new-color sel-obj)
  (let (val)
    (cond ((null new-fill-style) NIL)
	  ((and (eq new-color opal:black)
		(not (is-a-p new-fill-style opal:white-fill)))
		new-fill-style)
	  ((and (eq new-fill-style opal:black-fill)
		(setq val (cdr (assoc new-color Color-To-Fill-Mapping))))
	   val)
	  ;; next check if have the value typed into Other, with no color change
	  ((and (eq sel-obj (g-value fill-prop :fill-boxes :Other-box))
		(eq new-color (g-value new-fill-style :foreground-color)))
	   new-fill-style)
	  (T ; otherwise, create a filling style
	   (let ((kr::*constants-disabled* T))
	     (create-instance NIL opal:filling-style
			;; constant will be written out
			(:constant '(T))
			(:gilt-val new-fill-style)
			(:foreground-color new-color)
			(:stipple (g-value new-fill-style :stipple))
			(:fill-style (g-value new-fill-style :fill-style))))))))


;; Main OK procedure;  Sets the object's fill-style slot.
(defun fill-prop-ok (top-gadget values)
  (declare (ignore values))
  (let ((newsel (g-value top-gadget :fill-boxes :selected))
	(obj-or-objs (g-value top-gadget :for-obj))
	(slot (g-value top-gadget :for-slot))
	val)
    (if (null newsel) (error "Nothing selected for fill-prop")
	(setq val (g-value newsel :val)))
    (cond ((or (null val) (eq val :none))
	   (setq val NIL))
	  ((not (schema-p val)) ; then bad value
	   (setq val opal:black-fill))
	  (T (setq val (Check-Colors-For-Fill val
				    (g-value fill-prop :new-color) newsel))))
    (gilt:set-value-of-appropriate-objs obj-or-objs slot val)))

;; If an opal color filling style, then returns the black fill box, otherwise
;; checks if a created filling style using one of the standard patterns.
;; Call this if not one of the standard filling styles
(defun Check-Fills-For (new-fill-style)
  (let ((val (car (rassoc new-fill-style Color-To-Fill-Mapping))))
    (if val ; then return black fill
	(setq val (g-value fill-prop :fill-boxes :black-FILL-BOX))
	(progn
	  (setq val (g-value new-fill-style :gilt-val))
	  (when val (setq val (find-obj-for-style
			       (g-value fill-prop :fill-boxes) val)))))
    val))

;;;Main procedure to show the dialog box set up for the return
;;;
;;;  1/23/91 amickish: added '(or (null style) ..)' to new-color definition
;;;
(defun Show-Fill-Props-Dialog (left top obj-or-objs slot)
  (let* ((style (gilt:value-from-one-obj obj-or-objs slot))
	 (other-val (g-value fill-prop :other-val))
	 (other-box (g-value fill-prop :fill-boxes :Other-box))
	 (new-color (if (or (null style) (eq style :none))
			opal:black
			(g-value style :foreground-color)))
	initial-selection)
    (cond ((null style) (setq style :none))
	  ((eq style opal:default-filling-style)
	   (setq style opal:black-fill)))
    (s-value fill-prop :for-obj obj-or-objs)
    (s-value fill-prop :for-slot slot)
    (s-value fill-prop :new-color new-color)
    (use-color (g-value fill-prop :current-color) new-color)
    (setq initial-selection (find-obj-for-style
			     (g-value fill-prop :fill-boxes) style))
    (init-value other-val "") ; remove the string in :other
    (when (or (null initial-selection) ; use the "other" box
	      (eq initial-selection other-box))
      ;; first check if a special color fill
      (unless (setq initial-selection (Check-Fills-For style))
	;; use "Other"
	(setq initial-selection other-box)
	; mk-string is in prop-sheet
	(init-value other-val (garnet-gadgets::mk-string style T))))
	
    (inter:SelectObj (g-value fill-prop :press) initial-selection)
    (New-filling-style)
    (show-in-window fill-prop left top)
    ))
