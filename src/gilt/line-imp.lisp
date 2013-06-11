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

#|
============================================================
Change log:
    3/4/93 Brad Myers - made work with multiple objects
    10/22/92 Andrew Mickish - Added gq-type parameter to Line-or-Fill-New-Style
               and fixed Line-Prop-OK to handle NIL line-styles
    10/20/92 Andrew Mickish - Removed Careful-Eval -- superceded by
               gg:Careful-String-Eval
     8/24/92 Brad Myers - make more robust when bad "other": line-prop-ok
     5/08/92 Martin Sjolin - ext:ignore-errors --> ignore-errors
     3/25/92 Andrew Mickish - Get-Values ---> G-Value
     2/18/92 Brad Myers - put (constant T) in generated colors
     2/20/91 Osamu Hashimoto - separated it from particular gadgets
     1/13/91 Brad Myers - fixed bug where color rectangle isn't being reset
    11/30/90 Brad Myers - made to work with Gilt
    11/15/90 Osamu Hashimoto - created
============================================================
|#

(in-package "GILT")

(declaim (special LINE-PROP))

(defun Line-or-Fill-New-Style (prop slot1 slot2 agg-of-choices default-when-bad
				    slots-to-change graphic-quality-type)
  (let* ((obj (g-value prop :current-val))
	 (cur-style (g-value obj slot1))
	 (other-style (g-value obj slot2))
	 (color-to-use (g-value prop :new-color))
	 (style-to-use (g-value agg-of-choices :selected :val))
	 (error-obj (g-value prop :error)))
    (opal:update-all) ; try to separate the previous changes from the
		      ; changes to the new object to avoid flicker
    (s-value error-obj :visible NIL)
    (if (or (null style-to-use)
	    (eq :none style-to-use))
	(progn
	  (when cur-style
	    (s-value obj :other2-style cur-style))
	  (s-value obj slot1 NIL))
	;; else there is a style
	(progn
	  ;; Graphic quality type is expected to be opal:line-style or
	  ;; opal:filling-style
	  (if (is-a-p style-to-use graphic-quality-type)
	      (progn
		(s-value other-style :foreground-color color-to-use)
		(s-value error-obj :visible NIL))
	      ; else bad style, from "other"
	      (progn
		(s-value error-obj :visible T)
		(setq style-to-use default-when-bad)))
	  (unless cur-style ; if nil then get it from "other2"
	    (setq cur-style (g-value obj :other2-style)))
	  (dolist (slot slots-to-change)
	    (s-value other-style slot (g-value style-to-use slot)))
	  (s-value obj slot1 other-style)
	  (s-value obj slot2 cur-style)))))


;;; This is called when there is a new line style, so the final line style
;;; line can be changed
(defun New-Line-Style (&rest args)
  (declare (ignore args))
  (Line-or-Fill-New-Style line-prop :line-style :other-line-style
		       (g-value line-prop :line-style-boxes)
		       opal:dashed-line
		       '(:line-thickness :line-style :dash-pattern :stipple)
		       opal:line-style))

;;; This is called as the OK function when a new color is picked.  It changes the 
;;; color slot and line.
(defun Line-Color-OK (new-color)
  (s-value line-prop :new-color new-color)
  (use-color (g-value line-prop :current-color) new-color)
  ; if the line-style was :none, change it to be black, since usually
  ; wouldn't change the color and not want to use it
  (let ((cur-line (g-value line-prop :line-style-boxes :selected)))
    (when (eq cur-line (g-value line-prop :line-style-boxes :NON-BOX))
      (Inter:SelectObj (g-value line-prop :press)
		       (g-value line-prop :line-style-boxes :LINE0-BOX))))
  (new-line-style))

;;; This is called when the color button is hit to create the color dialog box.
(defun COLOR-PROP-From-Line-Or-Fill (gadget value)
  (declare (ignore value))
  (let* ((prop (g-value gadget :parent))
	 (old-color (g-value prop :new-color))
	 (Color-OK (g-value prop :color-ok-func)))
    (multiple-value-bind (left top)
			 (opal:convert-coordinates (g-value gadget :window)
						   (g-value gadget :left)
						   (opal:bottom gadget) NIL)
      (Show-Color-Prop old-color left (+ top 10) Color-OK))))

(defparameter Color-To-Line-Mapping
  `((,opal:black . ,opal:line-0)
    (,opal:red . ,opal:red-line)
    (,opal:green . ,opal:green-line)
    (,opal:blue . ,opal:blue-line)
    (,opal:yellow . ,opal:yellow-line)
    (,opal:purple . ,opal:purple-line)
    (,opal:cyan . ,opal:cyan-line)
    (,opal:orange . ,opal:orange-line)))

;; If the color is black, use the line style specified.  Otherwise, if
;; the line style is line0 and there is a color, see if can use one
;; of the standard opal line-styles.  Otherwise, create a new line style
;; object
(defun Check-Colors-For-Line (new-line-style new-color sel-obj)
  (let (val)
    (cond ((null new-line-style) NIL)
	  ((eq new-color opal:black) new-line-style)
	  ((and (eq new-line-style opal:line-0)
		(setq val (cdr (assoc new-color Color-To-Line-Mapping))))
	   val)
	  ;; next check if have the value typed into Other, with no color change
	  ((and (eq sel-obj (g-value line-prop :line-style-boxes :Other-box))
		(eq new-color (g-value new-line-style :foreground-color)))
	   new-line-style)
	  (T ; otherwise, create a line style
	   (let ((kr::*constants-disabled* T))
	     (create-instance NIL opal:line-style
		    ;; constant will be written out
		    (:constant '(T))
		    (:gilt-val new-line-style)
		    (:foreground-color new-color)
		    (:line-thickness (g-value new-line-style :line-thickness))
		    (:cap-style (g-value new-line-style :cap-style))
		    (:join-style (g-value new-line-style :join-style))
		    (:line-style (g-value new-line-style :line-style))
		    (:dash-pattern (g-value new-line-style :dash-pattern))
		    (:stipple (g-value new-line-style :stipple))))))))


;; Main OK procedure;  Sets the object's line-style slot.
(defun line-prop-ok (top-gadget values)
  (declare (ignore values))
  (let ((newsel (g-value top-gadget :line-style-boxes :selected))
	(obj-or-objs (g-value top-gadget :for-obj))
	(slot (g-value top-gadget :for-slot))
	val)
    (if (null newsel) (error "Nothing selected for line-prop")
	(setq val (g-value newsel :val)))
    (cond ((or (null val) (eq val :none))
	   (setq val NIL))
	  ((not (schema-p val)) ; then bad value
	   (setq val opal:line-0))
	  (T (setq val (Check-Colors-For-Line val
				    (g-value line-prop :new-color) newsel))))
    (gilt:set-value-of-appropriate-objs obj-or-objs slot val)))

;; Given a style, see if it is one of the standard choices
(defun find-obj-for-style (agg-of-choices style)
  (dolist (obj (g-value agg-of-choices :components))
    (when (eq style (g-value obj :val))
      (return-from find-obj-for-style obj))))

;; If an opal color line style, then returns the line-0 box, otherwise
;; checks if a created line style using one of the standard patterns.
;; Call this if not one of the standard line styles
(defun Check-Lines-For (new-line-style)
  (let ((val (car (rassoc new-line-style Color-To-line-Mapping))))
    (if val ; then return line-0
	(setq val (g-value line-prop :line-style-boxes :LINE0-BOX))
	(progn
	  (setq val (g-value new-line-style :gilt-val))
	  (when val (setq val (find-obj-for-style
			       (g-value line-prop :line-style-boxes) val)))))
    val))

;;;Main procedure to show the dialog box set up for the return
(defun Show-Line-Props-Dialog (left top obj-or-objs slot)
  (let* ((style (or (gilt:value-from-one-obj obj-or-objs slot) :none))
	 (other-val (g-value line-prop :other-val))
	 (other-box (g-value line-prop :line-style-boxes :Other-box))
	 (new-color (if (eq style :none)
			opal:black
			(g-value style :foreground-color)))
	initial-selection)
    (cond ((null style) (setq style :none))
	  ((or (eq style opal:line-1)
	       (eq style opal:default-line-style))
	   (setq style opal:line-0)))
    (s-value line-prop :for-obj obj-or-objs)
    (s-value line-prop :for-slot slot)
    (s-value line-prop :new-color new-color)
    (use-color (g-value line-prop :current-color) new-color)
    (setq initial-selection (find-obj-for-style
			     (g-value line-prop :line-style-boxes) style))
    (init-value other-val "") ; remove the string in :other
    (when (or (null initial-selection) ; use the "other" box
	      (eq initial-selection other-box))
      ;; first check if a special colored line
      (unless (setq initial-selection (Check-Lines-For style))
	;; use "Other"
	(setq initial-selection other-box)
	;mk-string is in prop-sheet
	(init-value other-val (garnet-gadgets::mk-string style T))))
    (inter:SelectObj (g-value line-prop :press) initial-selection)
    (New-Line-style)
    (show-in-window line-prop left top)
    ))
