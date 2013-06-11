;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
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
      7/6/92 Brad Vander Zanden -- added a new function, restore-fill-props,
              that restores defaults when there is a selected object. Also
              added code to save defaults when there is a selected object.
      5/27/91: Brad Vander Zanden -- created
============================================================
|#

(in-package "LAPIDARY")

;;; This is called when there is a new filling style, so the final filling style
;;; rectangle can be changed
(defun New-Filling-Style (&rest args)
  (declare (special fill-prop))
  (declare (ignore args))
  (Line-or-Fill-New-Style fill-prop :filling-style :other-filling-style
		       (g-value fill-prop :fill-boxes)
		       opal:white-fill '(:stipple :fill-style)))

;;; This is called as the OK function when a new color is picked.  It changes the 
;;; color slot and rectangles.
(defun Fill-Color-OK (new-color)
  (declare (special fill-prop))
  (s-value fill-prop :new-color new-color)
  (use-color (g-value fill-prop :current-color) new-color)
  ; if the filling-style was :none or :white, change it to be black, since usually
  ; wouldn't change the color and not want to use it
  (let ((cur-fill (g-value fill-prop :fill-boxes :selected)))
    (cond ((or (eq cur-fill (g-value fill-prop :fill-boxes :NON-FILL-BOX))
	       (eq cur-fill (g-value fill-prop :fill-boxes :WHITE-FILL-BOX)))
	   (Inter:SelectObj (g-value fill-prop :press)
			    (g-value fill-prop :fill-boxes :black-FILL-BOX)))
	  ;; if a constraint used to be in place, select the appropriate
	  ;; filling style
	  ((and (null cur-fill)
		(g-value fill-prop :constraint-button :selected))
	   (let ((default-fill (g-value fill-prop :fill-boxes :default)))
	     (if (or (eq default-fill 
			 (g-value fill-prop :fill-boxes :NON-FILL-BOX))
		     (eq default-fill 
			 (g-value fill-prop :fill-boxes :WHITE-FILL-BOX)))
		 (Inter:SelectObj (g-value fill-prop :press)
			    (g-value fill-prop :fill-boxes :black-FILL-BOX))
	         (Inter:SelectObj (g-value fill-prop :press) default-fill))))))
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
;; object
(defun Check-Colors-For-Fill (new-fill-style new-color sel-obj)
  (declare (special fill-prop))
  (let (val)
    (cond ((null new-fill-style) NIL)
	  ((and (eq new-color opal:black)
		(not (is-a-p new-fill-style opal:white-fill))) new-fill-style)
	  ((and (eq new-fill-style opal:black-fill)
		(setq val (cdr (assoc new-color Color-To-Fill-Mapping))))
	   val)
	  ;; next check if have the value typed into Other, with no color change
	  ((and (eq sel-obj (g-value fill-prop :fill-boxes :Other-box))
		(eq new-color (g-value new-fill-style :foreground-color)))
	   new-fill-style)
	  (T ; otherwise, create a filling style
	   (create-instance NIL opal:filling-style
			    (:lapidary-val new-fill-style)
			    (:foreground-color new-color)
			    (:stipple (g-value new-fill-style :stipple))
			    (:fill-style (g-value new-fill-style :fill-style)))))))


;; Main OK procedure;  Sets the object's fill-style slot.
(defun fill-prop-ok (top-gadget values)
  (declare (special fill-prop))
  (declare (ignore values))
  (let ((newsel (g-value top-gadget :fill-boxes :selected))
	(slot (g-value top-gadget :for-slot))
	(constraint-p (g-value top-gadget :constraint-button :selected))
	val)    
    (if (null newsel) 
	(if constraint-p
	    (setf val (g-value top-gadget :constraint-button :val))
	    (error "Nothing selected for fill-prop"))
        (progn
	  (setq val (g-value newsel :val))
	  (when (null val) (Error "no value in newsel ~s" newsel))))
    (when (eq val :none) (setq val NIL))
    (when (null constraint-p)
	  (setq val (Check-Colors-For-Fill val 
			 (g-value fill-prop :new-color) newsel))
;;	  (reset-undo)
	  (when (not (stringp val))
		(dolist (obj (g-value *selection-info* :selected))
;		  (undo-save obj slot)
		  (destroy-constraint obj slot)
		  (s-value obj slot val))))

    ;; if there is a selection, restore the original values
    (reset-undo)
    (restore-fill-props)
))


;; if there is a selection, restore the original values--we only want
;; the current fill-style to change if the user changes it when there
;; are no selections

(defun restore-fill-props (&rest args)
  (declare (special fill-prop))
  (declare (ignore args))
;  (undo)
  (when (g-value lapidary::*selection-info* :selected)
	;; make sure constraint button is not selected
	(s-value (g-value fill-prop :constraint-button) :selected nil)
	(s-value (g-value fill-prop :fill-boxes) :selected
		 (g-value fill-prop :save-selected))
	(s-value fill-prop :other-val (g-value fill-prop :save-other-val))
	(s-value fill-prop :other-box (g-value fill-prop :save-other-box))
	(s-value fill-prop :new-color (g-value fill-prop :save-new-color))
	(s-value fill-prop :current-color 
		 (g-value fill-prop :save-current-color))
	(s-value fill-prop :default (g-value fill-prop :save-default))
	(new-filling-style)))

;; If an opal color filling style, then returns the black fill box, otherwise
;; checks if a created filling style using one of the standard patterns.
;; Call this if not one of the standard filling styles
(defun Check-Fills-For (new-fill-style)
  (declare (special fill-prop))
  (let ((val (car (rassoc new-fill-style Color-To-Fill-Mapping))))
    (if val ; then return black fill
	(setq val (g-value fill-prop :fill-boxes :black-FILL-BOX))
	(progn
	  (setq val (g-value new-fill-style :lapidary-val))
	  (when val (setq val (find-obj-for-style
			       (g-value fill-prop :fill-boxes) val)))))
    val))

;;;Main procedure to show the dialog box set up for the return
;;;
;;;  1/23/91 amickish: added '(or (null style) ..)' to new-color definition
;;;
(defun Show-Fill-Props-Dialog (left top slot)
  (declare (special fill-prop))
  (reset-undo)
  (let* ((for-obj (car (g-value *selection-info* :selected)))
	 (style (cond (for-obj 
		       (g-value for-obj slot))
		      ((g-value fill-prop :window) :default)
		      (t nil)))
	 (other-val (g-value fill-prop :other-val))
	 (other-box (g-value fill-prop :fill-boxes :Other-box))
	 (new-color (cond ((schema-p style)
			   (or (g-value style :foreground-color)
			       (g-value fill-prop :new-color)))
			  (t (g-value fill-prop :new-color))))
	initial-selection)

    ;; if there is a selection, save the current values so they can be
    ;; restored once the fill-style of the selection has been changed
    (when for-obj
	  (s-value fill-prop :save-selected
		   (g-value fill-prop :fill-boxes :selected))
	  (s-value fill-prop :save-other-val other-val)
	  (s-value fill-prop :save-other-box other-box)
	  (s-value fill-prop :save-new-color (g-value fill-prop :new-color))
    	  (s-value fill-prop :save-current-color 
		   (g-value fill-prop :current-color))
	  (s-value fill-prop :save-default (g-value fill-prop :default)))
    (cond ((null style) (setq style :none))
	  ((eq style opal:default-filling-style)
	   (setq style opal:black-fill)))
    (s-value fill-prop :for-obj for-obj)
    (s-value fill-prop :for-slot slot)
    (s-value fill-prop :new-color new-color)
    (use-color (g-value fill-prop :current-color) new-color)
    (setq initial-selection 
	  (cond ((eq style :default) style)
		(t (find-obj-for-style
		    (g-value fill-prop :fill-boxes) style))))
    (init-value other-val "") ; remove the string in :other
    (when (or (null initial-selection) ; use the "other" box
	      (eq initial-selection other-box))
      ;; first check if a special color fill
      (unless (setq initial-selection (Check-Fills-For style))
	;; use "Other"
	(setq initial-selection other-box)
	; mk-string is in prop-sheet
	(init-value other-val (if (stringp style)
				  style
				  (let ((kr::*print-as-structure* nil))
				    (prin1-to-string style))))))
	
    (cond ((and for-obj (formula-p (get-value for-obj slot)))
	   (s-value (g-value fill-prop :constraint-button) :val nil)
	   ;; make sure the function that sets new filling styles can
	   ;; find the style if the user replaces the constraint with
	   ;; a color 
	   (s-value (g-value fill-prop :fill-boxes) :default initial-selection)
	   (inter:SelectObj (g-value fill-prop :press) nil)
	   (new-filling-style))
	  ((eq initial-selection :default) t)
	  (t (inter:SelectObj (g-value fill-prop :press) initial-selection)
	     (New-filling-style)))

    (gilt:show-in-window fill-prop left top t)
    ))
