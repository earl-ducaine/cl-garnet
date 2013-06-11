;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
============================================================
Change log:
    7/6/92 Brad Vander Zanden -- added a new function, restore-line-props,
              that restores defaults when there is a selected object. Also
              added code to save defaults when there is a selected object.
    5/08/92 Martin Sjolin -- ext:ignore-errors --> ignore-errors
    5/27/91 Brad Vander Zanden -- created
============================================================
|#

(in-package "LAPIDARY")

;;; Set the value slot, but first make sure that formulas are set up by
;;; calling g-value
(defun Init-Value (obj new-val)
  (g-value obj :value) ; need to do this to set up the dependencies
  (s-value obj :value new-val))

(defun Line-or-Fill-New-Style (prop slot1 slot2 agg-of-choices default-when-bad
				    slots-to-change)
  (let* ((obj (g-value prop :current-val))
	 (cur-style (g-value obj slot1))
	 (other-style (g-value obj slot2))
	 (color-to-use (g-value prop :new-color))
	 (style-selection (or (g-value agg-of-choices :selected)
			      (g-value agg-of-choices :default)))
	 (style-to-use (g-value style-selection :val))
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
	  (if (is-a-p style-to-use opal:graphic-quality)
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

(defun careful-eval (string)
  (multiple-value-bind (val errorp)
    #+cmu (ignore-errors (read-from-string string))
    #+lcl3.0 (system::ignore-errors (read-from-string string))
    #+allegro (excl::ignore-errors (read-from-string string))
    #-(or cmu lcl3.0 allegro) (read-from-string string) ; don't know the handler
    (if (numberp errorp) ; then no error
	(progn
	  (when (or (and (symbolp val)(boundp val)) ; if a symbol or list, eval it
		    (listp val))
	    (setq val (eval val))
	    ))
	(progn ; error, just use string
	  (setq val string)))
    val))


;;; This is called when there is a new line style, so the final line style
;;; line can be changed
(defun New-Line-Style (&rest args)
  (declare (special line-prop))
  (declare (ignore args))
  (Line-or-Fill-New-Style line-prop :line-style :other-line-style
		       (g-value line-prop :line-style-boxes)
		       opal:dashed-line
		       '(:line-thickness :line-style :dash-pattern :stipple)))

;;; This is called as the OK function when a new color is picked.  It changes the 
;;; color slot and line.
(defun Line-Color-OK (new-color)
  (declare (special line-prop))
  (s-value line-prop :new-color new-color)
  (use-color (g-value line-prop :current-color) new-color)
  ; if the line-style was :none, change it to be black, since usually
  ; wouldn't change the color and not want to use it
  (let ((cur-line (g-value line-prop :line-style-boxes :selected)))
    (cond ((eq cur-line (g-value line-prop :line-style-boxes :NON-BOX))
	   (Inter:SelectObj (g-value line-prop :press)
			    (g-value line-prop :line-style-boxes :LINE0-BOX)))
	  ;; if a constraint used to be in place, select the appropriate
	  ;; line style
	  ((and (null cur-line)
		(g-value line-prop :constraint-button :selected))
	   (let ((default-line (g-value line-prop :line-style-boxes :default)))
	     (if (eq default-line 
		     (g-value line-prop :line-style-boxes :NON-BOX))
		 (Inter:SelectObj (g-value line-prop :press)
				  (g-value line-prop :line-style-boxes :line0-box))
	         (Inter:SelectObj (g-value line-prop :press) default-line))))))
  (new-line-style))

;;; This is called when the color button is hit to create the color dialog box.
(defun COLOR-PROP-From-Line-Or-Fill (gadget value)
  (declare (special line-prop))
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
  (declare (special line-prop))
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
	   (create-instance NIL opal:line-style
		    (:lapidary-val new-line-style)
		    (:foreground-color new-color)
		    (:line-thickness (g-value new-line-style :line-thickness))
		    (:cap-style (g-value new-line-style :cap-style))
		    (:join-style (g-value new-line-style :join-style))
		    (:line-style (g-value new-line-style :line-style))
		    (:dash-pattern (g-value new-line-style :dash-pattern))
		    (:stipple (g-value new-line-style :stipple)))))))


;; Main OK procedure;  Sets the object's line-style slot.
(defun line-prop-ok (top-gadget values)
  (declare (special line-prop))
  (declare (ignore values))
  (let ((newsel (g-value top-gadget :line-style-boxes :selected))
	(slot (g-value top-gadget :for-slot))
	(constraint-p (g-value top-gadget :constraint-button :selected))
	val)
    (if (null newsel) 
	(if constraint-p
	    (setf val (g-value top-gadget :constraint-button :val))
	    (error "Nothing selected for line-prop"))
        (progn
	  (setq val (g-value newsel :val))
	  (when (null val) (Error "no value in newsel ~s" newsel))))
    (when (eq val :none) (setq val NIL))
;    (reset-undo)
    (when (null constraint-p)
	  (setq val (Check-Colors-For-Line val 
			 (g-value line-prop :new-color) newsel))
	  ;; do nothing if the line-style is a string--this means there
	  ;; was an error in the other box
	  (when (not (stringp val))
		(dolist (obj (g-value *selection-info* :selected))
;		  (undo-save obj slot)
		  (destroy-constraint obj slot)
		  (s-value obj slot val))))

    ;; if there is a selection, restore the original values
    (restore-line-props)
))

;; if there is a selection, restore the original values--we only want
;; the current line-style to change if the user changes it when there
;; are no selections

(defun restore-line-props (&rest args)
  (declare (ignore args))
  (declare (special line-prop))
;  (undo)
  (when (g-value lapidary::*selection-info* :selected)
	(s-value (g-value line-prop :constraint-button) :selected nil)
	(s-value (g-value line-prop :line-style-boxes) :selected
		 (g-value line-prop :save-selected))
	(s-value line-prop :other-val (g-value line-prop :save-other-val))
	(s-value line-prop :other-box (g-value line-prop :save-other-box))
	(s-value line-prop :new-color (g-value line-prop :save-new-color))
	(s-value line-prop :current-color 
		 (g-value line-prop :save-current-color))
	(s-value line-prop :default (g-value line-prop :save-default))
	(new-line-style)))

;; Given a style, see if it is one of the standard choices
(defun find-obj-for-style (agg-of-choices style)
  (dolist (obj (g-value agg-of-choices :components))
    (when (eq style (g-value obj :val))
      (return-from find-obj-for-style obj))))

;; If an opal color line style, then returns the line-0 box, otherwise
;; checks if a created line style using one of the standard patterns.
;; Call this if not one of the standard line styles
(defun Check-Lines-For (new-line-style)
  (declare (special line-prop))
  (let ((val (car (rassoc new-line-style Color-To-line-Mapping))))
    (if val ; then return line-0
	(setq val (g-value line-prop :line-style-boxes :LINE0-BOX))
	(progn
	  (setq val (g-value new-line-style :lapidary-val))
	  (when val (setq val (find-obj-for-style
			       (g-value line-prop :line-style-boxes) val)))))
    val))

;;;Main procedure to show the dialog box set up for the return
(defun Show-Line-Props-Dialog (left top slot)
  (declare (special line-prop))
  (let* ((for-obj (car (g-value *selection-info* :selected)))
	 (style (cond (for-obj (g-value for-obj slot))
		      ((g-value line-prop :window) :default)
		      (t opal:line-0)))
	 (other-val (g-value line-prop :other-val))
	 (other-box (g-value line-prop :line-style-boxes :Other-box))
	 (new-color (cond ((schema-p style)
			   (or (g-value style :foreground-color)
			       (g-value line-prop :new-color)))
			  (t (g-value line-prop :new-color))))
	initial-selection)
    ;; if there is a selection, save the current values so they can be
    ;; restored once the line-style of the selection has been changed
    (when for-obj
	  (s-value line-prop :save-selected
		   (g-value line-prop :line-style-boxes :selected))
	  (s-value line-prop :save-other-val other-val)
	  (s-value line-prop :save-other-box other-box)
	  (s-value line-prop :save-new-color (g-value line-prop :new-color))
    	  (s-value line-prop :save-current-color 
		   (g-value line-prop :current-color))
	  (s-value line-prop :save-default (g-value line-prop :default)))
    (cond ((null style) (setq style :none))
	  ((or (eq style opal:line-1)
	       (eq style opal:default-line-style))
	   (setq style opal:line-0)))
    (s-value line-prop :for-obj for-obj)
    (s-value line-prop :for-slot slot)
    (s-value line-prop :new-color new-color)
    (use-color (g-value line-prop :current-color) new-color)
    (setq initial-selection 
	  (cond ((eq style :default) style)
		(t (find-obj-for-style
		    (g-value line-prop :line-style-boxes) style))))
    (init-value other-val "") ; remove the string in :other
    (when (or (null initial-selection) ; use the "other" box
	      (eq initial-selection other-box))
      ;; first check if a special colored line
      (unless (setq initial-selection (Check-Lines-For style))
	;; use "Other"
	(setq initial-selection other-box)
	;mk-string is in prop-sheet
	(init-value other-val (if (stringp style) 
				  style
				  (let ((kr::*print-as-structure* nil))
				    (prin1-to-string style))))))
    (cond ((and for-obj (formula-p (get-value for-obj :line-style)))
	   (s-value (g-value line-prop :constraint-button) :val nil)
	   ;; make sure the function that sets new line styles can
	   ;; find the style if the user replaces the constraint with
	   ;; a color 
	   (s-value (g-value line-prop :line-style-boxes) :default 
		    initial-selection)
	   (inter:SelectObj (g-value line-prop :press) nil)
	   (new-line-style))
	  ((eq initial-selection :default) t)
	  (t (inter:SelectObj (g-value line-prop :press) initial-selection)
	     (New-Line-style)))

    (gilt:show-in-window line-prop left top t)
    ))

(defun props-constraint-final-fn (gadget value)
  (declare (ignore value))
  (declare (special *selection-info*))

  ;; determine if there is a selection
  (let ((selected (g-value *selection-info* :selected))
	(slot (g-value gadget :slot)))
    (if selected
	(progn
	  ;; deselect the item in the menu gadget
	  (s-value (g-value gadget :menu-gadget) :selected nil)

	  ;; get the top-level gadget and execute the cancel code. we
	  ;; want to pass control to c32 and this is the best way to
	  ;; clean up and do it
	  (gilt::standard-cancel-function (g-value gadget :parent))

	  ;; get the custom constraint
	  (gg:c32 (car selected) slot))
        ;; else there is no selection--deselect the constraint button
        (s-value gadget :selected nil))))
