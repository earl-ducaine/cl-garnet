;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: C32; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file created by GILT V0.4: The Garnet Interface Builder
;;; on Apr 14, 1991, 11:38 AM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id::                                                     $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CHANGE LOG:
;;;
;;; 12/16/92 Andrew Mickish - opal:window ---> opal::window
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "C32")

(defparameter c32-point-to-obj-feedback NIL)
(defparameter ask-object NIL)


;; get the aggregate for the selected object
(defun getobjagg (sel-obj)
  (let ((aggobj (g-value sel-obj :parent)))
    (or aggobj ; if there, then use it
	;else must be top agg, so go to window
	(if (is-a-p sel-obj opal::window)
	    (inter:beep) ;; already at the top
	    (g-value sel-obj :window)))))

;; get the object hiding underneath the specified object
(defun getobjunder (sel-obj)
  (let ((x (opal:center-x sel-obj))
	(y (opal:center-y sel-obj))
	newobj)
    (if (is-a-p sel-obj opal::window)
	(inter:beep)
    ;;; ** THIS DOESN'T REALLY WORK, SINCE WILL ONLY ALTERNATE BETWEEN
    ;;; ** THE TWO TOP OBJECTS AT THE POINT.  NEED OPAL'S HELP WITH THIS.
	(progn
	  (s-value sel-obj :visible NIL)
	  (setq newobj (opal:point-to-leaf (g-value sel-obj :window :aggregate)
				       x y))
	  (s-value sel-obj :visible T)
	  newobj))))



(defun Set-New-Obj (top-gadget obj)
  (s-value top-gadget :sel-obj obj)
  ;; next remove feedback object if it was visible
  (let ((agg (g-value c32-point-to-obj-feedback :parent))
	(newagg (when obj (g-value obj :window :aggregate))))
    (if (and agg (or (null obj)
		     (not (eq agg newagg))))
      (opal:remove-component agg c32-point-to-obj-feedback))
    ;; now make feedback object show over the new object
    (if obj
      (progn
	(s-value c32-point-to-obj-feedback :obj-over obj)
	(s-value c32-point-to-obj-feedback :visible T)
	(when (not (eq agg newagg))
	  (opal:add-component (g-value obj :window :aggregate)
			      c32-point-to-obj-feedback))
	;; now find the slot of that object
	(when (g-value top-gadget :slot-func)
	  (s-value top-gadget :obj-slot
		   (kr-send top-gadget :slot-func
			    (g-value top-gadget :from-obj)
			    (g-value top-gadget :from-slot)
			    obj
			    (inter:event-x inter:*current-event*)
			    (inter:event-y inter:*current-event*)))))
      ;; else remove slot
      (s-value top-gadget :obj-slot NIL))))



(defun create-ask-object ()
  (create-instance 'ASK-OBJECT OPAL:AGGREGADGET
    (:WINDOW-LEFT 300)
    (:WINDOW-TOP 400)
    (:WINDOW-WIDTH 600)
    (:WINDOW-HEIGHT 146)
    (:FUNCTION-FOR-OK 'c32::pop-up-ask-object-ok)
    (:EXPORT-P NIL)
    (:WINDOW-TITLE "Point to an Object")
    (:PACKAGE-NAME "C32")
    (:LEFT 0)
    (:TOP 0)
    (:WIDTH (o-formula (GVL :WINDOW :WIDTH) 476))
    (:HEIGHT (o-formula (GVL :WINDOW :HEIGHT) 146))
    (:slot-func NIL)			; function to get the slot name, if desired
    (:obj-slot NIL)			; slot name of the object
    (:sel-obj NIL)			; the selected object
    (:sel-obj-string (o-formula (write-obj (gvl :sel-obj))))
    (:obj-slot-string (o-formula (write-to-string (gvl :obj-slot))))
    (:from-obj NIL)			; used to guess the slot by slot-func
    (:from-slot NIL)			; used to guess the slot by slot-func

    (:interactors `(
		    (:obj-find ,inter:button-interactor
		     (:window T)	; all garnet-windows
		     (:start-where T)	; any where
		     (:continuous NIL)
		     (:start-event :control-leftdown)
		     (:waiting-priority ,inter:high-priority-level)
		     (:final-function
		      ,#'(lambda (inter obj)
			   (declare (ignore obj)) ; have to search since might find feedback
			   (let (newobj)
			     (let ((top-gadget (g-value inter :operates-on)))
			       (Set-New-Obj top-gadget NIL) ; turn off feedback before looking
			       (setq newobj (opal:point-to-leaf
					     (g-value inter :current-window :aggregate)
					     (inter:event-x inter:*current-event*)
					     (inter:event-y inter:*current-event*)))
			       (Set-New-Obj top-gadget newobj))))))
		    (:key-handle ,inter:button-interactor
		     (:window ,(o-formula (gvl :operates-on :window)))
		     (:start-where T)
		     (:start-event (:uparrow :downarrow))
		     (:continuous NIL)
		     (:waiting-priority ,inter:high-priority-level)
		     (:final-function
		      ,#'(lambda (inter obj)
			   (declare (ignore obj))
			   (let* ((top-gadget (g-value inter :operates-on))
				  (sel-obj (g-value top-gadget :sel-obj))
				  newobj)
			     (if sel-obj
			       (progn
				 (case (g-value inter :start-char)
				   (:downarrow (setq newobj (getobjunder sel-obj)))
				   (:uparrow (setq newobj (getobjagg sel-obj)))
				   (T (error "bad char")))
				 (when newobj
				   (Set-New-Obj top-gadget newobj)))
			       ;; else nothing selected
			       (inter:beep))))))))

    (:parts `(
	      (:instr ,OPAL:MULTI-TEXT
	       (:STRING "Press on an object with control-left button
Move mouse to this window, and type up-arrow
to get to its aggregate.  Use down-arrow to
get to object underneath it.  Press OK when done.")
	       (:BOX (10 10 3 3 ))
	       (:FONT ,(create-instance nil OPAL:FONT
			 (:FACE :BOLD)))
	       (:GILT-REF "TYPE-TEXT")
	       (:LEFT ,(o-formula (FIRST (GVL :BOX)) 10))
	       (:TOP ,(o-formula (SECOND (GVL :BOX)) 10)))
	      (:OBJ-LABEL ,OPAL:MULTI-TEXT
	       (:STRING "Object: ")
	       (:BOX (10 92 3 3 ))
	       (:FONT ,c32-bold-font)
	       (:GILT-REF "TYPE-TEXT")
	       (:LEFT ,(o-formula (FIRST (GVL :BOX)) 10))
	       (:TOP ,(o-formula (SECOND (GVL :BOX)) 92)))
	      (:OBJECT-NAME ,OPAL:TEXT
	       (:STRING ,(o-formula (gvl :parent :sel-obj-string)))
	       (:FONT ,c32-font)
	       (:BOX (70 92 3 3 ))
	       (:GILT-REF "TYPE-TEXT")
	       (:LEFT ,(o-formula (FIRST (GVL :BOX)) 70))
	       (:TOP ,(o-formula (SECOND (GVL :BOX)) 92)))
	      (:OK-CANCEL ,GARNET-GADGETS:TEXT-BUTTON-PANEL
	       (:BOX (500 10 117 29 ))
	       (:DIRECTION :Vertical)
	       (:FONT ,c32-button-font) 
	       (:SHADOW-OFFSET 5)
	       (:TEXT-OFFSET 2)
	       (:FINAL-FEEDBACK-P NIL)
	       (:GRAY-WIDTH 3)
	       (:ITEMS ("OK" "Apply" "Cancel" ))
	       (:GILT-REF "TYPE-OKCANCEL")
	       (:SELECTION-FUNCTION NIL) ; ** set below
	       (:LEFT ,(o-formula (FIRST (GVL :BOX)) 500))
	       (:TOP ,(o-formula (SECOND (GVL :BOX)) 10)))
	      (:LIN ,OPAL:LINE
	       (:Y2 ,(o-formula (FOURTH (GVL :POINTS)) 85))
	       (:X2 ,(o-formula (THIRD (GVL :POINTS)) 361))
	       (:Y1 ,(o-formula (SECOND (GVL :POINTS)) 85))
	       (:X1 ,(o-formula (FIRST (GVL :POINTS)) 87))
	       (:GROW-P T)
	       (:LINE-P T)
	       (:POINTS (87 85 361 85 ))
	       (:GILT-REF "TYPE-LINE"))
	      (:slot-LABEL ,OPAL:MULTI-TEXT
	       (:GILT-REF "TYPE-TEXT")
	       (:FONT ,(opal:get-standard-font :sans-serif :bold nil))
	       ;(:FONT ,(create-instance nil OPAL:FONT
		;	 (:FACE :BOLD)))
	       (:BOX (24 116 32 14 ))
	       (:visible ,(o-formula (gvl :parent :slot-func)))
	       (:STRING "Slot:")
	       (:LEFT ,(o-formula (FIRST (GVL :BOX)) 24))
	       (:TOP ,(o-formula (SECOND (GVL :BOX)) 116)))
	      (:SLOT-NAME ,OPAL:TEXT
	       (:GILT-REF "TYPE-TEXT")
	       (:BOX (67 116 3 3 ))
	       (:visible ,(o-formula (gvl :parent :slot-func)))
	       (:STRING ,(o-formula (gvl :parent :obj-slot-string)))
	       (:LEFT ,(o-formula (FIRST (GVL :BOX)) 67))
	       (:TOP ,(o-formula (SECOND (GVL :BOX)) 116))))))

  (s-value (g-value ASK-OBJECT :OK-CANCEL) :SELECTION-FUNCTION
	   #'(lambda (gadget value)
	       ;; turn off the feedback object
	       (s-value c32-point-to-obj-feedback :visible NIL)
	       (gilt:OKCANCEL-FUNCTION gadget value))))



(defun create-c32-point-to-obj-feedback ()
  (create-instance 'c32-point-to-obj-feedback opal:rectangle
    (:line-style (create-instance NIL opal:line-style
		   (:thickness 2)
		   (:foreground-color opal:red)))
    (:filling-style opal:black-fill)
    (:draw-function :xor)
    (:obj-over NIL)
    (:left (o-formula (let ((obj (gvl :obj-over)))
			(if (is-a-p obj opal::window)
			  0
			  (- (gv obj :left) 1)))))
    (:top (o-formula (let ((obj (gvl :obj-over)))
		       (if (is-a-p obj opal::window)
			 0
			 (- (gv obj :top) 1)))))
    (:width (o-formula (+ (gvl :obj-over :width) 2)))
    (:height (o-formula (+ (gvl :obj-over :height) 2)))))
