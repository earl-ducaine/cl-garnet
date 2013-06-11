;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file created by GILT V0.2: The Garnet Interface Builder
;;; on Dec 5, 1990, 10:55 AM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
============================================================
Change log:
    
    5-May-92 Brad Vander Zanden - hacked from lapidary read file
    18-May-92 Brad Vander Zanden - remove gadget from aggregate before
                                     adding it to lapidary window
============================================================
|#

(in-package "LAPIDARY")

(create-instance 'add-gadget OPAL:AGGREGADGET
  (:WINDOW-TITLE "Add Gadget")
  (:WINDOW-LEFT 120)
  (:WINDOW-TOP 220)
  (:WINDOW-WIDTH 316)
  (:WINDOW-HEIGHT 175)
  (:PACKAGE-NAME "LAPIDARY")
  (:FUNCTION-FOR-OK `LAPIDARY::add-gadget-ok-fn)
  (:LEFT 0)
  (:TOP 0)
  (:WIDTH (o-formula (GVL :WINDOW :WIDTH) 316))
  (:HEIGHT (o-formula (GVL :WINDOW :HEIGHT) 139))
  (:parts `(
    (NIL ,OPAL:TEXT
      (:FONT ,(create-instance nil OPAL:FONT
            (:SIZE :LARGE)
            (:FACE :BOLD-ITALIC)))
      (:BOX (9 8 35 14 ))
      (:STRING "Adding Gadget...")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 9))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 8)))
    (:GADGETNAME ,GARNET-GADGETS:SCROLLING-LABELED-BOX
      (:GILT-REF "TYPE-SCROLLING-LABELED-BOX")
      (:MIN-FRAME-WIDTH NIL)
      (:SELECT-FUNCTION NIL)
      (:FIELD-FONT ,OPAL:DEFAULT-FONT)
      (:FIELD-OFFSET 2)
      (:LABEL-OFFSET 5)
      (:LABEL-FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:BOX (20 40 285 19 ))
      (:LABEL-STRING "Gadgetname:")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 20))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 40))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 285)))
    (:WINDOWNAME ,GARNET-GADGETS:SCROLLING-LABELED-BOX
      (:GILT-REF "TYPE-SCROLLING-LABELED-BOX")
      (:MIN-FRAME-WIDTH NIL)
      (:SELECT-FUNCTION NIL)
      (:FIELD-FONT ,OPAL:DEFAULT-FONT)
      (:FIELD-OFFSET 2)
      (:LABEL-OFFSET 5)
      (:LABEL-FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:BOX (20 68 285 53 ))
      (:LABEL-STRING "Windowname:")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 20))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 68))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 285)))

    (:instance-p ,garnet-gadgets:radio-button-panel
      (:direction :vertical)
      (:fixed-height-p nil)
      (:text-on-left-p nil)
      (:top ,(o-formula (+ (opal:gv-bottom (gvl :parent :windowname)) 20)))
      (:left 20)
      (:items ("add this object" "add an instance of this object")))

    (NIL ,GARNET-GADGETS:TEXT-BUTTON-PANEL
      (:SELECTION-FUNCTION LAPIDARY::OKCANCEL-FUNCTION)
      (:GILT-REF "TYPE-OKCANCEL")
      (:SELECT-FUNCTION LAPIDARY::OKCANCEL-FUNCTION)
      (:ITEMS ("OK" "Cancel" ))
      (:GRAY-WIDTH 3)
      (:FINAL-FEEDBACK-P NIL)
      (:TEXT-OFFSET 2)
      (:SHADOW-OFFSET 5)
      (:DIRECTION :HORIZONTAL)
      (:BOX (188 6 117 29 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 188))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 6))))))

(defun add-gadget-ok-fn (add-gadget values)
  (declare (special lapidary-query-gadget))
  (let* ((gadgetname (value-of :gadgetname values))
	 (window-name (value-of :windowname values))
	 (instance-p (value-of :instance-p values))
	 (window (get-window-from-string window-name))
	 (gadget (when (not (string= "" gadgetname))
		       (read-from-string gadgetname)))
	 editor-agg)
    (cond ((string= "" gadgetname) (Lapidary-Error "Gadgetname must be supplied"))
	  ((or (not (boundp gadget))
	       (not (schema-p (setf gadget (symbol-value gadget)))))
	   (lapidary-error 
	    (format nil "~S is not the name of a garnet object. You
must enter the name of a garnet object" gadgetname)))
	  ((null window) 
	   ;; window could not be found--inform the user
	   (lapidary-error 
	    (format nil "Could not find window ~S. You must
enter either the name of a window's title bar
or the name of a window's icon" window-name)))

	(T 
	 (setq *Last-WindowName* window-name)

	 ;; determine whether to add the object or an instance of the
	 ;; object
	 (if (string= instance-p "add an instance of this object")
	     (setf gadget (create-instance nil gadget))
	     ;; else--check to see if this object has instances and
	     ;;   warn the user if there are
	     (progn
	       (when (g-value gadget :is-a-inv)
		     (s-value lapidary-query-gadget :selection-function nil)
		     (multiple-value-bind (left top)
		       (opal:convert-coordinates (g-value add-gadget :window)
						 (g-value add-gadget :left)
						 (opal:bottom add-gadget) NIL)
		       (s-value lapidary-query-gadget :window-left left)
		       (s-value lapidary-query-gadget :window-top top)
		       (when (equal (garnet-gadgets:display-query-and-wait 
				    lapidary-query-gadget
				    (format nil "Instances of ~S exist.
Editing it in Lapidary could have unexpected
consequences on applications containing these
instances. Do you wish to proceed?" gadgetname)
				    '("OK" "CANCEL"))
				    "CANCEL")
			     
			     ;; do not want to proceed--get out of here
			     (return-from add-gadget-ok-fn))))))

	 (fix-all-interactors gadget nil)

	 ;; determine if the added objects is a graphical object that
	 ;; will have to be added to a window
	 (when (not (is-a-p gadget inter:interactor))
	       (s-value window :width (max (+ (opal:right gadget) 10)
					   (g-value window :width)))
	       (s-value window :height (max (+ (opal:bottom gadget) 10)
					    (g-value window :height)))

	       ;; if the object is a circle add a formula that computes its 
	       ;; radius, width and height from its diameter
	       (when (is-a-p gadget opal:circle)
		     (s-value gadget :suggested-width 
			      (if (formula-p (get-value gadget :width))
				  (copy-formula (get-value gadget :width))
				  (g-value gadget :width)))
		     (s-value gadget :suggested-height 
			      (if (formula-p (get-value gadget :height))
				  (copy-formula (get-value gadget :height))
				  (g-value gadget :height)))
		     (s-value gadget :diameter 
			      (o-formula (min (gvl :suggested-width) (gvl :suggested-height))))
		     (s-value gadget :width (o-formula (gvl :diameter)))
		     (s-value gadget :height (o-formula (gvl :diameter)))
		     (s-value gadget :radius (o-formula (round (gvl :diameter) 2))))

	       (setf editor-agg (g-value window :editor-agg))
	       (when (g-value gadget :parent)
		     (opal:remove-component (g-value gadget :parent) gadget))
	       (opal:add-component editor-agg gadget))))))

;;; This pops up the add gadget dialog box, after determining the default 
;;; values 
(defun Show-Add-Gadget-Dialog ()
  (declare (special *Last-WindowName* add-gadget))
  (let* ((window (or (get-window-from-string *Last-WindowName*)
		     (car (g-value *selection-info* :window))))
	 (window-name (if window (g-value window :title) "")))
    (set-initial-value add-gadget :gadgetname "")
    (set-initial-value add-gadget :windowname window-name)
    (set-initial-value add-gadget :instance-p "add this object")
    (gilt:show-in-window add-gadget
			 (g-value add-gadget :window-left)
			 (g-value add-gadget :window-top)
			 t)))
