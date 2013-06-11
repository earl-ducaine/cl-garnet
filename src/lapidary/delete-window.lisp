;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -*- Mode: Lisp; Package: LAPIDARY -*-
;;;
;;; Implementation of (lapidary-delete-window)
;;;

(in-package "LAPIDARY")

(create-instance 'delete-window-gadget OPAL:AGGREGADGET
  (:WINDOW-LEFT 14)
  (:WINDOW-TOP 15)
  (:WINDOW-WIDTH 399)
  (:WINDOW-HEIGHT 142)
  (:FUNCTION-FOR-OK `LAPIDARY::DELETE-WINDOW)
  (:EXPORT-P T)
  (:WINDOW-TITLE "Delete Window")
  (:PACKAGE-NAME "LAPIDARY")
  (:LEFT 0)
  (:TOP 0)
  (:WIDTH (o-formula (KR:GVL :WINDOW :WIDTH) 450))
  (:HEIGHT (o-formula (KR:GVL :WINDOW :HEIGHT) 300))
  (:parts `(
    (:window-name ,GARNET-GADGETS:SCROLLING-LABELED-BOX
      (:GILT-REF "TYPE-SCROLLING-LABELED-BOX")
      (:MIN-FRAME-WIDTH NIL)
      (:value "")
      (:FIELD-FONT ,OPAL:DEFAULT-FONT)
      (:FIELD-OFFSET 2)
      (:LABEL-OFFSET 5)
      (:LABEL-FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:BOX (31 60 327 74 ))
      (:MIN-WIDTH 20)
      (:GROW-P T)
      (:LABEL-STRING "")
      (:LEFT ,(o-formula (FIRST (KR:GVL :BOX)) 31))
      (:TOP ,(o-formula (SECOND (KR:GVL :BOX)) 60))
      (:WIDTH ,(o-formula (THIRD (KR:GVL :BOX)) 327)))
   (NIL ,OPAL:MULTI-TEXT
      (:GILT-REF "TYPE-TEXT")
      (:BOX (10 10 370 14 ))
      (:STRING "Enter the name that appears on the window's title bar")
      (:LEFT ,(o-formula (FIRST (KR:GVL :BOX)) 10))
      (:TOP ,(o-formula (SECOND (KR:GVL :BOX)) 10)))
    (NIL ,OPAL:MULTI-TEXT
      (:GILT-REF "TYPE-TEXT")
      (:BOX (115 30 3 3 ))
      (:STRING "or on the window's icon")
      (:LEFT ,(o-formula (FIRST (KR:GVL :BOX)) 115))
      (:TOP ,(o-formula (SECOND (KR:GVL :BOX)) 30)))
    (NIL ,GARNET-GADGETS:TEXT-BUTTON-PANEL
      (:SELECTION-FUNCTION GILT:OKCANCEL-FUNCTION)
      (:GILT-REF "TYPE-OKAPPLYCANCEL")
      (:ITEMS ("OK" "Apply" "Cancel" ))
      (:GRAY-WIDTH 3)
      (:FINAL-FEEDBACK-P NIL)
      (:TEXT-OFFSET 2)
      (:SHADOW-OFFSET 5)
      (:DIRECTION :HORIZONTAL)
      (:BOX (106 100 159 51 ))
      (:LEFT ,(o-formula (FIRST (KR:GVL :BOX)) 106))
      (:TOP ,(o-formula (SECOND (KR:GVL :BOX)) 100))))))



(defun lapidary-delete-window ()
  (gilt::show-in-window delete-window-gadget 300 300 t))
  
(defun delete-window (gadget values)
  (declare (ignore gadget))
  (let ((window-name (second (car values)))
	win)
    ;; try to locate the window by checking the :title and :icon-title
    ;; slots of windows
    (setf win
	  (dolist (window (g-value *selection-info* :window))
	    (when (or (string-equal window-name (g-value window :title))
		      (string-equal window-name (g-value window :icon-title)))
		  (return window))))

    (if win
	(progn
	  ;; if this is the only window, create a new window first, 
	  ;; so that lapidary's interactors will not be blown away
	  (when (null (cdr (g-value *selection-info* :window)))
		(make-drawing-window))
	  (s-value *selection-info* :window 
		   (delete win (g-value *selection-info* :window)))
	  (deselect-objects win)
	  (opal:destroy win)
	  (opal:update-all))
        ;; window could not be found--inform the user
        (lapidary-error 
	 (format nil "Could not find window ~S. You must
enter either the name of a window's title bar
or the name of a window's icon" window-name)))))

