;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GILT; Base: 10 -*-
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
;;; on Dec 5, 1990, 10:49 AM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
============================================================
Change log:
   7/01/93 Andrew Mickish - Moved Show-Save-Dialog here from gilt.lisp
   2/19/92 Brad Myers - added constant definitions
   3/13/91 Osamu Hashimoto - Changed grayout from rectangle-covering to :active-p
   02/28/91 Andrew Mickish - Changed Export-p to be a Motif-Check-Button
   02/27/91 Osamu Hashimoto - Changed into Motif-version
   12/5/90 Brad Myers - hacked from created file
   12/5/90 Gilt - created
============================================================
|#

(in-package "GILT")

(create-instance 'MOTIF-GILT-SAVE-BOLD-FONT opal:font
		 (:CONSTANT '(T))
		 (:face :bold))

(create-instance 'SAVE-FILE OPAL:AGGREGADGET
  (:WINDOW-TITLE "Save File")
  (:WINDOW-LEFT 100)
  (:WINDOW-TOP 200)
  (:WINDOW-WIDTH 316)
  (:WINDOW-HEIGHT 228)
  (:window-background-color opal:motif-gray)
  (:PACKAGE-NAME "GILT")
  (:FUNCTION-FOR-OK `GILT::DO-SAVE-FILE)
  (:LEFT 0)
  (:TOP 0)
  (:WIDTH (o-formula (GVL :WINDOW :WIDTH) 316))
  (:HEIGHT (o-formula (GVL :WINDOW :HEIGHT) 228))
  (:parts `(
    (NIL ,OPAL:TEXT
      (:CONSTANT (T))
      (:GILT-REF "TYPE-TEXT")
      (:FONT ,(create-instance nil OPAL:FONT
                 (:CONSTANT '(T))
		 (:SIZE :LARGE)
		 (:FACE :BOLD-ITALIC)))
      (:BOX (9 8 35 14 ))
      (:STRING "Saving...")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 9))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 8)))
    (:FILENAME ,GARNET-GADGETS:MOTIF-SCROLLING-LABELED-BOX
      (:CONSTANT (T))
      (:GILT-REF "TYPE-SCROLLING-LABELED-BOX")
      (:MIN-FRAME-WIDTH NIL)
      (:SELECT-FUNCTION NIL)
      (:FIELD-FONT ,OPAL:DEFAULT-FONT)
      (:FIELD-OFFSET 2)
      (:LABEL-OFFSET 5)
      (:LABEL-FONT ,MOTIF-GILT-SAVE-BOLD-FONT)
      (:BOX (20 40 285 19 ))
      (:LABEL-STRING "Filename:")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 20))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 40))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 285)))
    (:GADGET-NAME ,GARNET-GADGETS:MOTIF-SCROLLING-LABELED-BOX
      (:CONSTANT (T))
      (:GILT-REF "TYPE-SCROLLING-LABELED-BOX")  
      (:MIN-FRAME-WIDTH NIL)
      (:SELECT-FUNCTION NIL)
      (:FIELD-FONT ,OPAL:DEFAULT-FONT)
      (:FIELD-OFFSET 2)
      (:LABEL-OFFSET 5)
      (:LABEL-FONT ,MOTIF-GILT-SAVE-BOLD-FONT)
      (:LABEL-STRING "Top-level Gadget name:")
      (:BOX (20 70 285 18 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 20))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 70))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 285)))
    (:WIN-TITLE ,GARNET-GADGETS:MOTIF-SCROLLING-LABELED-BOX
      (:CONSTANT (T))
      (:GILT-REF "TYPE-SCROLLING-LABELED-BOX")
      (:MIN-FRAME-WIDTH NIL)
      (:SELECT-FUNCTION NIL)
      (:FIELD-FONT ,OPAL:DEFAULT-FONT)
      (:FIELD-OFFSET 2)
      (:LABEL-OFFSET 5)
      (:LABEL-FONT ,MOTIF-GILT-SAVE-BOLD-FONT)
      (:LABEL-STRING "Window Title:")
      (:BOX (20 130 285 18 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 20))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 130))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 285)))
    (:PACKAGE-NAME ,GARNET-GADGETS:MOTIF-SCROLLING-LABELED-BOX
      (:GILT-REF "TYPE-SCROLLING-LABELED-BOX")
      (:CONSTANT (T))
      (:MIN-FRAME-WIDTH NIL)
      (:SELECT-FUNCTION NIL)
      (:FIELD-FONT ,OPAL:DEFAULT-FONT)
      (:FIELD-OFFSET 2)
      (:LABEL-OFFSET 5)
      (:LABEL-FONT ,MOTIF-GILT-SAVE-BOLD-FONT)
      (:LABEL-STRING "Package name:")
      (:BOX (20 160 285 18 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 20))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 160))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 285)))
    (:FUNCTION-FOR-OK-NAME ,GARNET-GADGETS:MOTIF-SCROLLING-LABELED-BOX
      (:CONSTANT (T :EXCEPT :ACTIVE-p))
      (:GILT-REF "TYPE-SCROLLING-LABELED-BOX")
      (:MIN-FRAME-WIDTH NIL)
      (:SELECT-FUNCTION NIL)
      (:FIELD-FONT ,OPAL:DEFAULT-FONT)
      (:FIELD-OFFSET 2)
      (:LABEL-OFFSET 5)
      (:LABEL-FONT ,MOTIF-GILT-SAVE-BOLD-FONT)
      (:BOX (20 190 285 18 ))
      (:LABEL-STRING "Function-for-OK name:")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 20))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 190))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 285)))
    (NIL ,GARNET-GADGETS:MOTIF-TEXT-BUTTON-PANEL
      (:SELECTION-FUNCTION GILT:OKCANCEL-FUNCTION)
      (:CONSTANT (T))
      (:GILT-REF "TYPE-OKCANCEL")
      (:SELECT-FUNCTION GILT:OKCANCEL-FUNCTION)
      (:ITEMS ("OK" "Cancel" ))
      (:GRAY-WIDTH 3)
      (:FINAL-FEEDBACK-P NIL)
      (:TEXT-OFFSET 5)
      (:SHADOW-OFFSET 5)
      (:DIRECTION :HORIZONTAL)
      (:BOX (188 6 117 29 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 188))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 6)))
    (:EXPORT-P ,GARNET-GADGETS:MOTIF-CHECK-BUTTON
      (:CONSTANT (T))
      (:SELECT-FUNCTION NIL)
      (:TEXT-ON-LEFT-P T)
      (:BUTTON-HEIGHT 20)
      (:TEXT-OFFSET 5)
      (:FONT ,MOTIF-GILT-SAVE-BOLD-FONT)
      (:BOX (20 100 197 25 ))
      (:string "Export Top-level Gadget?")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 20))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 100))))))



;;; This pops up the save dialog box, after determining the default values 
(defun Show-Save-Dialog (&rest args)
  (declare (ignore args))
  (let ((filename *Last-Filename*)
	(package (g-value *objs-agg* :package-name))
	(window-title (g-value *objs-agg* :window-title))
	(export-p (if (g-value *objs-agg* :export-p)
		      "Export Top-level Gadget?"))
	(gadget-name *Top-Gadget-Name*)
	(function-for-ok-name (or (g-value *objs-agg* :FUNCTION-FOR-OK) ""))
	(function-for-ok-invalid (Check-Ask-OK)))  
    (unless (stringp function-for-ok-name)
      (setq function-for-ok-name (write-to-string function-for-ok-name)))
    (set-initial-value save-file :filename filename)
    (set-initial-value save-file :gadget-name gadget-name)
    (set-initial-value save-file :win-title window-title)
    (set-initial-value save-file :package-name package)
    (set-initial-value save-file :FUNCTION-FOR-OK-NAME function-for-ok-name)
    (set-initial-value save-file :export-p export-p)
		       
    (s-value (g-value save-file :function-for-ok-name)
	     :active-p
	     (not function-for-ok-invalid))

    (show-in-window save-file)))

