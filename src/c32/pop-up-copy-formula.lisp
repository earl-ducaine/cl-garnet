;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: C32; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file created by GILT V0.4: The Garnet Interface Builder
;;; on Apr 11, 1991, 5:13 PM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id::                                                     $	


;;; This file uses the following objects:
;;;     TEXT-BUTTON-PANEL from package GARNET-GADGETS
;;;     MULTI-TEXT from package OPAL
;;;     X-BUTTON-PANEL from package GARNET-GADGETS
;;;     LABELED-BOX from package GARNET-GADGETS

;;; CHANGE LOG
;;;
;;; 09/01/92 Andrew Mickish - Removed extra Gilt variables that caused
;;;            compiler warnings.

#||
(dolist (gadget '("labeled-box-loader"
		  "x-buttons-loader"
		  "text-buttons-loader"
		  ))
  (load (common-lisp-user::garnet-pathnames gadget
			 common-lisp-user::Garnet-Gadgets-PathName)))
;;;
;;;     Functions needed from Gilt
(load (common-lisp-user::garnet-pathnames "gilt-functions-loader"
			 common-lisp-user::Garnet-Gilt-PathName))

||#

;;; ***** Pop-Up-Generalize must be loaded first ****
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "C32")


(create-instance 'POP-UP-COPY-FORMULA OPAL:AGGREGADGET
  (:WINDOW-LEFT 300)
  (:WINDOW-TOP 300)
  (:WINDOW-WIDTH 466)
  (:WINDOW-HEIGHT 256)
  (:FUNCTION-FOR-OK `C32::COPY-FORMULA-OK)
  (:EXPORT-P T)
  (:WINDOW-TITLE "Copy Formula")
  (:PACKAGE-NAME "C32")
  (:LEFT 0)
  (:TOP 0)
  (:WIDTH (o-formula (GVL :WINDOW :WIDTH) 466))
  (:from-obj 'R1)
  (:from-slot :left)
  (:to-obj 'r2)
  (:to-slot :top)
  (:parts `(
    (:tit ,OPAL:MULTI-TEXT
      (:GILT-REF "TYPE-TEXT")
      (:FONT ,c32-title-font)
      (:BOX (10 10 438 20 ))
      (:STRING ,(o-formula (concatenate 'simple-string
				"Copying "
				(write-obj (gvl :parent :from-obj))
				"'s "
				(write-obj (gvl :parent :from-slot))
				" to 
        "
				(write-obj (gvl :parent :to-obj))
				"'s "
				(write-obj (gvl :parent :to-slot)))))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 10))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 10)))
   (:slot-list ,opal:aggrelist
      (:BOX (19 70 37 25 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX))))
      (:TOP ,(o-formula (SECOND (GVL :BOX))))
      (:item-prototype ,(create-instance NIL one-line-prompt
                           (:middle-string " converted to:")))
      (:items 1))
    (:okcancel ,GARNET-GADGETS:TEXT-BUTTON-PANEL
      (:SELECTION-FUNCTION GILT:OKCANCEL-FUNCTION)
      (:GILT-REF "TYPE-OKCANCEL")
      (:ITEMS ("OK" "Cancel" ))
      (:GRAY-WIDTH 3)
      (:FINAL-FEEDBACK-P NIL)
      (:TEXT-OFFSET 2)
      (:SHADOW-OFFSET 5)
      (:DIRECTION :HORIZONTAL)
      (:BOX (336 70 117 29 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 336))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 70))))))


(defun Show-copy (from-obj from-slot to-obj to-slot
			   old-slots new-slots func)
  (s-value POP-UP-COPY-FORMULA :from-obj from-obj)
  (s-value POP-UP-COPY-FORMULA :to-obj to-obj)
  (s-value POP-UP-COPY-FORMULA :from-slot from-slot)
  (s-value POP-UP-COPY-FORMULA :to-slot to-slot)
  
  (Set-one-line-prompts (g-value POP-UP-COPY-FORMULA :slot-list)
			old-slots new-slots)
  (s-value POP-UP-COPY-FORMULA :function-for-ok func)
  (let ((h (+ 30 (g-value POP-UP-COPY-FORMULA :height))))
    (s-value POP-UP-COPY-FORMULA :window-height h))
  (gilt:show-in-window POP-UP-COPY-FORMULA))
