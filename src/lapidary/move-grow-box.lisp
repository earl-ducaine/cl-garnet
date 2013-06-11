;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file created by GILT V0.4: The Garnet Interface Builder
;;; on Jun 7, 1991, 4:22 PM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "LAPIDARY")

;;; This file uses the following objects:
;;;     MULTI-TEXT from package OPAL
;;;     TEXT-BUTTON-PANEL from package GARNET-GADGETS
;;;     X-BUTTON-PANEL from package GARNET-GADGETS
(dolist (gadget '("x-buttons-loader"
		  "text-buttons-loader"
		  ))
  (load (merge-pathnames gadget common-lisp-user::Garnet-Gadgets-PathName)))
;;;
;;;     Functions needed from Gilt
(common-lisp-user::garnet-load "gilt:gilt-functions-loader")

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (eval load compile)
  (export '(BOX-PARAMS)))

(setf common-lisp-user::*Garnet-Object-Just-Created* 
(create-instance 'BOX-PARAMS OPAL:AGGREGADGET
  (:WINDOW-LEFT 0)
  (:WINDOW-TOP 0)
  (:WINDOW-WIDTH 402)
  (:WINDOW-HEIGHT 163)
  (:FUNCTION-FOR-OK `LAPIDARY::SET-BOX-SLOTS)
  (:EXPORT-P T)
  (:WINDOW-TITLE "Box Window")
  (:PACKAGE-NAME "LAPIDARY")
  (:LEFT 0)
  (:TOP 0)
  (:WIDTH (o-formula (GVL :WINDOW :WIDTH) 450))
  (:HEIGHT (o-formula (GVL :WINDOW :HEIGHT) 300))
  (:parts `(
    (:box-slots ,GARNET-GADGETS:X-BUTTON-PANEL
      (:GILT-REF "TYPE-X-BUTTON-PANEL")
      (:INDENT 0)
      (:H-SPACING 5)
      (:GRAY-WIDTH 3)
      (:TEXT-ON-LEFT-P T)
      (:V-SPACING 5)
      (:BUTTON-HEIGHT 20)
      (:FIXED-HEIGHT-P NIL)
      (:DIRECTION :HORIZONTAL)
      (:PIXEL-MARGIN NIL)
      (:RANK-MARGIN NIL)
      (:TEXT-OFFSET 5)
      (:SHADOW-OFFSET 5)
      (:BUTTON-WIDTH 20)
      (:FONT ,OPAL:DEFAULT-FONT)
      (:FIXED-WIDTH-P T)
      (:BOX (10 40 128 98 ))
      (:ITEMS (:LEFT :TOP :WIDTH :HEIGHT ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 10))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 40)))
    (NIL ,GARNET-GADGETS:TEXT-BUTTON-PANEL
      (:SELECTION-FUNCTION GILT:OKCANCEL-FUNCTION)
      (:GILT-REF "TYPE-OKCANCEL")
      (:ITEMS ("OK" "Cancel" ))
      (:GRAY-WIDTH 3)
      (:FINAL-FEEDBACK-P NIL)
      (:TEXT-OFFSET 2)
      (:SHADOW-OFFSET 5)
      (:DIRECTION :HORIZONTAL)
      (:BOX (104 90 127 46 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 104))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 90)))
    (NIL ,OPAL:MULTI-TEXT
      (:GILT-REF "TYPE-TEXT")
      (:BOX (10 10 3 3 ))
      (:STRING "Slots that should derive their value from the box slot:")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 10))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 10))))))

)

