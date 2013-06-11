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
;;; on May 23, 1991, 12:38 PM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file uses the following objects:
;;;     TEXT-BUTTON-PANEL from package GARNET-GADGETS
;;;     SCROLLING-LABELED-BOX from package GARNET-GADGETS
;;;     MULTI-TEXT from package OPAL

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "LAPIDARY")

(eval-when (eval load compile)
  (export '(PROMPT-GADGET)))

(setf common-lisp-user::*Garnet-Object-Just-Created* 
(create-instance 'PROMPT-GADGET OPAL:AGGREGADGET
  (:WINDOW-LEFT 0)
  (:WINDOW-TOP 0)
  (:WINDOW-WIDTH (o-formula (+ 20 (gvl :width))))
  (:WINDOW-HEIGHT (o-formula (+ 20 (gvl :height))))
  (:FUNCTION-FOR-OK NIL)
  (:EXPORT-P T)
  (:WINDOW-TITLE "Prompt-Win")
  (:PACKAGE-NAME "LAPIDARY")
  (:LEFT 0)
  (:TOP 0)
  (:string "Please Enter Type Restriction:")
  (:parts `(
    (:prompt ,OPAL:MULTI-TEXT
      (:GILT-REF "TYPE-TEXT")
      (:BOX (10 7 4 3 ))
      (:STRING ,(o-formula (gvl :parent :string)))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 10))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 7)))
    (:result ,GARNET-GADGETS:SCROLLING-LABELED-BOX
      (:GILT-REF "TYPE-SCROLLING-LABELED-BOX")
      (:BOX (6 57 315 22 ))
      (:MIN-WIDTH 20)
      (:GROW-P T)
      (:LABEL-STRING "")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 6))
      (:TOP ,(o-formula (+ (opal:gv-bottom (gvl :parent :prompt)) 15)))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 315)))
    (NIL ,GARNET-GADGETS:TEXT-BUTTON-PANEL
      (:SELECTION-FUNCTION gilt:OKCANCEL-FUNCTION)
      (:GILT-REF "TYPE-OKCANCEL")
      (:ITEMS ("OK" "Cancel" ))
      (:GRAY-WIDTH 3)
      (:FINAL-FEEDBACK-P NIL)
      (:TEXT-OFFSET 2)
      (:SHADOW-OFFSET 5)
      (:DIRECTION :HORIZONTAL)
      (:BOX (106 100 67 17 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 106))
      (:TOP ,(o-formula (+ (opal:gv-bottom (gvl :parent :result)) 15)))))))

)

