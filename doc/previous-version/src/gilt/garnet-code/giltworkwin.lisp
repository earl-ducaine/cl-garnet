;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file created by GILT V3.0: The Garnet Interface Builder
;;; on Sep 16, 1993, 4:25 PM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file uses the following objects:
;;;     X-BUTTON-PANEL from package GARNET-GADGETS
;;;     V-SLIDER from package GARNET-GADGETS
;;;     SCROLLING-LABELED-BOX from package GARNET-GADGETS
;;;     TEXT-BUTTON-PANEL from package GARNET-GADGETS
;;;     LINE from package OPAL
;;;     TEXT from package OPAL
(dolist (gadget '("text-buttons-loader"
		  "scrolling-labeled-box-loader"
		  "v-slider-loader"
		  "x-buttons-loader"
		  ))
  (load (merge-pathnames gadget
			 user::Garnet-Gadgets-PathName)))
;;;
;;;
;;;     Functions needed from Gilt
(dolist (file '("gilt-functions-loader"
		"filter-functions-loader"))
  (load (merge-pathnames file
                         user::Garnet-Gilt-PathName)))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "USER" :use '("LISP" "KR"))

(defparameter user::*Used-Gilt-Version* "V3.0")


(export '(TEMP-GADGET))

(defparameter user::*Garnet-Object-Just-Created* 
(create-instance 'TEMP-GADGET OPAL:AGGREGADGET
  (:FUNCTION-FOR-OK NIL)
  (:PACKAGE-NAME "USER")
  (:LEFT 0)
  (:TOP 0)
  (:HEIGHT (o-formula (GVL :WINDOW :HEIGHT) 391))
  (:WINDOW-LEFT 0)
  (:WINDOW-TOP 0)
  (:WINDOW-WIDTH 450)
  (:WINDOW-HEIGHT 391)
  (:WIDTH (o-formula (GVL :WINDOW :WIDTH) 450))
  (:WINDOW-TITLE "TEMP WINDOW")
  (:EXPORT-P T)
  (:parts `(
    (0 ,OPAL:TEXT
      (:FONT ,(opal:get-standard-font :SERIF :BOLD-ITALIC :VERY-LARGE))
      (:BOX (39 19 3 3 ))
      (:STRING "Properties:")
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 39))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 19)))
    (1 ,OPAL:LINE
      (:Y1 ,(o-formula (SECOND (GVL :POINTS)) 49))
      (:Y2 ,(o-formula (FOURTH (GVL :POINTS)) 49))
      (:POINTS (35 49 150 49 ))
      (:GROW-P T)
      (:CONSTANT T)
      (:LINE-STYLE ,OPAL:LINE-4)
      (:X1 ,(o-formula (FIRST (GVL :POINTS)) 35))
      (:X2 ,(o-formula (THIRD (GVL :POINTS)) 150)))
    (2 ,GARNET-GADGETS:TEXT-BUTTON-PANEL
      (:FINAL-FEEDBACK-P NIL)
      (:BOX (278 19 3 3 ))
      (:GRAY-WIDTH 3)
      (:SELECTION-FUNCTION GILT:OKCANCEL-FUNCTION)
      (:OK-CANCEL-P T)
      (:SHADOW-OFFSET 5)
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 278))
      (:ITEMS ("OK" "Cancel" ))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 19))
      (:DIRECTION :HORIZONTAL)
      (:TEXT-OFFSET 2))
    (3 ,GARNET-GADGETS:SCROLLING-LABELED-BOX
      (:BOX (231 113 190 18 ))
      (:GROW-P T)
      (:MIN-WIDTH 120)
      (:SELECTED T)
      (:VALUE "short string")
      (:CONSTANT T)
      (:LABEL-STRING "Object Name:")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 231))
      (:DO-NOT-DUMP-SLOTS (:POINT-TO-LEAF :SELECT-FUNCTION :BEHAVIORS :PARTS :INTERACTORS :COMPONENTS :PARENT :DEPENDENTS :DEPENDED-SLOTS :IS-A :WINDOW :UPDATE-SLOTS :UPDATE-SLOTS-VALUES :UPDATE-INFO :IS-A-INV :INTERIM-SELECTED :KNOWN-AS :CHILD :GG-SELECTED :GG-INTERIM-SELECTED ))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 113))
      (:DO-NOT-DUMP-OBJECTS :ME)
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 190)))
    (4 ,GARNET-GADGETS:SCROLLING-LABELED-BOX
      (:BOX (231 65 190 18 ))
      (:MIN-WIDTH 120)
      (:GROW-P T)
      (:SELECTED NIL)
      (:VALUE "very long string")
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 231))
      (:LABEL-STRING "Filename:")
      (:DO-NOT-DUMP-SLOTS (:POINT-TO-LEAF :SELECT-FUNCTION :BEHAVIORS :PARTS :INTERACTORS :COMPONENTS :PARENT :DEPENDENTS :DEPENDED-SLOTS :IS-A :WINDOW :UPDATE-SLOTS :UPDATE-SLOTS-VALUES :UPDATE-INFO :IS-A-INV :INTERIM-SELECTED :KNOWN-AS :CHILD :GG-SELECTED :GG-INTERIM-SELECTED ))
      (:DO-NOT-DUMP-OBJECTS :ME)
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 65))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 190)))
    (5 ,OPAL:TEXT
      (:FONT ,(opal:get-standard-font :FIXED :BOLD :MEDIUM))
      (:BOX (230 161 3 3 ))
      (:STRING "Debug Level:")
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 230))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 161)))
    (6 ,GARNET-GADGETS:V-SLIDER
      (:BOX (324 161 39 216 ))
      (:NUM-MARKS 6)
      (:GROW-P T)
      (:CONSTANT T)
      (:VAL-2 10)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 324))
      (:DO-NOT-DUMP-OBJECTS :ME)
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 161))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 216)))
    (7 ,GARNET-GADGETS:X-BUTTON-PANEL
      (:BOX (52 73 92 85 ))
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 52))
      (:ITEMS ("Bold" "Italic" "Underline" ))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 73))))))

)

