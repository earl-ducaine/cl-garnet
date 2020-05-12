;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file created by GILT V3.0: The Garnet Interface Builder
;;; on Jul 14, 1993, 5:42 PM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file uses the following objects:
;;;     MULTIFONT-TEXT from package OPAL
;;;     MOTIF-OPTION-BUTTON from package GARNET-GADGETS
;;;     MOTIF-RECT from package GARNET-GADGETS
;;;     RECTANGLE from package OPAL
;;;     BITMAP from package OPAL
;;;     PIXMAP from package OPAL
;;;     MOTIF-SCROLLING-LABELED-BOX from package GARNET-GADGETS
;;;     MOTIF-GAUGE from package GARNET-GADGETS
;;;     MOTIF-TRILL-DEVICE from package GARNET-GADGETS
;;;     MOTIF-H-SCROLL-BAR from package GARNET-GADGETS
;;;     MOTIF-SLIDER from package GARNET-GADGETS
;;;     MOTIF-V-SCROLL-BAR from package GARNET-GADGETS
;;;     MOTIF-RADIO-BUTTON-PANEL from package GARNET-GADGETS
;;;     MOTIF-CHECK-BUTTON-PANEL from package GARNET-GADGETS
;;;     MOTIF-SCROLLING-MENU from package GARNET-GADGETS
;;;     MOTIF-MENU from package GARNET-GADGETS
;;;     MOTIF-TEXT-BUTTON-PANEL from package GARNET-GADGETS
;;;     TEXT from package OPAL
;;;     LINE from package OPAL
;;;     MOTIF-MENUBAR from package GARNET-GADGETS
;;;     MOTIF-BACKGROUND from package GARNET-GADGETS
(dolist (gadget '("motif-menubar-loader"
		  "motif-text-buttons-loader"
		  "motif-menu-loader"
		  "motif-scrolling-menu-loader"
		  "motif-check-buttons-loader"
		  "motif-radio-buttons-loader"
		  "motif-v-scroll-loader"
		  "motif-slider-loader"
		  "motif-h-scroll-loader"
		  "motif-trill-device-loader"
		  "motif-gauge-loader"
		  "motif-scrolling-labeled-box-loader"
		  "motif-option-button-loader"
		  ))
  (load (merge-pathnames gadget
			 user::Garnet-Gadgets-PathName)))

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
  (:HEIGHT (o-formula (GVL :WINDOW :HEIGHT) 380))
  (:WINDOW-LEFT 1)
  (:WINDOW-TOP 74)
  (:WINDOW-WIDTH 465)
  (:WINDOW-HEIGHT 380)
  (:WIDTH (o-formula (GVL :WINDOW :WIDTH) 465))
  (:WINDOW-TITLE "Gilt Motif Gadgets")
  (:EXPORT-P T)
  (:parts `(
    (0 ,GARNET-GADGETS:MOTIF-BACKGROUND
      (:FOREGROUND-COLOR ,OPAL:MOTIF-LIGHT-GRAY)
      (:BOX (426 231 3 3 ))
      (:SELECT-OUTLINE-ONLY T)
      (:CONSTANT T)
      (:LEFT 0)
      (:TOP 0))
    (1 ,GARNET-GADGETS:MOTIF-MENUBAR
      (:FOREGROUND-COLOR ,OPAL:MOTIF-LIGHT-GRAY)
      (:BOX (19 30 3 3 ))
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 19))
      (:ITEMS (("File" NIL (("Open..." ) ("New" ) ("Close" ) ("Print" ) ) ) ("Edit" NIL (("Cut" ) ("Copy" ) ("Paste" ) ("Delete" ) ) ) ("Other" NIL (("sub-label1" ) ("sub-label2" ) ) ) ))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 30)))
    (2 ,OPAL:LINE
      (:Y1 ,(o-formula (SECOND (GVL :POINTS)) 15))
      (:Y2 ,(o-formula (FOURTH (GVL :POINTS)) 15))
      (:POINTS (5 15 443 15 ))
      (:GROW-P T)
      (:CONSTANT T)
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (GVL :POINTS)) 5))
      (:X2 ,(o-formula (THIRD (GVL :POINTS)) 443)))
    (3 ,OPAL:TEXT
      (:FONT ,(opal:get-standard-font :SERIF :ITALIC :LARGE))
      (:FILL-BACKGROUND-P T)
      (:BOX (178 4 105 22 ))
      (:STRING "Motif Gadgets")
      (:CONSTANT T)
      (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
            (:BACKGROUND-COLOR OPAL:MOTIF-LIGHT-GRAY)))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 178))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 4)))
    (4 ,GARNET-GADGETS:MOTIF-TEXT-BUTTON-PANEL
      (:FOREGROUND-COLOR ,OPAL:MOTIF-LIGHT-GRAY)
      (:BOX (19 72 3 3 ))
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 19))
      (:ITEMS ("Label1" "Label2" "Label3" ))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 72)))
    (5 ,GARNET-GADGETS:MOTIF-MENU
      (:FOREGROUND-COLOR ,OPAL:MOTIF-LIGHT-GRAY)
      (:BOX (19 168 66 70 ))
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 19))
      (:ITEMS ("Label1" "Label2" "Label3" ))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 168)))
    (6 ,GARNET-GADGETS:MOTIF-SCROLLING-MENU
      (:FOREGROUND-COLOR ,OPAL:MOTIF-LIGHT-GRAY)
      (:BOX (19 252 3 3 ))
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 19))
      (:DO-NOT-DUMP-OBJECTS :ME)
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 252)))
    (7 ,GARNET-GADGETS:MOTIF-CHECK-BUTTON-PANEL
      (:FOREGROUND-COLOR ,OPAL:MOTIF-LIGHT-GRAY)
      (:BOX (102 76 66 64 ))
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 102))
      (:ITEMS ("Label1" "Label2" "Label3" ))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 76)))
    (8 ,GARNET-GADGETS:MOTIF-RADIO-BUTTON-PANEL
      (:FOREGROUND-COLOR ,OPAL:MOTIF-LIGHT-GRAY)
      (:BOX (102 166 3 3 ))
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 102))
      (:ITEMS ("Label1" "Label2" "Label3" ))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 166)))
    (9 ,GARNET-GADGETS:MOTIF-V-SCROLL-BAR
      (:FOREGROUND-COLOR ,OPAL:MOTIF-LIGHT-GRAY)
      (:BOX (182 48 20 201 ))
      (:MIN-HEIGHT 40)
      (:GROW-P T)
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 182))
      (:DO-NOT-DUMP-OBJECTS :ME)
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 48))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 201)))
    (10 ,GARNET-GADGETS:MOTIF-SLIDER
      (:FOREGROUND-COLOR ,OPAL:MOTIF-LIGHT-GRAY)
      (:BOX (206 48 46 203 ))
      (:MIN-HEIGHT 40)
      (:GROW-P T)
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 206))
      (:DO-NOT-DUMP-OBJECTS :ME)
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 48))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 203)))
    (11 ,GARNET-GADGETS:MOTIF-H-SCROLL-BAR
      (:FOREGROUND-COLOR ,OPAL:MOTIF-LIGHT-GRAY)
      (:BOX (267 37 193 20 ))
      (:MIN-WIDTH 40)
      (:GROW-P T)
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 267))
      (:DO-NOT-DUMP-OBJECTS :ME)
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 37))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 193)))
    (12 ,GARNET-GADGETS:MOTIF-TRILL-DEVICE
      (:FOREGROUND-COLOR ,OPAL:MOTIF-LIGHT-GRAY)
      (:BOX (267 67 77 25 ))
      (:GROW-P T)
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 267))
      (:DO-NOT-DUMP-OBJECTS :ME)
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 67))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 25))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 77)))
    (13 ,GARNET-GADGETS:MOTIF-GAUGE
      (:FOREGROUND-COLOR ,OPAL:MOTIF-LIGHT-GRAY)
      (:BOX (350 68 113 83 ))
      (:NUM-MARKS 6)
      (:GROW-P T)
      (:CONSTANT T)
      (:VAL-1 10)
      (:VAL-2 0)
      (:VALUE-FEEDBACK-P NIL)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 350))
      (:INT-FEEDBACK-P NIL)
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 68))
      (:TITLE "Title")
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 113)))
    (14 ,GARNET-GADGETS:MOTIF-SCROLLING-LABELED-BOX
      (:FOREGROUND-COLOR ,OPAL:MOTIF-LIGHT-GRAY)
      (:FIELD-STRING "Scrolling Text Box")
      (:BOX (269 154 184 31 ))
      (:GROW-P T)
      (:MIN-WIDTH 100)
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 269))
      (:LABEL-STRING "Title:")
      (:DO-NOT-DUMP-OBJECTS :ME)
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 154))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 184)))
    (15 ,OPAL:PIXMAP
      (:BOX (265 187 3 3 ))
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 265))
      (:IMAGE ,(o-formula (OPAL:READ-XPM-FILE (GVL :IMAGE-NAME))))
      (:IMAGE-NAME "/afs/cs.cmu.edu/project/garnet/test/lib/pixmaps/garnetlogo.xpm")
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 187)))
    (16 ,OPAL:BITMAP
      (:BOX (349 188 3 3 ))
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 349))
      (:IMAGE ,(o-formula (OPAL:READ-IMAGE (GVL :IMAGE-NAME))))
      (:IMAGE-NAME "/afs/cs.cmu.edu/project/garnet/test/lib/gilt/giltbitmap.bitmap")
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 188)))
    (17 ,OPAL:LINE
      (:Y1 ,(o-formula (SECOND (GVL :POINTS)) 189))
      (:Y2 ,(o-formula (FOURTH (GVL :POINTS)) 212))
      (:POINTS (402 189 435 212 ))
      (:GROW-P T)
      (:CONSTANT T)
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (GVL :POINTS)) 402))
      (:X2 ,(o-formula (THIRD (GVL :POINTS)) 435)))
    (18 ,OPAL:RECTANGLE
      (:BOX (348 228 46 35 ))
      (:GROW-P T)
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 348))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 228))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 35))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 46)))
    (19 ,GARNET-GADGETS:MOTIF-RECT
      (:FOREGROUND-COLOR ,OPAL:MOTIF-LIGHT-GRAY)
      (:BOX (404 225 43 39 ))
      (:GROW-P T)
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 404))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 225))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 39))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 43)))
    (20 ,GARNET-GADGETS:MOTIF-TEXT-BUTTON-PANEL
      (:FOREGROUND-COLOR ,OPAL:MOTIF-LIGHT-GRAY)
      (:FINAL-FEEDBACK-P NIL)
      (:BOX (127 265 3 3 ))
      (:SELECTION-FUNCTION GILT:OKCANCEL-FUNCTION)
      (:OK-CANCEL-P T)
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 127))
      (:ITEMS ("OK" "Cancel" ))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 265))
      (:DIRECTION :HORIZONTAL)
      (:TEXT-OFFSET 5))
    (21 ,GARNET-GADGETS:MOTIF-TEXT-BUTTON-PANEL
      (:FOREGROUND-COLOR ,OPAL:MOTIF-LIGHT-GRAY)
      (:FINAL-FEEDBACK-P NIL)
      (:BOX (127 298 3 3 ))
      (:SELECTION-FUNCTION GILT:OKCANCEL-FUNCTION)
      (:OK-CANCEL-P T)
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 127))
      (:ITEMS ("OK" "Apply" "Cancel" ))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 298))
      (:DIRECTION :HORIZONTAL)
      (:TEXT-OFFSET 5))
    (22 ,GARNET-GADGETS:MOTIF-OPTION-BUTTON
      (:FOREGROUND-COLOR ,OPAL:MOTIF-LIGHT-GRAY)
      (:BOX (134 338 3 3 ))
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 134))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 338)))
    (23 ,OPAL:TEXT
      (:BOX (323 288 28 14 ))
      (:STRING "Text")
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 323))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 288)))
    (24 ,OPAL:MULTIFONT-TEXT
      (:BOX (370 279 101 34 ))
      (:SELECTION-P NIL)
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 370))
      (:INITIAL-TEXT ((("Multi" . ,OPAL:DEFAULT-FONT) ("Font," . ,(opal:get-standard-font :FIXED :ROMAN :LARGE)) ) (("multi-" . ,(opal:get-standard-font :SERIF :ROMAN :MEDIUM)) ("line " . ,(opal:get-standard-font :SERIF :ITALIC :MEDIUM)) ("text" . ,(opal:get-standard-font :SERIF :BOLD :MEDIUM)) ("" . ,OPAL:DEFAULT-FONT) ) ))
      (:line-style ,(create-instance 'L opal:line-style
		      (:background-color opal:motif-light-gray)))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 279)))
    (25 ,OPAL:TEXT
      (:FONT ,(opal:get-standard-font :FIXED :BOLD :MEDIUM))
      (:BOX (326 331 113 14 ))
      (:STRING "Motif-Background")
      (:CONSTANT T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 326))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 331))))))

)

