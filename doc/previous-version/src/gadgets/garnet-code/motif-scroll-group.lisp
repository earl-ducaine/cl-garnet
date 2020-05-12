;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file created by GILT V1.1: The Garnet Interface Builder
;;; on Oct 15, 1991, 0:05 AM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file uses the following objects:
;;;     MULTI-TEXT from package OPAL
;;;     MOTIF-GAUGE from package GARNET-GADGETS
;;;     MOTIF-SLIDER from package GARNET-GADGETS
;;;     MOTIF-V-SCROLL-BAR from package GARNET-GADGETS
;;;     MOTIF-H-SCROLL-BAR from package GARNET-GADGETS
;;;     MOTIF-TRILL-DEVICE from package GARNET-GADGETS
;;;     MOTIF-BACKGROUND from package GARNET-GADGETS
(dolist (gadget '("motif-h-scroll-loader"
		  "motif-v-scroll-loader"
		  "motif-slider-loader"
		  "motif-gauge-loader"
		  "motif-trill-device-loader"
		  ))
  (load (merge-pathnames gadget
			 user::Garnet-Gadgets-PathName)))
;;;
;;;     Functions needed from Gilt
(load (merge-pathnames "gilt-functions-loader"
			 user::Garnet-Gilt-PathName))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "USER" :use '("LISP" "KR"))

(defparameter user::*Used-Gilt-Version* "V1.1")


(export '(TEMP-GADGET))

(defparameter user::*Garnet-Object-Just-Created* 
(create-instance 'TEMP-GADGET OPAL:AGGREGADGET
  (:LEFT 0)
  (:TOP 0)
  (:WIDTH (o-formula (GVL :WINDOW :WIDTH) 253))
  (:HEIGHT (o-formula (GVL :WINDOW :HEIGHT) 342))
  (:FUNCTION-FOR-OK NIL)
  (:EXPORT-P T)
  (:WINDOW-TITLE "TEMP WINDOW")
  (:PACKAGE-NAME "USER")
  (:WINDOW-HEIGHT 342)
  (:WINDOW-WIDTH 253)
  (:WINDOW-TOP 40)
  (:WINDOW-LEFT 750)
  (:parts `(
    (NIL ,GARNET-GADGETS:MOTIF-BACKGROUND
      (:LEFT 0)
      (:TOP 0)
      (:CONSTANT (T ))
      (:SELECT-OUTLINE-ONLY T)
      (:FOREGROUND-COLOR ,OPAL:MOTIF-LIGHT-GRAY)
      (:BOX (184 317 3 3 ))
      (:GILT-REF "TYPE-MOTIF-BACKGROUND"))
    (NIL ,GARNET-GADGETS:MOTIF-H-SCROLL-BAR
      (:LEFT ,(formula `(FIRST (GVL :BOX ) ) 10))
      (:TOP ,(formula `(SECOND (GVL :BOX ) ) 257))
      (:WIDTH ,(formula `(THIRD (GVL :BOX ) ) 200))
      (:FOREGROUND-COLOR ,OPAL:MOTIF-LIGHT-GRAY)
      (:CONSTANT (T ))
      (:GROW-P T)
      (:BOX (10 257 200 20 ))
      (:GILT-REF "TYPE-MOTIF-H-SCROLL-BAR"))
    (NIL ,GARNET-GADGETS:MOTIF-V-SCROLL-BAR
      (:LEFT ,(formula `(FIRST (GVL :BOX ) ) 10))
      (:TOP ,(formula `(SECOND (GVL :BOX ) ) 10))
      (:HEIGHT ,(formula `(FOURTH (GVL :BOX ) ) 205))
      (:FOREGROUND-COLOR ,OPAL:MOTIF-LIGHT-GRAY)
      (:CONSTANT (T ))
      (:GROW-P T)
      (:BOX (10 10 20 205 ))
      (:GILT-REF "TYPE-MOTIF-V-SCROLL-BAR"))
    (NIL ,GARNET-GADGETS:MOTIF-SLIDER
      (:LEFT ,(formula `(FIRST (GVL :BOX ) ) 40))
      (:TOP ,(formula `(SECOND (GVL :BOX ) ) 10))
      (:HEIGHT ,(formula `(FOURTH (GVL :BOX ) ) 205))
      (:FOREGROUND-COLOR ,OPAL:MOTIF-LIGHT-GRAY)
      (:CONSTANT (T ))
      (:NUM-MARKS 6)
      (:VAL-2 10)
      (:GROW-P T)
      (:BOX (40 10 39 205 ))
      (:GILT-REF "TYPE-MOTIF-SLIDER"))
    (NIL ,GARNET-GADGETS:MOTIF-GAUGE
      (:LEFT ,(formula `(FIRST (GVL :BOX ) ) 100))
      (:TOP ,(formula `(SECOND (GVL :BOX ) ) 38))
      (:WIDTH ,(formula `(THIRD (GVL :BOX ) ) 130))
      (:FOREGROUND-COLOR ,OPAL:MOTIF-LIGHT-GRAY)
      (:CONSTANT (T ))
      (:INT-FEEDBACK-P NIL)
      (:VALUE-FEEDBACK-P NIL)
      (:TITLE "Gauge")
      (:NUM-MARKS 6)
      (:VAL-2 0)
      (:VAL-1 10)
      (:INT-FEEDBACK-P NIL)
      (:BOX (100 38 130 88 ))
      (:GILT-REF "TYPE-MOTIF-GAUGE"))
    (NIL ,GG:MOTIF-TRILL-DEVICE
     (:left 122) (:top 175)
     (:FOREGROUND-COLOR ,OPAL:MOTIF-LIGHT-GRAY)
     )
    (NIL ,opal:multi-text
     (:left 154) (:top 200)
     (:string "(j)")
     (:font ,(opal:get-standard-font :serif :bold NIL)))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(formula `(FIRST (GVL :BOX ) ) 13))
      (:TOP ,(formula `(SECOND (GVL :BOX ) ) 220))
      (:CONSTANT (T ))
      (:GILT-REF "TYPE-TEXT")
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)
            (:FAMILY :SERIF)))
      (:BEHAVIORS NIL)
      (:BOX (13 220 14 15 ))
      (:STRING "(g)"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(formula `(FIRST (GVL :BOX ) ) 62))
      (:TOP ,(formula `(SECOND (GVL :BOX ) ) 220))
      (:CONSTANT (T ))
      (:GILT-REF "TYPE-TEXT")
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)
            (:FAMILY :SERIF)))
      (:BEHAVIORS NIL)
      (:BOX (62 220 14 15 ))
      (:STRING "(h)"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(formula `(FIRST (GVL :BOX ) ) 153))
      (:TOP ,(formula `(SECOND (GVL :BOX ) ) 129))
      (:CONSTANT (T ))
      (:GILT-REF "TYPE-TEXT")
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)
            (:FAMILY :SERIF)))
      (:BEHAVIORS NIL)
      (:BOX (153 129 14 15 ))
      (:STRING "(i)"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(formula `(FIRST (GVL :BOX ) ) 222))
      (:TOP ,(formula `(SECOND (GVL :BOX ) ) 259))
      (:CONSTANT (T ))
      (:GILT-REF "TYPE-TEXT")
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)
            (:FAMILY :SERIF)))
      (:BEHAVIORS NIL)
      (:BOX (222 259 14 15 ))
      (:STRING "(k)")))))

)

