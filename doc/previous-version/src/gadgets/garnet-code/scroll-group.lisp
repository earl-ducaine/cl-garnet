;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file created by GILT V1.1: The Garnet Interface Builder
;;; on Oct 14, 1991, 11:51 PM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file uses the following objects:
;;;     MULTI-TEXT from package OPAL
;;;     TRILL-DEVICE from package GARNET-GADGETS
;;;     GAUGE from package GARNET-GADGETS
;;;     V-SLIDER from package GARNET-GADGETS
;;;     V-SCROLL-BAR from package GARNET-GADGETS
;;;     H-SLIDER from package GARNET-GADGETS
;;;     H-SCROLL-BAR from package GARNET-GADGETS
(dolist (gadget '("h-scroll-loader"
		  "h-slider-loader"
		  "v-scroll-loader"
		  "v-slider-loader"
		  "gauge-loader"
		  "trill-device-loader"
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
  (:WINDOW-TOP 19)
  (:WINDOW-LEFT 0)
  (:parts `(
    (NIL ,GARNET-GADGETS:H-SCROLL-BAR
      (:LEFT ,(formula `(FIRST (GVL :BOX ) ) 10))
      (:TOP ,(formula `(SECOND (GVL :BOX ) ) 257))
      (:WIDTH ,(formula `(THIRD (GVL :BOX ) ) 200))
      (:CONSTANT (T ))
      (:GILT-REF "TYPE-H-SCROLL-BAR")
      (:BOX (10 257 200 20 ))
      (:GROW-P T))
    (NIL ,GARNET-GADGETS:H-SLIDER
      (:LEFT ,(formula `(FIRST (GVL :BOX ) ) 10))
      (:TOP ,(formula `(SECOND (GVL :BOX ) ) 290))
      (:WIDTH ,(formula `(THIRD (GVL :BOX ) ) 200))
      (:CONSTANT (T ))
      (:GILT-REF "TYPE-H-SLIDER")
      (:BOX (10 290 200 34 ))
      (:GROW-P T)
      (:VAL-2 10)
      (:NUM-MARKS 6))
    (NIL ,GARNET-GADGETS:V-SCROLL-BAR
      (:LEFT ,(formula `(FIRST (GVL :BOX ) ) 10))
      (:TOP ,(formula `(SECOND (GVL :BOX ) ) 10))
      (:HEIGHT ,(formula `(FOURTH (GVL :BOX ) ) 205))
      (:CONSTANT (T ))
      (:GILT-REF "TYPE-V-SCROLL-BAR")
      (:BOX (10 10 20 205 ))
      (:GROW-P T))
    (NIL ,GARNET-GADGETS:V-SLIDER
      (:LEFT ,(formula `(FIRST (GVL :BOX ) ) 40))
      (:TOP ,(formula `(SECOND (GVL :BOX ) ) 10))
      (:HEIGHT ,(formula `(FOURTH (GVL :BOX ) ) 205))
      (:CONSTANT (T ))
      (:GILT-REF "TYPE-V-SLIDER")
      (:BOX (40 10 39 205 ))
      (:GROW-P T)
      (:VAL-2 10)
      (:NUM-MARKS 6))
    (NIL ,GARNET-GADGETS:GAUGE
      (:LEFT ,(formula `(FIRST (GVL :BOX ) ) 100))
      (:TOP ,(formula `(SECOND (GVL :BOX ) ) 38))
      (:WIDTH ,(formula `(THIRD (GVL :BOX ) ) 130))
      (:CONSTANT (T ))
      (:GILT-REF "TYPE-GAUGE")
      (:BOX (100 38 130 88 ))
      (:INT-FEEDBACK-P NIL)
      (:VAL-1 10)
      (:VAL-2 0)
      (:NUM-MARKS 6)
      (:TITLE "Gauge")
      (:VALUE-FEEDBACK-P NIL)
      (:INT-FEEDBACK-P NIL))
    (NIL ,GARNET-GADGETS:TRILL-DEVICE
      (:LEFT ,(formula `(FIRST (GVL :BOX ) ) 110))
      (:TOP ,(formula `(SECOND (GVL :BOX ) ) 175))
      (:CONSTANT (T ))
      (:GILT-REF "TYPE-TRILL-DEVICE")
      (:BOX (110 175 106 20 )))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 13))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 220))
      (:CONSTANT (T ))
      (:STRING "(a)")
      (:BOX (13 220 14 15 ))
      (:BEHAVIORS NIL)
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)
            (:FAMILY :SERIF)))
      (:GILT-REF "TYPE-TEXT"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 60))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 220))
      (:CONSTANT (T ))
      (:STRING "(b)")
      (:BOX (60 220 14 15 ))
      (:BEHAVIORS NIL)
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)
            (:FAMILY :SERIF)))
      (:GILT-REF "TYPE-TEXT"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 153))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 129))
      (:CONSTANT (T ))
      (:STRING "(c)")
      (:BOX (153 129 14 15 ))
      (:BEHAVIORS NIL)
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)
            (:FAMILY :SERIF)))
      (:GILT-REF "TYPE-TEXT"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 154))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 200))
      (:CONSTANT (T ))
      (:STRING "(d)")
      (:BOX (154 200 14 15 ))
      (:BEHAVIORS NIL)
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)
            (:FAMILY :SERIF)))
      (:GILT-REF "TYPE-TEXT"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 222))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 259))
      (:CONSTANT (T ))
      (:STRING "(e)")
      (:BOX (222 259 14 15 ))
      (:BEHAVIORS NIL)
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)
            (:FAMILY :SERIF)))
      (:GILT-REF "TYPE-TEXT"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 222))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 293))
      (:CONSTANT (T ))
      (:STRING "(f)")
      (:BOX (222 293 14 15 ))
      (:BEHAVIORS NIL)
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)
            (:FAMILY :SERIF)))
      (:GILT-REF "TYPE-TEXT")))))

)

