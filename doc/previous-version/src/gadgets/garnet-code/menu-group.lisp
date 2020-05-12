;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-

(in-package "USER" :use '("LISP" "KR"))

(defvar MENU-GROUP-INIT
(dolist (gadget '("menu-loader"
		  "scrolling-menu-loader"
		  "text-buttons-loader"
		  "x-buttons-loader"
		  "radio-buttons-loader"
		  "menubar-loader"
		  "option-button-loader"
		  "popup-menu-button-loader"
		  ))
  (load (merge-pathnames gadget
			 user::Garnet-Gadgets-PathName))))

(when (boundp 'MENU-WIN) (opal:destroy MENU-WIN))
(setf kr::*constants-disabled* T)

(create-instance 'MENU-AGG OPAL:AGGREGADGET
  (:LEFT 0)
  (:TOP 0)
  (:FUNCTION-FOR-OK NIL)
  (:EXPORT-P T)
  (:WINDOW-TITLE "TEMP WINDOW")
  (:PACKAGE-NAME "USER")
  (:parts `(
    (NIL ,GARNET-GADGETS:MENU
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 271))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 28))
      (:CONSTANT (T ))
      (:TITLE "Title")
      (:ITEMS ("Label1" "Label2" "Label3" ))
      (:BOX (130 10 54 100 ))
      (:GILT-REF "TYPE-MENU"))
    (NIL ,GARNET-GADGETS:SCROLLING-MENU
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 342))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 12))
      (:num-visible 4)
      (:page-trill-p NIL)
      (:CONSTANT (T ))
      (:TITLE "Title")
      (:ITEMS ("Label1" "Label2" "Label3" "Label4" "Label5" "Label6" "Label7" "Label8" ))
      (:BOX (190 5 70 127 ))
      (:GILT-REF "TYPE-SCROLLING-MENU"))
    (NIL ,GARNET-GADGETS:TEXT-BUTTON-PANEL
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 10))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 113))
      (:CONSTANT (T ))
      (:ITEMS ("Label1" "Label2" "Label3" ))
      (:BOX (10 140 223 44 ))
      (:FINAL-FEEDBACK-P T)
      (:H-SPACING 5)
      (:V-SPACING 5)
      (:INDENT 0)
      (:DIRECTION :HORIZONTAL)
      (:FIXED-HEIGHT-P T)
      (:GRAY-WIDTH 5)
      (:TEXT-OFFSET 5)
      (:FONT ,OPAL:DEFAULT-FONT)
      (:SHADOW-OFFSET 10)
      (:FIXED-WIDTH-P T)
      (:RANK-MARGIN NIL)
      (:PIXEL-MARGIN NIL)
      (:H-ALIGN :CENTER)
      (:GILT-REF "TYPE-TEXT-BUTTON-PANEL"))
    (NIL ,GARNET-GADGETS:X-BUTTON-PANEL
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 29))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 170))
      (:CONSTANT (T ))
      (:ITEMS ("Label1" "Label2" "Label3" ))
      (:BOX (20 200 71 85 ))
      (:GILT-REF "TYPE-X-BUTTON-PANEL"))
    (NIL ,GARNET-GADGETS:RADIO-BUTTON-PANEL
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 147))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 170))
      (:CONSTANT (T ))
      (:ITEMS ("Label1" "Label2" "Label3" ))
      (:BOX (147 193 74 94 ))
      (:GILT-REF "TYPE-RADIO-BUTTON-PANEL"))
    (NIL ,GG:OPTION-BUTTON
     (:left 50) (:top 310)
     (:label "Color:")
     (:items ,(list "Red" "Blue" "Green"))
     ))))

(create-instance 'MENU-WIN inter:interactor-window
  (:left 700) (:top 10)
  (:width 270) (:height 350)
  (:aggregate MENU-AGG))
(opal:update MENU-WIN)


(create-instance 'DEMO-MENUBAR gg:MENUBAR
  (:items
   '((:family NIL
      ((:fixed)(:serif)(:sans-serif)))
     (:face NIL
      ((:roman)(:bold)(:italic)(:bold-italic)))
     (:size NIL
      ((:small)(:medium)(:large)(:very-large))))))
(opal:add-component MENU-AGG DEMO-MENUBAR)
(opal:notice-items-changed DEMO-MENUBAR)
(opal:update MENU-WIN)


(create-instance 'PMB gg:popup-menu-button
  (:left 25) (:top 100)
  )
(opal:add-component MENU-AGG PMB)
(opal:update MENU-WIN)

(create-instance 'MENU-A opal:text
  (:left 88) (:top 43)
  (:string "(a)")
  (:font (opal:get-standard-font NIL :bold NIL)))
(create-instance 'MENU-B opal:text
  (:left 60) (:top 102)
  (:string "(b)")
  (:font (opal:get-standard-font NIL :bold NIL)))
(create-instance 'MENU-C opal:text
  (:left 148) (:top 117)
  (:string "(c)")
  (:font (opal:get-standard-font NIL :bold NIL)))
(create-instance 'MENU-D opal:text
  (:left 219) (:top 117)
  (:string "(d)")
  (:font (opal:get-standard-font NIL :bold NIL)))
(create-instance 'MENU-E opal:text
  (:left 241) (:top 155)
  (:string "(e)")
  (:font (opal:get-standard-font NIL :bold NIL)))
(create-instance 'MENU-F opal:text
  (:left 101) (:top 220)
  (:string "(f)")
  (:font (opal:get-standard-font NIL :bold NIL)))
(create-instance 'MENU-G opal:text
  (:left 229) (:top 214)
  (:string "(g)")
  (:font (opal:get-standard-font NIL :bold NIL)))
(create-instance 'MENU-H opal:text
  (:left 160) (:top 312)
  (:string "(h)")
  (:font (opal:get-standard-font NIL :bold NIL)))

(opal:add-components MENU-AGG MENU-A MENU-B MENU-C MENU-D MENU-E MENU-F
		     MENU-G MENU-H)
(opal:update MENU-WIN)

(create-instance 'I inter:move-grow-interactor
  (:objs (list MENU-A MENU-B MENU-C MENU-D MENU-E MENU-F MENU-G MENU-H))
  (:start-where (o-formula (list :list-element-of (gv :self) :objs)))
  (:window MENU-WIN)
  (:slots-to-set (list T T NIL NIL)))


(setf kr::*constants-disabled* NIL)