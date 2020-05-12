;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-

(in-package "USER" :use '("LISP" "KR"))

(defvar MOTIF-MENU-GROUP-INIT
(dolist (gadget '("motif-menu-loader"
		  "motif-scrolling-menu-loader"
		  "motif-text-buttons-loader"
		  "motif-check-buttons-loader"
		  "motif-radio-buttons-loader"
		  "motif-option-button-loader"
		  "motif-menubar-loader"
		  ))
  (load (merge-pathnames gadget
			 user::Garnet-Gadgets-PathName))))

(when (boundp 'MOTIF-MENU-WIN) (opal:destroy MOTIF-MENU-WIN))

(setf kr::*constants-disabled* T)
(create-instance 'MOTIF-MENU-AGG OPAL:AGGREGADGET
  (:LEFT 0)
  (:TOP 0)
  (:FUNCTION-FOR-OK NIL)
  (:EXPORT-P T)
  (:WINDOW-TITLE "TEMP WINDOW")
  (:PACKAGE-NAME "USER")
  (:parts `(
    (NIL ,GARNET-GADGETS:MOTIF-MENU
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 27))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 23))
      (:foreground-color ,opal:motif-light-gray)
      (:ITEMS ("Label1" "Label2" "Label3" "Label4"))
      (:BOX (10 50 3 3 ))
      (:GILT-REF "TYPE-MOTIF-MENU"))
    (NIL ,GARNET-GADGETS:MOTIF-SCROLLING-MENU
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 122))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 25))
      (:foreground-color ,opal:motif-light-gray)
      (:num-visible 4)
      (:ITEMS ("Label1" "Label2" "Label3" "Label4"
			"Label5" "Label6" "Label7"
			"Label8"))
      (:BOX (100 50 3 3 ))
      (:GILT-REF "TYPE-MOTIF-MENU"))
    (:a ,GARNET-GADGETS:MOTIF-TEXT-BUTTON-PANEL
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 22))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 114))
      (:ITEMS ("Label1" "Label2" "Label3" ))
      (:foreground-color ,opal:motif-light-gray)
      (:BOX (200 55 3 3 ))
      (:GILT-REF "TYPE-MOTIF-TEXT-BUTTON-PANEL"))
    (:b ,GARNET-GADGETS:MOTIF-CHECK-BUTTON-PANEL
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 24))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 145))
      (:foreground-color ,opal:motif-light-gray)
      (:ITEMS ("Label1" "Label2" "Label3" ))
      (:BOX (15 185 208 18 ))
      (:DIRECTION :HORIZONTAL)
      (:GILT-REF "TYPE-MOTIF-CHECK-BUTTON-PANEL"))
    (:c ,GARNET-GADGETS:MOTIF-RADIO-BUTTON-PANEL
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 22))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 182))
      (:foreground-color ,opal:motif-light-gray)
      (:ITEMS ("Label1" "Label2" "Label3" ))
      (:BOX (15 212 208 18 ))
      (:DIRECTION :HORIZONTAL)
      (:GILT-REF "TYPE-MOTIF-RADIO-BUTTON-PANEL")))))

(create-instance 'MOTIF-MENU-WIN inter:interactor-window
  (:left 700) (:top 10)
  (:width 270) (:height 350)
  (:border-width 0)
  (:background-color opal:motif-light-gray)
  (:aggregate MOTIF-MENU-AGG))
(opal:update MOTIF-MENU-WIN)


(create-instance 'MOB1 gg:motif-option-button
  (:left 10) (:top 290)
  (:foreground-color opal:motif-light-gray)
  (:items '("Red" "Blue" "Green"))
  (:initial-item "Blue")
  (:label "Color:"))
(create-instance 'MOB2 gg:motif-option-button
  (:left 150) (:top 290)
  (:foreground-color opal:motif-light-gray)
  (:items '("Red" "Blue" "Green"))
  (:initial-item "Blue")
  (:label "Color:"))
(opal:add-components MOTIF-MENU-AGG MOB1 MOB2)
(opal:update MOTIF-MENU-WIN)


(create-instance 'MMBAR gg:MOTIF-MENUBAR
  (:min-menubar-width 270)
  (:foreground-color opal:motif-light-gray)
  (:items
   '((:family NIL
      ((:fixed)(:serif)(:sans-serif)))
     (:face NIL
      ((:roman)(:bold)(:italic)(:bold-italic)))
     (:size NIL
      ((:small)(:medium)(:large)(:very-large))))))
(opal:add-components MOTIF-MENU-AGG MMBAR)
(opal:update MOTIF-MENU-WIN)


(create-instance 'MOTIF-MENU-I opal:text
  (:left 241) (:top 7)
  (:string "(i)")
  (:font (opal:get-standard-font NIL :bold NIL)))
(create-instance 'MOTIF-MENU-J opal:text
  (:left 32) (:top 147)
  (:string "(j)")
  (:font (opal:get-standard-font NIL :bold NIL)))
(create-instance 'MOTIF-MENU-K opal:text
  (:left 137) (:top 147)
  (:string "(k)")
  (:font (opal:get-standard-font NIL :bold NIL)))
(create-instance 'MOTIF-MENU-L opal:text
  (:left 223) (:top 147)
  (:string "(l)")
  (:font (opal:get-standard-font NIL :bold NIL)))
(create-instance 'MOTIF-MENU-M opal:text
  (:left 232) (:top 185)
  (:string "(m)")
  (:font (opal:get-standard-font NIL :bold NIL)))
(create-instance 'MOTIF-MENU-N opal:text
  (:left 232) (:top 212)
  (:string "(n)")
  (:font (opal:get-standard-font NIL :bold NIL)))
(create-instance 'MOTIF-MENU-O opal:text
  (:left 122) (:top 310)
  (:string "(o)")
  (:font (opal:get-standard-font NIL :bold NIL)))

(opal:add-components MOTIF-MENU-AGG MOTIF-MENU-I MOTIF-MENU-J MOTIF-MENU-K
		     MOTIF-MENU-L MOTIF-MENU-M MOTIF-MENU-N MOTIF-MENU-O)
(opal:update MOTIF-MENU-WIN)

(create-instance 'I inter:move-grow-interactor
  (:objs (list MOTIF-MENU-I MOTIF-MENU-J MOTIF-MENU-K MOTIF-MENU-L
	       MOTIF-MENU-M MOTIF-MENU-N MOTIF-MENU-O))
  (:start-where (o-formula (list :list-element-of (gv :self) :objs)))
  (:window MOTIF-MENU-WIN)
  (:slots-to-set (list T T NIL NIL)))

(setf kr::*constants-disabled* NIL)
