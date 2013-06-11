;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-

(in-package "USER" :use '("LISP" "KR"))

(defvar TEXT-GROUP-INIT
  (progn
    (garnet-load "opal:multifont-loader")
    (dolist (gadget '("labeled-box-loader"
		      "scrolling-labeled-box-loader"
		      "scrolling-input-string-loader"
		      "motif-scrolling-labeled-box-loader"
		      ))
      (load (merge-pathnames gadget
			     user::Garnet-Gadgets-PathName)))))

(when (boundp 'TEXT-WIN) (opal:destroy TEXT-WIN))

(setf kr::*constants-disabled* T)

(create-instance 'TEXT-AGG OPAL:AGGREGADGET
  (:LEFT 0)
  (:TOP 0)
  (:parts `(
    (:lb ,GARNET-GADGETS:LABELED-BOX
      (:LEFT ,(formula `(FIRST (GVL :BOX ) ) 200))
      (:TOP ,(formula `(SECOND (GVL :BOX ) ) 18))
      (:GILT-REF "TYPE-LABELED-BOX")
      (:BOX (10 10 175 18 ))
      (:MIN-WIDTH 140)
      (:LABEL-STRING "Title:")
      (:FIELD-OFFSET 6)
      (:FIELD-FONT ,OPAL:DEFAULT-FONT)
      (:MIN-FRAME-WIDTH 130)
      (:LABEL-OFFSET 5)
      (:LABEL-FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD))))
    (:slb ,GARNET-GADGETS:SCROLLING-LABELED-BOX
      (:LEFT ,(formula `(FIRST (GVL :BOX ) ) 200))
      (:TOP ,(formula `(SECOND (GVL :BOX ) ) 44))
      (:WIDTH ,(formula `(THIRD (GVL :BOX ) ) 196))
      (:GILT-REF "TYPE-SCROLLING-LABELED-BOX")
      (:BOX (10 45 175 18 ))
      (:MIN-WIDTH 120)
      (:GROW-P T)
      (:LABEL-STRING "Title:"))
    (:sis ,GARNET-GADGETS:SCROLLING-INPUT-STRING
      (:LEFT ,(formula `(FIRST (GVL :BOX ) ) 215))
      (:TOP ,(formula `(SECOND (GVL :BOX ) ) 70))
      (:WIDTH 150)
      (:BOX (235 10 170 14 ))
      (:GILT-REF "TYPE-TEXT")))))

(create-instance 'TEXT-WIN inter:interactor-window
  (:left 500) (:top 10)
  (:width 443) (:height 70)
  (:aggregate TEXT-AGG))
(opal:update TEXT-WIN)

(create-instance 'SUB-TEXT-WIN inter:interactor-window
  (:left 220) (:top 30)
  (:border-width 0)
  (:parent TEXT-WIN)
  (:background-color opal:motif-gray)
  (:aggregate (create-instance 'SUB-TEXT-AGG opal:aggregate)))
(opal:update SUB-TEXT-WIN)

(opal:add-component SUB-TEXT-AGG
		    (create-instance 'MSLB gg:MOTIF-SCROLLING-LABELED-BOX
		      (:left 10) (:top 10)
		      (:width 175)))
(opal:update SUB-TEXT-WIN)


(setf lb (g-value text-agg :lb))
(setf slb (g-value text-agg :slb))
(setf sis (g-value text-agg :sis))


(create-instance 'TEXT-A opal:text
  (:left 191) (:top 12)
  (:string "(a)")
  (:font (opal:get-standard-font NIL :bold NIL)))
(create-instance 'TEXT-B opal:text
  (:left 191) (:top 46)
  (:string "(b)")
  (:font (opal:get-standard-font NIL :bold NIL)))
(create-instance 'TEXT-C opal:text
  (:left 414) (:top 9)
  (:string "(c)")
  (:font (opal:get-standard-font NIL :bold NIL)))
(create-instance 'TEXT-D opal:text
  (:left 194) (:top 14)
  (:string "(d)")
  (:font (opal:get-standard-font NIL :bold NIL)))

(opal:add-components TEXT-AGG TEXT-A TEXT-B TEXT-C)
(opal:add-component SUB-TEXT-AGG TEXT-D)
(opal:update TEXT-WIN)

(create-instance 'I inter:move-grow-interactor
  (:objs (list TEXT-A TEXT-B TEXT-C TEXT-D))
  (:start-where (o-formula (list :list-element-of (gv :self) :objs)))
  (:window (list TEXT-WIN SUB-TEXT-WIN))
  (:slots-to-set (list T T NIL NIL)))

(setf kr::*constants-disabled* NIL)

