;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GILT; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file originally created by Gilt, then hacked to pieces

#|
============================================================
Change log:
    10/20/92 Andrew Mickish - Careful-Eval ---> gg:Careful-String-Eval
     2/19/92 Brad Myers - added constant definitions
    11/22/91 Andrew Mickish - Added fast redraw to feedback objects
    3/13/91 Osamu Hashimoto - changed grayout from rectangle-covering to active-p
    2/27/91 Osamu Hashimoto - separated from "implemenation",
                          and chaned into Motif-version
    1/13/91 Brad Myers - fixed bug where color rectangle isn't being reset
    11/30/90 Brad Myers - made to work with Gilt
    11/15/90 Osamu Hashimoto - created
============================================================
|#

(in-package "GILT")

(create-instance 'my-fillingl1 opal:filling-style) ; these two are used for the
(create-instance 'my-fillingl2 opal:filling-style) ; current-color

(create-instance 'my-line3 opal:line-style) ; these two are used for the
(create-instance 'my-line4 opal:line-style) ; output line


(create-instance 'style-boxes opal:aggregadget
  (:parts `(

    (:LINE0 ,OPAL:LINE
      (:DRAW-FUNCTION :COPY)
      (:CONSTANT (T))
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:GILT-REF "TYPE-LINE")
      (:POINTS (35 50 80 50 ))
      (:LINE-P NIL)
      (:X1 ,(o-formula (FIRST (GVL :POINTS)) 20))
      (:Y1 ,(o-formula (SECOND (GVL :POINTS)) 50))
      (:X2 ,(o-formula (THIRD (GVL :POINTS)) 65))
      (:Y2 ,(o-formula (FOURTH (GVL :POINTS)) 50)))
    (:LINE2 ,OPAL:LINE
      (:GILT-REF "TYPE-LINE")
      (:CONSTANT (T))
      (:POINTS (35 80 80 80 ))
      (:LINE-P NIL)
      (:X1 ,(o-formula (FIRST (GVL :POINTS)) 20))
      (:Y1 ,(o-formula (SECOND (GVL :POINTS)) 80))
      (:X2 ,(o-formula (THIRD (GVL :POINTS)) 65))
      (:Y2 ,(o-formula (FOURTH (GVL :POINTS)) 80))
      (:LINE-STYLE ,OPAL:LINE-2)
      (:DRAW-FUNCTION :COPY))
    (:LINE4 ,OPAL:LINE
      (:GILT-REF "TYPE-LINE")
      (:CONSTANT (T))
      (:POINTS (35 110 80 110 ))
      (:LINE-P NIL)
      (:X1 ,(o-formula (FIRST (GVL :POINTS)) 20))
      (:Y1 ,(o-formula (SECOND (GVL :POINTS)) 110))
      (:X2 ,(o-formula (THIRD (GVL :POINTS)) 65))
      (:Y2 ,(o-formula (FOURTH (GVL :POINTS)) 110))
      (:LINE-STYLE ,OPAL:LINE-4)
      (:DRAW-FUNCTION :COPY))
    (:LINE8 ,OPAL:LINE
      (:GILT-REF "TYPE-LINE")
      (:CONSTANT (T))
      (:POINTS (35 140 80 140 ))
      (:LINE-P NIL)
      (:X1 ,(o-formula (FIRST (GVL :POINTS)) 20))
      (:Y1 ,(o-formula (SECOND (GVL :POINTS)) 140))
      (:X2 ,(o-formula (THIRD (GVL :POINTS)) 65))
      (:Y2 ,(o-formula (FOURTH (GVL :POINTS)) 140))
      (:LINE-STYLE ,OPAL:LINE-8)
      (:DRAW-FUNCTION :COPY))
    (:Non ,OPAL:TEXT
      (:GILT-REF "TYPE-TEXT")
      (:CONSTANT (T))
      (:BOX (45 162 35 14 ))
      (:STRING "None")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 27))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 164)))
  (:Other-label ,opal:text
      (:BOX (38 253 175 20))
      (:CONSTANT (T))
      (:LEFT ,(o-formula (FIRST (GVL :BOX))))
      (:TOP ,(o-formula (SECOND (GVL :BOX))))
      (:string "Other:"))
    (:DOTTED ,OPAL:LINE
      (:GILT-REF "TYPE-LINE")
      (:CONSTANT (T))
      (:DRAW-FUNCTION :COPY)
      (:LINE-STYLE ,OPAL:DOTTED-LINE)
      (:POINTS (35 200 80 200 ))
      (:LINE-P NIL)
      (:X1 ,(o-formula (FIRST (GVL :POINTS)) 20))
      (:Y1 ,(o-formula (SECOND (GVL :POINTS)) 199))
      (:X2 ,(o-formula (THIRD (GVL :POINTS)) 62))
      (:Y2 ,(o-formula (FOURTH (GVL :POINTS)) 199)))
    (:DASHED ,OPAL:LINE
      (:GILT-REF "TYPE-LINE")
      (:CONSTANT (T))
      (:DRAW-FUNCTION :COPY)
      (:LINE-STYLE ,OPAL:DASHED-LINE)
      (:POINTS (35 230 80 230 ))
      (:LINE-P NIL)
      (:X1 ,(o-formula (FIRST (GVL :POINTS)) 22))
      (:Y1 ,(o-formula (SECOND (GVL :POINTS)) 229))
      (:X2 ,(o-formula (THIRD (GVL :POINTS)) 64))
      (:Y2 ,(o-formula (FOURTH (GVL :POINTS)) 229)))

    (:LINE0-BOX ,OPAL:RECTANGLE
      (:CONSTANT (T))
      (:DRAW-FUNCTION :COPY)
      (:FILLING-STYLE NIL)
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:val ,opal:line-0)
      (:BOX (30 40 55 20 ))
      (:GILT-REF "TYPE-RECTANGLE")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 15))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 40))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 55))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 20)))
    (:LINE2-BOX ,OPAL:RECTANGLE
      (:CONSTANT (T))
      (:DRAW-FUNCTION :COPY)
      (:FILLING-STYLE NIL)
      (:val ,opal:line-2)
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:BOX (30 70 55 20 ))
      (:GILT-REF "TYPE-RECTANGLE")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 15))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 70))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 55))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 20)))
    (:LINE4-BOX ,OPAL:RECTANGLE
      (:DRAW-FUNCTION :COPY)
      (:FILLING-STYLE NIL)
      (:CONSTANT (T))
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:val ,opal:line-4)
      (:BOX (30 100 55 20 ))
      (:GILT-REF "TYPE-RECTANGLE")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 15))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 100))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 55))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 20)))
    (:LINE8-BOX ,OPAL:RECTANGLE
      (:CONSTANT (T))
      (:DRAW-FUNCTION :COPY)
      (:FILLING-STYLE NIL)
      (:val ,opal:line-8)
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:BOX (30 130 55 20 ))
      (:GILT-REF "TYPE-RECTANGLE")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 15))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 130))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 55))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 20)))
    (:NON-BOX ,OPAL:RECTANGLE
      (:CONSTANT (T))
      (:DRAW-FUNCTION :COPY)
      (:FILLING-STYLE NIL)
      (:val :none)
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:BOX (30 160 55 20))
      (:GILT-REF "TYPE-RECTANGLE")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 15))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 160))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 55))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 20)))
    (:DOTTED-BOX ,OPAL:RECTANGLE
      (:CONSTANT (T))
      (:DRAW-FUNCTION :COPY)
      (:FILLING-STYLE NIL)
      (:val ,opal:dotted-line)
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:GILT-REF "TYPE-RECTANGLE")
      (:BOX (30 190 55 20 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 14))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 190))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 55))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 20)))
    (:DASHED-BOX ,OPAL:RECTANGLE
      (:CONSTANT (T))
      (:DRAW-FUNCTION :COPY)
      (:FILLING-STYLE NIL)
      (:val ,opal:dashed-line)
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:BOX (30 220 55 20 ))
      (:GILT-REF "TYPE-RECTANGLE")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 14))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 220))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 55))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 20)))
  (:Other-box ,opal:rectangle
      (:CONSTANT (T))
      (:BOX (30 250 55 20))
      (:LEFT ,(o-formula (FIRST (GVL :BOX))))
      (:TOP ,(o-formula (SECOND (GVL :BOX))))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 55))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 20))
      (:val ,(o-formula (gv (kr-path 0 :parent :parent :other-val) :val))))
  )))

(create-instance 'LINE-PROP opal:aggregadget
  (:WINDOW-TITLE "Line Properties")
  (:WINDOW-LEFT 0)
  (:WINDOW-TOP 0)
  (:WINDOW-WIDTH 250)
  (:WINDOW-HEIGHT 300)
  (:window-background-color opal:motif-gray)
  (:PACKAGE-NAME "GILT")
  (:FUNCTION-FOR-OK 'line-prop-ok)
  (:color-ok-func 'Line-Color-OK)
  (:LEFT 0)
  (:TOP 0)
  (:WIDTH (o-formula (GVL :WINDOW :WIDTH) 450))
  (:HEIGHT (o-formula (GVL :WINDOW :HEIGHT) 300))
  (:parts `(
    (:OKCANCEL-button ,GARNET-GADGETS:motif-TEXT-BUTTON-PANEL
      (:CONSTANT (T))
      (:BOX (120 50 120 30 ))
      (:DIRECTION :HORIZONTAL)
      (:SHADOW-OFFSET 5)
      (:TEXT-OFFSET 5)
      (:FINAL-FEEDBACK-P NIL)
      (:GRAY-WIDTH 3)
      (:ITEMS ("OK" "Cancel" ))
      (:SELECT-FUNCTION GILT:OKCANCEL-FUNCTION)
      (:FIXED-WIDTH-P T)
      (:RANK-MARGIN NIL)
      (:PIXEL-MARGIN NIL)
      (:H-SPACING 5)
      (:FIXED-HEIGHT-P T)
      (:H-ALIGN :CENTER)
      (:V-SPACING 5)
      (:V-ALIGN :TOP)
      (:INDENT 0)
      (:GILT-REF "TYPE-OKCANCEL")
      (:SELECTION-FUNCTION GILT:OKCANCEL-FUNCTION)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 119))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 48)))

    (:LINE-STYLES ,OPAL:MULTI-TEXT
      (:CONSTANT (T))
      (:FONT ,(create-instance nil OPAL:FONT
            (:CONSTANT '(T))
            (:SIZE :LARGE)
            (:FACE :BOLD-ITALIC)))
      (:STRING "Line-Style:")
      (:BOX (10 10 3 3 ))
      (:GILT-REF "TYPE-TEXT")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 12))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 15)))

    (:CURRENT-VAL ,OPAL:line
      (:CONSTANT (T :except :line-style))
      (:GILT-REF "TYPE-RECTANGLE")
      (:line-STYLE ,my-line3)
      (:OTHER-line-STYLE ,my-line4)
      (:x1 165)
      (:y1 15)
      (:x2 220)
      (:y2 15)
      (:fast-redraw-p :rectangle)
      (:fast-redraw-filling-style ,opal:motif-gray-fill))
    (:error ,OPAL:text
      (:CONSTANT (T :except :visible))
      (:string "Error")
      (:FONT ,(create-instance nil OPAL:FONT
			       (:constant '(T))
			       (:FACE :italic)))
      (:visible NIL)
      (:LEFT 175)
      (:TOP 20)
      (:line-style ,(o-formula
		     (if (gv opal:color :color-p)
			 (create-instance NIL opal:line-style
			    (:CONSTANT '(T))
			    (:foreground-color opal:red))
			 opal:default-line-style)))
      (:fast-redraw-p :rectangle)
      (:fast-redraw-filling-style ,opal:motif-gray-fill))
    (:CURRENT-COLOR ,OPAL:RECTANGLE
      (:CONSTANT (T :except :filling-style :line-style))
      (:FILLING-STYLE ,my-fillingl1)
      (:OTHER-FILLING-STYLE ,my-fillingl2)
      (:BOX (150 150 55 35 ))
      (:GILT-REF "TYPE-RECTANGLE")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 152))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 124))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 45))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 45))
      (:fast-redraw-p :rectangle)
      (:fast-redraw-filling-style ,opal:motif-gray-fill))
    (:COLOR-button ,GARNET-GADGETS:motif-TEXT-BUTTON-PANEL
      (:CONSTANT (T))
      (:ITEMS ("Color" ))
      (:BOX (150 200 55 35 ))
      (:FINAL-FEEDBACK-P NIL)
      (:FIXED-WIDTH-P T)
      (:GRAY-WIDTH 3)
      (:TEXT-OFFSET 5)
      (:FONT ,OPAL:DEFAULT-FONT)
      (:SHADOW-OFFSET 5)
      (:RANK-MARGIN NIL)
      (:PIXEL-MARGIN NIL)
      (:FIXED-HEIGHT-P T)
      (:V-SPACING 5)
      (:H-ALIGN :CENTER)
      (:DIRECTION :VERTICAL)
      (:H-SPACING 5)
      (:V-ALIGN :TOP)
      (:INDENT 0)
      (:GILT-REF "TYPE-MOTIF-TEXT-BUTTON-PANEL")
      (:SELECTION-FUNCTION COLOR-PROP-From-Line-Or-Fill)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 146))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 188)))

    (:line-style-boxes ,style-boxes)
    (:Other-VAL ,Garnet-gadgets:motif-scrolling-labeled-box 
      (:CONSTANT (T :except :active-p))
      (:BOX (90 250 150 20))
      (:LEFT ,(o-formula (FIRST (GVL :BOX))))
      (:TOP ,(o-formula (SECOND (GVL :BOX))))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX))))
      (:val ,(o-formula (gg:Careful-String-Eval (gvl :value) *error-gadget*)))
      (:active-p ,(o-formula (gv (kr-path 0 :parent :line-style-boxes
					   :other-box) :selected)))
      (:label-string "")
      (:field-string "")
      (:selection-function New-Line-Style))
    (:i-feedback ,opal:rectangle
      (:obj-over NIL)
      (:visible ,(o-formula (gvl :obj-over)))
      (:left ,(o-formula (- (gvl :obj-over :left) 4)))
      (:top ,(o-formula (- (gvl :obj-over :top) 4)))
      (:width ,(o-formula (+ (gvl :obj-over :width) 8)))
      (:height ,(o-formula (+ (gvl :obj-over :height) 8)))
      (:line-style ,opal:dashed-line)
      (:fast-redraw-p :redraw)
      (:fast-redraw-line-style ,(create-instance NIL opal:line-style
				   (:CONSTANT '(T))
				   (:foreground-color opal:motif-gray))))
    (:feedback ,opal:rectangle
      (:obj-over NIL)
      (:visible ,(o-formula (gvl :obj-over)))
      (:left ,(o-formula (- (gvl :obj-over :left) 3)))
      (:top ,(o-formula (- (gvl :obj-over :top) 3)))
      (:width ,(o-formula (+ (gvl :obj-over :width) 6)))
      (:height ,(o-formula (+ (gvl :obj-over :height) 6)))
      (:line-style ,opal:line-2)
      (:fast-redraw-p :redraw)
      (:fast-redraw-line-style ,(create-instance NIL opal:line-style
				   (:CONSTANT '(T))
				   (:line-thickness 2)
				   (:foreground-color opal:motif-gray))))))
   (:interactors `(
    (:press ,inter:menu-interactor
      (:start-where
        ,(o-formula (list :element-of (gvl :operates-on :line-style-boxes))))
      (:feedback-obj ,(o-formula (gvl :operates-on :i-feedback)))
      (:final-feedback-obj ,(o-formula (gvl :operates-on :feedback)))
      (:window ,(o-formula (gvl :operates-on :window)))
      (:final-function New-Line-Style)))))

