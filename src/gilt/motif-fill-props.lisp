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
;;
;;; Must be loaded after line-prop.lisp and color-prop, because it uses some
;;; functions from those files.

#|
============================================================
Change log:
    10/20/92 Andrew Mickish - Careful-Eval ---> gg:Careful-String-Eval
     8/24/92 Brad Myers - Active-p slot of other-val shouldn't be constant!
     2/18/92 Brad Myers - added constant definitions
    11/22/91 Andrew Mickish - Added fast-redraw to feedback objects
     3/13/91 Osamu Hashimoto - changed grayout from rectangle-covering to active-p
     2/20/91 Osamu Hashimoto - separate from "implementation",
                          and changed into Motif-version
     1/23/91 Andrew Mickish added '(or (null style) ..)' to NEW-COLOR
             definition in Show-Fill-Props-Dialog
     1/13/91 Brad Myers - fixed bug where color rectangle isn't being reset
    11/30/90 Brad Myers - made to work with Gilt
    11/15/90 Osamu Hashimoto - created
============================================================
|#

(in-package "GILT")

(create-instance 'my-fillingf1 opal:filling-style) ; these two are used for the
(create-instance 'my-fillingf2 opal:filling-style) ; current-color

(create-instance 'my-fillingf3 opal:filling-style) ; these two are used for the
(create-instance 'my-fillingf4 opal:filling-style) ; output shade

(create-instance 'filling-boxes opal:aggregadget
  (:parts `(
    (:NON-FILL-BOX ,OPAL:RECTANGLE
      (:GILT-REF "TYPE-RECTANGLE")
      (:CONSTANT (T))
      (:BOX (30 40 55 20 ))
      (:val :none)
      (:FILLING-STYLE NIL)
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:DRAW-FUNCTION :COPY)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 30))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 40))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 55))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 20)))
    (:WHITE-FILL-BOX ,OPAL:RECTANGLE
      (:GILT-REF "TYPE-RECTANGLE")
      (:CONSTANT (T))
      (:BOX (30 70 55 20 ))
      (:val ,opal:white-fill)
      (:FILLING-STYLE ,OPAL:WHITE-FILL)
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:DRAW-FUNCTION :COPY)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 30))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 70))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 55))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 20)))
    (:LIGHT-FILL-BOX ,OPAL:RECTANGLE
      (:GILT-REF "TYPE-RECTANGLE")
      (:CONSTANT (T))
      (:BOX (30 100 55 20 ))
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:val ,opal:light-gray-fill)
      (:FILLING-STYLE ,OPAL:LIGHT-GRAY-FILL)
      (:DRAW-FUNCTION :COPY)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 30))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 100))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 55))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 20)))
    (:GRAY-FILL-BOX ,OPAL:RECTANGLE
      (:GILT-REF "TYPE-RECTANGLE")
      (:CONSTANT (T))
      (:BOX (30 130 55 20 ))
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:val ,opal:gray-fill)
      (:FILLING-STYLE ,OPAL:GRAY-FILL)
      (:DRAW-FUNCTION :COPY)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 30))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 130))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 55))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 20)))
    (:DARK-FILL-BOX ,OPAL:RECTANGLE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-RECTANGLE")
      (:BOX (30 160 55 20 ))
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:val ,opal:dark-gray-fill)
      (:FILLING-STYLE ,OPAL:DARK-GRAY-FILL)
      (:DRAW-FUNCTION :COPY)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 30))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 160))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 55))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 20)))
    (:BLACK-FILL-BOX ,OPAL:RECTANGLE
      (:CONSTANT (T))
      (:BOX (30 190 55 20 ))
      (:GILT-REF "TYPE-RECTANGLE")
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:val ,opal:black-fill)
      (:FILLING-STYLE ,OPAL:BLACK-FILL)
      (:DRAW-FUNCTION :COPY)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 30))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 190))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 55))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 20)))
  (:Other-box ,opal:rectangle
      (:CONSTANT (T :except :filling-style))
      (:BOX (30 220 55 20))
      (:LEFT ,(o-formula (FIRST (GVL :BOX))))
      (:TOP ,(o-formula (SECOND (GVL :BOX))))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 55))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 20))
      (:val ,(o-formula (gv (kr-path 0 :parent :parent :other-val) :val)))))))

(create-instance 'Fill-PROP OPAL:AGGREGADGET
  (:WINDOW-TITLE "Filling Properties")
  (:WINDOW-LEFT 0)
  (:WINDOW-TOP 0)
  (:WINDOW-WIDTH 250)
  (:WINDOW-HEIGHT 255)
  (:window-background-color opal:motif-gray)
  (:PACKAGE-NAME "GILT")
  (:FUNCTION-FOR-OK `fill-prop-OK)
  (:color-ok-func 'Fill-Color-OK)
  (:LEFT 0)
  (:TOP 0)
  (:WIDTH (o-formula (GVL :WINDOW :WIDTH) 450))
  (:HEIGHT (o-formula (GVL :WINDOW :HEIGHT) 300))
  (:new-color opal:black) ; default color
  (:parts `(
    (:FILL-STYLES ,OPAL:TEXT
      (:CONSTANT (T))
      (:GILT-REF "TYPE-TEXT")
      (:BOX (10 10 3 3 ))
      (:STRING "Fill-Style:")
      (:FONT ,(create-instance nil OPAL:FONT
            (:SIZE :LARGE)
	    (:constant '(T))
            (:FACE :BOLD-italic)))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 10))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 15)))
    (:non ,OPAL:MULTI-TEXT
      (:CONSTANT (T))
      (:GILT-REF "TYPE-TEXT")
      (:BOX (45 42 3 3 ))
      (:STRING "None")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 42))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 45)))
    (:Other-label ,opal:text
      (:BOX (38 223 175 20))
      (:CONSTANT (T))
      (:LEFT ,(o-formula (FIRST (GVL :BOX))))
      (:TOP ,(o-formula (SECOND (GVL :BOX))))
      (:string "Other:"))
    (:Other-VAL ,Garnet-gadgets:motif-scrolling-labeled-box
      (:BOX (90 220 150 20))
      (:CONSTANT (T :except :active-p))
      (:LEFT ,(o-formula (FIRST (GVL :BOX))))
      (:TOP ,(o-formula (SECOND (GVL :BOX))))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX))))
      (:active-p
       ,(o-formula (gv (kr-path 0 :parent :fill-boxes :other-box) :selected)))
      (:label-string "")
      (:field-string "")
      (:val ,(o-formula (gg:Careful-String-Eval (gvl :value) *error-gadget*)))
      (:selection-function New-Filling-Style))
    (:OKCANCEL-BUTTON ,GARNET-GADGETS:MOTIF-TEXT-BUTTON-PANEL
      (:SELECTION-FUNCTION OKCANCEL-FUNCTION)
      (:CONSTANT (T))
      (:GILT-REF "TYPE-OKCANCEL")
      (:INDENT 0)
      (:V-ALIGN :TOP)
      (:V-SPACING 5)
      (:H-ALIGN :CENTER)
      (:FIXED-HEIGHT-P T)
      (:H-SPACING 5)
      (:PIXEL-MARGIN NIL)
      (:RANK-MARGIN NIL)
      (:FIXED-WIDTH-P T)
      (:SELECT-FUNCTION OKCANCEL-FUNCTION)
      (:ITEMS ("OK" "Cancel" ))
      (:GRAY-WIDTH 3)
      (:FINAL-FEEDBACK-P NIL)
      (:TEXT-OFFSET 5)
      (:SHADOW-OFFSET 5)
      (:DIRECTION :HORIZONTAL)
      (:BOX (120 50 120 30 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 120))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 50)))
    (:CURRENT-COLOR ,OPAL:RECTANGLE
      (:CONSTANT (T :except :filling-style))
      (:GILT-REF "TYPE-RECTANGLE")
      (:BOX (150 120 55 35 ))
      (:FILLING-STYLE ,my-fillingf1)
      (:OTHER-FILLING-STYLE ,my-fillingf2)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 147))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 115))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 55))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 35))
      (:fast-redraw-p :rectangle)
      (:fast-redraw-filling-style ,opal:motif-gray-fill))
    (:CURRENT-VAL ,OPAL:RECTANGLE
      (:CONSTANT (T :except :filling-style))
      (:GILT-REF "TYPE-RECTANGLE")
      (:BOX (165 10 55 20))
      (:FILLING-STYLE ,my-fillingf3)
      (:OTHER-FILLING-STYLE ,my-fillingf4)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 147))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 115))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 55))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 35))
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
      (:TOP 12)
      (:fast-redraw-p :rectangle)
      (:fast-redraw-filling-style ,opal:motif-gray-fill))
    (:COLOR-BUTTON ,GARNET-GADGETS:MOTIF-TEXT-BUTTON-PANEL
      (:CONSTANT (T))
      (:SELECTION-FUNCTION COLOR-PROP-From-Line-Or-Fill)
      (:GILT-REF "TYPE-MOTIF-TEXT-BUTTON-PANEL")
      (:INDENT 0)
      (:FINAL-FEEDBACK-P NIL)
      (:V-ALIGN :TOP)
      (:H-SPACING 5)
      (:DIRECTION :VERTICAL)
      (:H-ALIGN :CENTER)
      (:V-SPACING 5)
      (:FIXED-HEIGHT-P T)
      (:PIXEL-MARGIN NIL)
      (:RANK-MARGIN NIL)
      (:SHADOW-OFFSET 5)
      (:FONT ,OPAL:DEFAULT-FONT)
      (:TEXT-OFFSET 5)
      (:GRAY-WIDTH 3)
      (:FIXED-WIDTH-P T)
      (:BOX (150 170 54 35 ))
      (:ITEMS ("Color" ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 145))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 171)))

    (:fill-boxes ,filling-boxes)
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
        ,(o-formula (list :element-of (gvl :operates-on :fill-boxes))))
      (:feedback-obj ,(o-formula (gvl :operates-on :i-feedback)))
      (:final-feedback-obj ,(o-formula (gvl :operates-on :feedback)))
      (:window ,(o-formula (gvl :operates-on :window)))
      (:final-function New-Filling-Style)))))

