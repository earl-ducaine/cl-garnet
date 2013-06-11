;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file originally created by Gilt, then hacked

#|
============================================================
Change log:
      5/27/91: Brad Vander Zanden -- created
============================================================
|#

(in-package "LAPIDARY")
(create-instance 'my-color opal:color
        (:red 0.0)(:green 0.0)(:blue 0.0))
(create-instance 'my-filling1 opal:filling-style)
(create-instance 'my-filling2 opal:filling-style)

(create-instance 'label-texts opal:aggregadget
  (:parts `(	   
    (:COLOR-SELECTION ,OPAL:MULTI-TEXT
      (:constant (t))
      (:GILT-REF "TYPE-TEXT")
      (:STRING "Color-Selection")
      (:FONT ,(create-instance nil OPAL:FONT
            (:SIZE :LARGE)
            (:FACE :BOLD)))
      (:LEFT 10)
      (:TOP 15))
    (:WHITE-LABEL ,OPAL:MULTI-TEXT
      (:constant (t))
      (:STRING "White")
      (:GILT-REF "TYPE-TEXT")
      (:LEFT 100)
      (:TOP 42))
    (:BLACK-LABEL ,OPAL:MULTI-TEXT
      (:constant (t))
      (:STRING "Black")
      (:GILT-REF "TYPE-TEXT")
      (:LEFT 100)
      (:TOP 72))
    (:RED-LABEL ,OPAL:MULTI-TEXT
      (:STRING "Red")
      (:constant (t))
      (:GILT-REF "TYPE-TEXT")
      (:LEFT 100)
      (:TOP 102))
    (:GREEN-LABEL ,OPAL:MULTI-TEXT
      (:constant (t))
      (:STRING "Green")
      (:GILT-REF "TYPE-TEXT")
      (:LEFT 100)
      (:TOP 132))
    (:BLUE-LABEL ,OPAL:MULTI-TEXT
      (:constant (t))
      (:STRING "Blue")
      (:GILT-REF "TYPE-TEXT")
      (:LEFT 100)
      (:TOP 162))
    (:YELLOW-LABEL ,OPAL:MULTI-TEXT
      (:STRING "Yellow")
      (:constant (t))
      (:GILT-REF "TYPE-TEXT")
      (:LEFT 100)
      (:TOP 192))
    (:PURPLE-LABEL ,OPAL:MULTI-TEXT
      (:constant (t))
      (:STRING "Purple")
      (:GILT-REF "TYPE-TEXT")
      (:LEFT 100)
      (:TOP 222))
    (:CYAN-LABEL ,OPAL:MULTI-TEXT
      (:STRING "Cyan")
      (:constant (t))
      (:GILT-REF "TYPE-TEXT")
      (:LEFT 100)
      (:TOP 252))
    (:ORANGE-LABEL ,OPAL:MULTI-TEXT
      (:constant (t))
      (:STRING "Orange")
      (:GILT-REF "TYPE-TEXT")
      (:LEFT 100)
      (:TOP 282))
    (:red-slider-label ,OPAL:MULTI-TEXT
      (:GILT-REF "TYPE-TEXT")
      (:constant (t))
      (:STRING "RED")
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:LEFT 202)
      (:TOP 290))
    (:green-slider-label ,OPAL:MULTI-TEXT
      (:GILT-REF "TYPE-TEXT")
      (:constant (t))
      (:STRING "GREEN")
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:LEFT 275)
      (:TOP 290))
    (:blue-slider-label ,OPAL:MULTI-TEXT
      (:GILT-REF "TYPE-TEXT")
      (:constant (t))
      (:STRING "BLUE")
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:LEFT 359)
      (:TOP 290))
)))


;;;**** TEMP *** white-fill has color black now??
(s-value opal:white-fill :foreground-color opal:white)

(create-instance 'color-box opal:rectangle
    (:left 30)
    (:width 55)
    (:height 20)
    (:line-style opal:default-line-style))

(create-instance 'color-boxes opal:aggregadget
  (:parts `(	   
    (:WHITE-BOX ,COLOR-BOX
      (:constant (t))
      (:FILLING-STYLE ,OPAL:WHITE-FILL)
      (:TOP 40))
    (:BLACK-BOX ,COLOR-BOX
      (:constant (t))
      (:FILLING-STYLE ,OPAL:BLACK-FILL)
      (:TOP 70))
    (:RED-BOX ,COLOR-BOX
      (:constant (t))
      (:FILLING-STYLE ,OPAL:RED-FILL)
      (:TOP 100))
    (:GREEN-BOX ,COLOR-BOX
      (:constant (t))
      (:FILLING-STYLE ,OPAL:GREEN-FILL)
      (:TOP 130))
    (:BLUE-BOX ,COLOR-BOX
      (:constant (t))
      (:FILLING-STYLE ,OPAL:BLUE-FILL)
      (:TOP 160))
    (:YELLOW-BOX ,COLOR-BOX
      (:constant (t))
      (:FILLING-STYLE ,OPAL:YELLOW-FILL)
      (:TOP 190))
    (:PURPLE-BOX ,COLOR-BOX
      (:FILLING-STYLE ,OPAL:PURPLE-FILL)
      (:constant (t))
      (:TOP 220))
    (:CYAN-BOX ,COLOR-BOX
      (:constant (t))
      (:FILLING-STYLE ,OPAL:CYAN-FILL)
      (:TOP 250))
    (:ORANGE-BOX ,COLOR-BOX
      (:constant (t))
      (:FILLING-STYLE ,OPAL:ORANGE-FILL)
      (:TOP 280))
)))

(create-instance 'COLOR-PROP OPAL:AGGREGADGET
  (:WINDOW-TITLE "Color Properties")
  (:WINDOW-LEFT 0)
  (:WINDOW-TOP 0)
  (:WINDOW-WIDTH 450)
  (:WINDOW-HEIGHT 330)
  (:PACKAGE-NAME "LAPIDARY")
  (:FUNCTION-FOR-OK `color-prop-ok)
  (:LEFT 0)
  (:TOP 0)
  (:WIDTH (o-formula (GVL :WINDOW :WIDTH) 450))
  (:HEIGHT (o-formula (GVL :WINDOW :HEIGHT) 330))
  (:parts `(
    (:OKCANCEL-BUTTON ,GARNET-GADGETS:TEXT-BUTTON-PANEL
      (:constant (t))
      (:SELECTION-FUNCTION OKCANCEL-FUNCTION)
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
      (:TEXT-OFFSET 2)
      (:SHADOW-OFFSET 5)
      (:DIRECTION :HORIZONTAL)
      (:LEFT 300)
      (:TOP 30))

    (:CURRENT-COLOR ,OPAL:RECTANGLE
      (:constant (t :except :filling-style))
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:FILLING-STYLE ,my-filling1)
      (:OTHER-FILLING-STYLE ,my-filling2)
      (:LEFT 200)
      (:TOP 20)
      (:WIDTH 55)
      (:HEIGHT 35))

    (:RED-SLIDER ,GARNET-GADGETS:V-SLIDER
      (:constant (t))
      (:selection-function slider-value)
      (:color-type :red)
      (:val-1 0)
      (:VAL-2 100)
      (:page-incr 10)
      (:NUM-MARKS 6)
      (:GILT-REF "TYPE-V-SLIDER")
      (:LEFT 180)
      (:TOP 80)
      (:HEIGHT 205))
    (:GREEN-SLIDER ,GARNET-GADGETS:V-SLIDER
      (:constant (t))
      (:selection-function slider-value)
      (:color-type :GREEN)
      (:val-1 0)
      (:VAL-2 100)
      (:page-incr 10)
      (:NUM-MARKS 6)
      (:GILT-REF "TYPE-V-SLIDER")
      (:LEFT 260)
      (:TOP 80)
      (:HEIGHT 205))
    (:BLUE-SLIDER ,GARNET-GADGETS:V-SLIDER
      (:constant (t))
      (:selection-function slider-value)
      (:color-type :BLUE)
      (:val-1 0)
      (:VAL-2 100)
      (:page-incr 10)
      (:NUM-MARKS 6)
      (:LEFT 340)
      (:TOP 80)
      (:HEIGHT 205))

    (:labels ,label-texts)
    (:colors ,color-boxes)
    (:i-feedback ,opal:rectangle
      (:constant (:line-style :filling-style :draw-function))
      (:obj-over NIL)
      (:visible ,(o-formula (gvl :obj-over)))
      (:left ,(o-formula (- (gvl :obj-over :left) 3)))
      (:top ,(o-formula (- (gvl :obj-over :top) 3)))
      (:width ,(o-formula (+ (gvl :obj-over :width) 6)))
      (:height ,(o-formula (+ (gvl :obj-over :height) 6)))
      (:fast-redraw-p T)
      (:line-style ,opal:dashed-line)
      (:draw-function :xor))
    (:feedback ,opal:rectangle
      (:constant (:line-style :filling-style :draw-function))
      (:obj-over NIL)
      (:visible ,(o-formula (gvl :obj-over)))
      (:left ,(o-formula (- (gvl :obj-over :left) 3)))
      (:top ,(o-formula (- (gvl :obj-over :top) 3)))
      (:width ,(o-formula (+ (gvl :obj-over :width) 6)))
      (:height ,(o-formula (+ (gvl :obj-over :height) 6)))
      (:fast-redraw-p T)
      (:line-style ,opal:line-2)
      (:draw-function :xor))))
  (:interactors `(
    (:press ,inter:menu-interactor
      (:constant (:start-where :feedback-obj :final-feedback-obj))
      (:start-where
        ,(o-formula (list :element-of (gvl :operates-on :colors))))
      (:feedback-obj ,(o-formula (gvl :operates-on :i-feedback)))
      (:final-feedback-obj ,(o-formula (gvl :operates-on :feedback)))
      (:final-function definite-color)
      (:window ,(o-formula (gvl :operates-on :window)))))))

