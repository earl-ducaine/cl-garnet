;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
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
==============================================================================
Change log:
      7/6/92: Brad Vander Zanden -- changed so that defaults are remembered
                 and restored when there is a selected object
      5/27/91: Brad Vander Zanden -- created
==============================================================================
|#

(in-package "LAPIDARY")

(create-instance 'my-fillingf1 opal:filling-style) ; these two are used for the
(create-instance 'my-fillingf2 opal:filling-style) ; current-color

(create-instance 'my-fillingf3 opal:filling-style) ; these two are used for the
(create-instance 'my-fillingf4 opal:filling-style) ; output shade

(create-instance 'filling-box opal:rectangle
  (:left 30)
  (:width 55)
  (:height 20))

(create-instance 'filling-boxes opal:aggregadget
  (:parts `(
    (:NON-FILL-BOX ,FILLING-BOX
      (:val :none)
      (:name "transparent")
      (:FILLING-STYLE NIL)
      (:TOP 40))
    (:WHITE-FILL-BOX ,FILLING-BOX
      (:name "no-texture")
      (:val ,opal:white-fill)
      (:FILLING-STYLE ,OPAL:WHITE-FILL)
      (:TOP 70))
    (:LIGHT-FILL-BOX ,FILLING-BOX
      (:name "light-texture")
      (:val ,opal:light-gray-fill)
      (:FILLING-STYLE ,OPAL:LIGHT-GRAY-FILL)
      (:TOP 100))
    (:GRAY-FILL-BOX ,FILLING-BOX
      (:name "medium-texture")
      (:val ,opal:gray-fill)
      (:FILLING-STYLE ,OPAL:GRAY-FILL)
      (:TOP 130))
    (:DARK-FILL-BOX ,FILLING-BOX
      (:name "dark-texture")
      (:val ,opal:dark-gray-fill)
      (:FILLING-STYLE ,OPAL:DARK-GRAY-FILL)
      (:TOP 160))
    (:BLACK-FILL-BOX ,FILLING-BOX
      (:name "solid")
      (:val ,opal:black-fill)
      (:FILLING-STYLE ,OPAL:BLACK-FILL)
      (:TOP 190))
  (:Other-box ,filling-box 
      (:TOP 220)
      (:val ,(o-formula (gv (kr-path 0 :parent :parent :other-val) :val)))))))

(create-instance 'Fill-PROP OPAL:AGGREGADGET
  (:constant '(t))
  (:visible t)
  (:WINDOW-TITLE "Filling Properties")
  (:WINDOW-LEFT 0)
  (:WINDOW-TOP 0)
  (:WINDOW-WIDTH 250)
  (:WINDOW-HEIGHT 255)
  (:PACKAGE-NAME "LAPIDARY")
  (:FUNCTION-FOR-OK `fill-prop-OK)
  (:function-for-cancel 'restore-fill-props)
  (:color-ok-func 'Fill-Color-OK)
  (:LEFT 0)
  (:TOP 0)
  (:WIDTH (o-formula (GVL :WINDOW :WIDTH) 450))
  (:HEIGHT (o-formula (GVL :WINDOW :HEIGHT) 300))
  (:new-color opal:black) ; default color
  (:parts `(
    (:FILL-STYLES ,OPAL:TEXT
      (:constant (t))
      (:STRING "Fill-Style:")
      (:FONT ,(create-instance nil OPAL:FONT
            (:SIZE :LARGE)
            (:FACE :BOLD-italic)))
      (:LEFT 10)
      (:TOP 10))
    (:non ,OPAL:MULTI-TEXT
      (:constant (t))
      (:STRING "None")
      (:LEFT 45)
      (:TOP 42))
  (:Other-label ,opal:text
      (:constant (t))
      (:LEFT 38)
      (:TOP 223)
      (:string "Other:"))
    (:Other-VAL ,Garnet-gadgets:scrolling-labeled-box
      (:constant (t))
      (:LEFT 85)
      (:TOP 220)
      (:WIDTH 150)
      (:label-string "")
      (:val ,(o-formula (let ((val (careful-eval (gvl :value))))
			  (if (is-a-p val opal:filling-style) val ""))))
      (:selection-function New-Filling-Style))
    (:other-val-valid ,gray-out
      (:obj-over ,(o-formula (gv (kr-path 0 :parent :other-val))))
      (:visible ,(o-formula (not (gv (kr-path 0 :parent :fill-boxes
					   :other-box) :selected)))))
    (:OKCANCEL-BUTTON ,GARNET-GADGETS:TEXT-BUTTON-PANEL
      (:constant (t))
      (:SELECTION-FUNCTION gilt:OKCANCEL-FUNCTION)
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
      (:ITEMS ("OK" "Cancel" ))
      (:GRAY-WIDTH 3)
      (:FINAL-FEEDBACK-P NIL)
      (:TEXT-OFFSET 2)
      (:SHADOW-OFFSET 5)
      (:DIRECTION :HORIZONTAL)
      (:BOX (120 50 120 30 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 120))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 50)))
    (:CURRENT-COLOR ,OPAL:RECTANGLE
      (:constant (t :except :filling-style))
      (:FILLING-STYLE ,my-fillingf1)
      (:OTHER-FILLING-STYLE ,my-fillingf2)
      (:LEFT 150)
      (:TOP 125)
      (:WIDTH 55)
      (:HEIGHT 35))
    (:CURRENT-VAL ,OPAL:RECTANGLE
      (:constant (t :except :filling-style))
      (:BOX (165 10 55 20))
      (:visible ,(o-formula (or (gvl :parent :fill-boxes :selected)
				(gvl :parent :fill-boxes :default))))
      (:FILLING-STYLE ,my-fillingf3)
      (:OTHER-FILLING-STYLE ,my-fillingf4)
      (:LEFT 165)
      (:TOP 10)
      (:WIDTH 55)
      (:HEIGHT 20)) 
    (:error ,OPAL:text
      (:constant (t :except :visible))
      (:string "Error")
      (:FONT ,(create-instance nil OPAL:FONT
			       (:FACE :italic)))
      (:visible NIL)
      (:LEFT 175)
      (:TOP 12))
    (:constraint-button ,garnet-gadgets:text-button
      (:constant (t))
      (:string "Constraint")
      (:left ,(o-formula (opal:gv-center-x-is-center-of 
			  (gvl :parent :okcancel-button))))
      (:top ,(o-formula (+ 10 (opal:gv-bottom (gvl :parent :okcancel-button)))))
      (:final-feedback-p t)
      (:GRAY-WIDTH 3)
      (:TEXT-OFFSET 2)
      (:SHADOW-OFFSET 5)
      (:selected ,(o-formula (not (gvl :parent :fill-boxes :selected))))
      (:selection-function props-constraint-final-fn)
      (:slot :filling-style)
      (:menu-gadget ,(o-formula (gvl :parent :fill-boxes))))
    (:COLOR-BUTTON ,GARNET-GADGETS:TEXT-BUTTON-PANEL
      (:constant (t))
      (:SELECTION-FUNCTION COLOR-PROP-From-Line-Or-Fill)
      (:GILT-REF "TYPE-TEXT-BUTTON-PANEL")
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
      (:ITEMS ("Color" ))
      (:LEFT 150)
      (:TOP 175))

    (:fill-boxes ,filling-boxes)
    (:i-feedback ,opal:rectangle
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
      (:obj-over ,(o-formula (gvl :parent :fill-boxes :selected)))
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
        ,(o-formula (list :element-of (gvl :operates-on :fill-boxes))))
      (:feedback-obj ,(o-formula (gvl :operates-on :i-feedback)))
      (:final-feedback-obj ,(o-formula (gvl :operates-on :feedback)))
      (:window ,(o-formula (gvl :operates-on :window)))
      (:final-function New-Filling-Style)))))

;; set up the default for the fill boxes
(s-value (g-value fill-prop :fill-boxes) :default
	 (g-value fill-prop :fill-boxes :non-fill-box))
(s-value (g-value fill-prop :fill-boxes) :selected
	 (g-value fill-prop :fill-boxes :non-fill-box))

;; force evaluation of the :obj-over slot in the feedback for the fill
;; style boxes

(g-value fill-prop :feedback :obj-over)
