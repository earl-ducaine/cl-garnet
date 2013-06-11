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

#|
============================================================================
Change log:
      7/6/92: Brad Vander Zanden -- changed so that defaults are remembered
                 and restored when there is a selected object
      5/27/91: Brad Vander Zanden -- created
============================================================================
|#

(in-package "LAPIDARY")

(create-instance 'gray-out opal:rectangle
		 (:draw-function :and)
		 (:obj-over NIL)
		 (:line-style NIL)
		 (:filling-style opal:gray-fill)
		 (:left (o-formula (gvl :obj-over :left)))
		 (:top (o-formula (gvl :obj-over :top)))
		 (:width (o-formula (gvl :obj-over :width)))
		 (:height (o-formula (gvl :obj-over :height)))
		 (:visible NIL)) ; usually replace this with a formula

(create-instance 'my-fillingl1 opal:filling-style) ; these two are used for the
(create-instance 'my-fillingl2 opal:filling-style) ; current-color

(create-instance 'my-line3 opal:line-style) ; these two are used for the
(create-instance 'my-line4 opal:line-style) ; output line

(create-instance 'line-style-line opal:line
  (:x1 35)
  (:x2 80))

(create-instance 'line-style-box opal:rectangle
  (:left 30)
  (:width 55)
  (:height 20)
  (:filling-style nil))

(create-instance 'style-boxes opal:aggregadget
  (:parts `(

    (:LINE0 ,LINE-STYLE-LINE
      (:constant (t))
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:Y1 50)
      (:Y2 50))
    (:LINE2 ,LINE-STYLE-LINE
      (:constant (t))
       (:Y1 80)
       (:Y2 80)
      (:LINE-STYLE ,opal:LINE-2))
    (:LINE4 ,LINE-STYLE-LINE
      (:constant (t))
      (:Y1 110)
      (:Y2 110)
      (:LINE-STYLE ,opal:LINE-4))
    (:LINE8 ,LINE-STYLE-line
      (:constant (t))
      (:Y1 140)
      (:Y2 140)
      (:LINE-STYLE ,OPAL:LINE-8))
    (:Non ,OPAL:TEXT
      (:constant (t))
      (:STRING "None")
      (:LEFT 45)
      (:TOP 162))
  (:Other-label ,opal:text
      (:constant (t))
      (:LEFT 38)
      (:TOP 253)
      (:string "Other:"))
    (:DOTTED ,line-style-line
      (:LINE-STYLE ,OPAL:DOTTED-LINE)
      (:constant (t))
      (:Y1 200)
      (:Y2 200))
    (:DASHED ,line-style-line
      (:LINE-STYLE ,OPAL:DASHED-LINE)
      (:constant (t))
      (:Y1 230)
      (:Y2 230))

    (:LINE0-BOX ,LINE-STYLE-BOX
      (:name "thin-line")
      (:val ,opal:line-0)
      (:TOP 40))
    (:LINE2-BOX ,LINE-STYLE-BOX
      (:name "line-2")
      (:val ,opal:line-2)
      (:TOP 70))
    (:LINE4-BOX ,LINE-STYLE-BOX
      (:name "line-4")
      (:val ,opal:line-4)
      (:TOP 100))
    (:LINE8-BOX ,LINE-STYLE-BOX
      (:name "line-8")
      (:val ,opal:line-8)
      (:TOP 130))
    (:NON-BOX ,LINE-STYLE-BOX
      (:name "borderless")
      (:val :none)
      (:TOP 160))
    (:DOTTED-BOX ,LINE-STYLE-BOX
      (:name "dotted-line")
      (:val ,opal:dotted-line)
      (:TOP 190))
    (:DASHED-BOX ,LINE-STYLE-BOX
      (:name "dashed-line")
      (:val ,opal:dashed-line)
      (:TOP 220))
  (:Other-box ,line-style-box
      (:TOP 250)
      (:val ,(o-formula (careful-eval
			   (gv (kr-path 0 :parent :parent :other-val) :val)))))
  )))

(create-instance 'LINE-PROP opal:aggregadget
  (:WINDOW-TITLE "Line Properties")
  (:WINDOW-LEFT 0)
  (:WINDOW-TOP 0)
  (:WINDOW-WIDTH 250)
  (:WINDOW-HEIGHT 300)
  (:PACKAGE-NAME "LAPIDARY")
  (:FUNCTION-FOR-OK 'line-prop-ok)
  (:function-for-cancel 'restore-line-props)
  (:color-ok-func 'Line-Color-OK)
  (:new-color opal:black)
  (:LEFT 0)
  (:TOP 0)
  (:WIDTH (o-formula (GVL :WINDOW :WIDTH) 450))
  (:HEIGHT (o-formula (GVL :WINDOW :HEIGHT) 300))
  (:parts `(

    (:OKCANCEL-button ,GARNET-GADGETS:TEXT-BUTTON-PANEL
      (:constant (t))
      (:DIRECTION :HORIZONTAL)
      (:SHADOW-OFFSET 5)
      (:TEXT-OFFSET 2)
      (:FINAL-FEEDBACK-P NIL)
      (:GRAY-WIDTH 3)
      (:ITEMS ("OK" "Cancel" ))
      (:FIXED-WIDTH-P T)
      (:RANK-MARGIN NIL)
      (:PIXEL-MARGIN NIL)
      (:H-SPACING 5)
      (:FIXED-HEIGHT-P T)
      (:H-ALIGN :CENTER)
      (:V-SPACING 5)
      (:V-ALIGN :TOP)
      (:INDENT 0)
      (:SELECTION-FUNCTION GILT:OKCANCEL-FUNCTION)
      (:LEFT 120)
      (:TOP 50))

    (:LINE-STYLES ,OPAL:MULTI-TEXT
      (:constant (t))
      (:FONT ,(create-instance nil OPAL:FONT
            (:SIZE :LARGE)
            (:FACE :BOLD-ITALIC)))
      (:STRING "Line-Style:")
      (:LEFT 10)
      (:TOP 10))

    (:CURRENT-VAL ,OPAL:line
      (:constant (t :except :line-style))
      (:line-STYLE ,my-line3)
      (:OTHER-line-STYLE ,my-line4)
      (:visible ,(o-formula (or (gvl :parent :line-style-boxes :selected)
				(gvl :parent :line-style-boxes :default))))
      (:x1 165)
      (:y1 15)
      (:x2 220)
      (:y2 15))
    (:error ,OPAL:text
      (:constant (t :except :visible))
      (:string "Error")
      (:FONT ,(create-instance nil OPAL:FONT
			       (:FACE :italic)))
      (:visible NIL)
      (:LEFT 175)
      (:TOP 20))
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
      (:selected ,(o-formula (not (gvl :parent :line-style-boxes :selected))))
      (:selection-function props-constraint-final-fn)
      (:slot :line-style)
      (:menu-gadget ,(o-formula (gvl :parent :line-style-boxes))))
    (:CURRENT-COLOR ,OPAL:RECTANGLE
      (:constant (t :except :filling-style))
      (:FILLING-STYLE ,my-fillingl1)
      (:OTHER-FILLING-STYLE ,my-fillingl2)
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:LEFT 150)
      (:TOP 150)
      (:WIDTH 55)
      (:HEIGHT 35))
    (:COLOR-button ,GARNET-GADGETS:TEXT-BUTTON-PANEL
      (:constant (t))
      (:ITEMS ("Color" ))
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
      (:SELECTION-FUNCTION COLOR-PROP-From-Line-Or-Fill)
      (:LEFT 150)
      (:TOP 200))
    (:line-style-boxes ,style-boxes)
    (:Other-VAL ,Garnet-gadgets:scrolling-labeled-box
      (:constant (t))
      (:LEFT 85)
      (:TOP 250)
      (:WIDTH 150)
      (:val ,(o-formula (let ((val (careful-eval (gvl :value))))
			  (if (is-a-p val opal:line-style) val ""))))
      (:label-string "")
      (:selection-function New-Line-Style))
    (:other-val-valid ,gray-out
      (:obj-over ,(o-formula (gv (kr-path 0 :parent :other-val))))
      (:visible ,(o-formula (not (gv (kr-path 0 :parent :line-style-boxes
					   :other-box) :selected)))))
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
      (:obj-over ,(o-formula (gvl :parent :line-style-boxes :selected)))
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
        ,(o-formula (list :element-of (gvl :operates-on :line-style-boxes))))
      (:feedback-obj ,(o-formula (gvl :operates-on :i-feedback)))
      (:final-feedback-obj ,(o-formula (gvl :operates-on :feedback)))
      (:window ,(o-formula (gvl :operates-on :window)))
      (:final-function New-Line-Style)))))

;; demand the bounding box :current-val so that if its first line-style is nil,
;; its bounding box will already have been computed (the computation needs
;; to know line-thickness and thus initial line-style cannot be nil
(opal:bounding-box (g-value line-prop :current-val))

;; set up the default for the line-style boxes
(s-value (g-value line-prop :line-style-boxes) :default
	 (g-value line-prop :line-style-boxes :line0-box))
(s-value (g-value line-prop :line-style-boxes) :selected
	 (g-value line-prop :line-style-boxes :line0-box))
 
;; force evaluation of the :obj-over slot in the feedback for the line
;; style boxes

(g-value line-prop :feedback :obj-over)
