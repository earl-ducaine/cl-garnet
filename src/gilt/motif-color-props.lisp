;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GILT; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file created by GILT V0.3: The Garnet Interface Builder
;;; on Feb 27, 1991, 1:04 PM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
============================================================
Change log:
     2/18/92 Brad Myers - added constant definitions
     2/27/91 Brad Myers - created using Gilt, then hacked
============================================================
|#

;;; This file uses the following objects:
;;;     MULTI-TEXT from package OPAL
;;;     MOTIF-TEXT-BUTTON-PANEL from package GARNET-GADGETS
;;;     RECTANGLE from package OPAL
;;;     MOTIF-SLIDER from package GARNET-GADGETS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "GILT")

(create-instance 'my-color opal:color
        (:red 0.0)(:green 0.0)(:blue 0.0))
(create-instance 'my-filling1 opal:filling-style)
(create-instance 'my-filling2 opal:filling-style)
(create-instance 'gray-i-feedback-ls opal:line-style
  (:constant T)
  (:foreground-color opal:motif-gray))

;-----------------------------------------------------------------------

(create-instance 'label-texts opal:aggregadget
  (:parts `(	   
    (NIL ,OPAL:MULTI-TEXT
      (:CONSTANT (T))
      (:GILT-REF "TYPE-TEXT")
      (:FONT ,(create-instance nil OPAL:FONT
	    (:CONSTANT '(T))
            (:FACE :BOLD)))
      (:BOX (303 276 22 14 ))
      (:STRING "Red")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 303))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 276)))
    (NIL ,OPAL:MULTI-TEXT
      (:CONSTANT (T))
      (:GILT-REF "TYPE-TEXT")
      (:FONT ,(create-instance nil OPAL:FONT
	    (:CONSTANT '(T))
            (:FACE :BOLD)))
      (:BOX (353 276 34 14 ))
      (:STRING "Green")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 353))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 276)))
    (NIL ,OPAL:MULTI-TEXT
      (:CONSTANT (T))
      (:GILT-REF "TYPE-TEXT")
      (:FONT ,(create-instance nil OPAL:FONT
	    (:CONSTANT '(T))
            (:FACE :BOLD)))
      (:BOX (415 276 28 14 ))
      (:STRING "Blue")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 415))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 276)))
    (NIL ,OPAL:MULTI-TEXT
      (:GILT-REF "TYPE-TEXT")
      (:CONSTANT (T))
      (:FONT ,(create-instance nil OPAL:FONT
	    (:CONSTANT '(T))
            (:SIZE :LARGE)
            (:FACE :BOLD)))
      (:BOX (11 13 164 20 ))
      (:STRING "Color Selection")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 11))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 13)))
    (NIL ,OPAL:MULTI-TEXT
      (:CONSTANT (T))
      (:GILT-REF "TYPE-TEXT")
      (:BOX (74 51 34 14 ))
      (:STRING "White")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 74))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 51)))
    (NIL ,OPAL:MULTI-TEXT
      (:CONSTANT (T))
      (:GILT-REF "TYPE-TEXT")
      (:BOX (74 89 35 14 ))
      (:STRING "Black")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 74))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 89)))
    (NIL ,OPAL:MULTI-TEXT
      (:CONSTANT (T))
      (:GILT-REF "TYPE-TEXT")
      (:BOX (74 127 70 14 ))
      (:STRING "Motif-gray")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 74))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 127)))
    (NIL ,OPAL:MULTI-TEXT
      (:CONSTANT (T))
      (:GILT-REF "TYPE-TEXT")
      (:BOX (74 165 69 14 ))
      (:STRING "Motif-blue")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 74))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 165)))
    (NIL ,OPAL:MULTI-TEXT
      (:CONSTANT (T))
      (:GILT-REF "TYPE-TEXT")
      (:BOX (74 241 83 14 ))
      (:STRING "Motif-orange")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 74))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 241)))
    (NIL ,OPAL:MULTI-TEXT
      (:CONSTANT (T))
      (:GILT-REF "TYPE-TEXT")
      (:BOX (74 203 77 14 ))
      (:STRING "Motif-green")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 74))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 203)))
    (NIL ,OPAL:MULTI-TEXT
      (:CONSTANT (T))
      (:GILT-REF "TYPE-TEXT")
      (:BOX (74 279 3 3 ))
      (:STRING "Red")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 74))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 279)))
    (NIL ,OPAL:MULTI-TEXT
      (:GILT-REF "TYPE-TEXT")
      (:CONSTANT (T))
      (:BOX (240 88 3 3 ))
      (:STRING "Green")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 240))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 88)))
    (NIL ,OPAL:MULTI-TEXT
      (:CONSTANT (T))
      (:GILT-REF "TYPE-TEXT")
      (:BOX (240 126 3 3 ))
      (:STRING "Blue")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 240))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 126)))
    (NIL ,OPAL:MULTI-TEXT
      (:CONSTANT (T))
      (:GILT-REF "TYPE-TEXT")
      (:BOX (240 164 3 3 ))
      (:STRING "Yellow")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 240))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 164)))
    (NIL ,OPAL:MULTI-TEXT
      (:CONSTANT (T))
      (:GILT-REF "TYPE-TEXT")
      (:BOX (240 202 3 3 ))
      (:STRING "Purple")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 240))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 202)))
    (NIL ,OPAL:MULTI-TEXT
      (:CONSTANT (T))
      (:GILT-REF "TYPE-TEXT")
      (:BOX (240 240 3 3 ))
      (:STRING "Cyan")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 240))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 240)))
    (NIL ,OPAL:MULTI-TEXT
      (:CONSTANT (T))
      (:GILT-REF "TYPE-TEXT")
      (:STRING "Orange")
      (:BOX (240 278 3 3 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 240))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 278))))))

;;;**** TEMP *** white-fill has color black now??
(s-value opal:white-fill :foreground-color opal:white)

(create-instance 'color-boxes opal:aggregadget
  (:parts `(	   
    (:white-box ,OPAL:RECTANGLE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-RECTANGLE")
      (:DRAW-FUNCTION :COPY)
      (:FILLING-STYLE ,OPAL:WHITE-FILL)
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:BOX (13 44 52 28 ))
      (:GROW-P T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 13))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 44))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 52))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 28)))
    (:black-box ,OPAL:RECTANGLE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-RECTANGLE")
      (:DRAW-FUNCTION :COPY)
      (:FILLING-STYLE ,OPAL:BLACK-FILL)
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:GROW-P T)
      (:BOX (13 82 52 28 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 13))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 82))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 52))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 28)))
    (:motif-gray-box ,OPAL:RECTANGLE
      (:GILT-REF "TYPE-RECTANGLE")
      (:CONSTANT (T))
      (:FILLING-STYLE ,opal:motif-gray-fill)
      (:GROW-P T)
      (:BOX (13 120 52 28 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 13))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 120))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 52))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 28)))
    (:motif-blue-box ,OPAL:RECTANGLE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-RECTANGLE")
      (:FILLING-STYLE ,opal:motif-blue-fill)
      (:BOX (13 158 52 28 ))
      (:GROW-P T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 13))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 158))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 52))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 28)))
    (:motif-green-box ,OPAL:RECTANGLE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-RECTANGLE")
      (:FILLING-STYLE ,opal:motif-green-fill)
      (:GROW-P T)
      (:BOX (13 196 52 28 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 13))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 196))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 52))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 28)))
    (:motif-orange-box ,OPAL:RECTANGLE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-RECTANGLE")
      (:FILLING-STYLE ,opal:motif-orange-fill)
      (:BOX (13 234 52 28 ))
      (:GROW-P T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 13))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 234))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 52))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 28)))
    (:red-box ,OPAL:RECTANGLE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-RECTANGLE")
      (:FILLING-STYLE ,OPAL:RED-FILL)
      (:BOX (13 272 52 28 ))
      (:GROW-P T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 13))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 272))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 52))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 28)))
    (:green-box ,OPAL:RECTANGLE
      (:GILT-REF "TYPE-RECTANGLE")
      (:CONSTANT (T))
      (:FILLING-STYLE ,OPAL:GREEN-FILL)
      (:GROW-P T)
      (:BOX (180 82 52 28 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 180))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 82))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 52))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 28)))
    (:blue-box ,OPAL:RECTANGLE
      (:GILT-REF "TYPE-RECTANGLE")
      (:CONSTANT (T))
      (:FILLING-STYLE ,OPAL:BLUE-FILL)
      (:BOX (180 120 52 28 ))
      (:GROW-P T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 180))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 120))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 52))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 28)))
    (:yellow-box ,OPAL:RECTANGLE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-RECTANGLE")
      (:FILLING-STYLE ,OPAL:YELLOW-FILL)
      (:GROW-P T)
      (:BOX (180 158 52 28 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 180))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 158))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 52))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 28)))
    (:purple-box ,OPAL:RECTANGLE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-RECTANGLE")
      (:FILLING-STYLE ,OPAL:PURPLE-FILL)
      (:GROW-P T)
      (:BOX (180 196 52 28 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 180))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 196))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 52))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 28)))
    (:cyan-box ,OPAL:RECTANGLE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-RECTANGLE")
      (:FILLING-STYLE ,OPAL:CYAN-FILL)
      (:BOX (180 234 52 28 ))
      (:GROW-P T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 180))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 234))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 52))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 28)))
    (:orange-box ,OPAL:RECTANGLE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-RECTANGLE")
      (:FILLING-STYLE ,OPAL:ORANGE-FILL)
      (:GROW-P T)
      (:BOX (180 272 52 28 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 180))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 272))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 52))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 28))))))

(setf common-lisp-user::*Garnet-Object-Just-Created* 
(create-instance 'COLOR-PROP OPAL:AGGREGADGET
  (:WINDOW-LEFT 0)
  (:WINDOW-TOP 0)
  (:WINDOW-WIDTH 459)
  (:WINDOW-HEIGHT 311)
  (:window-background-color opal:motif-gray)
  (:FUNCTION-FOR-OK `GILT::COLOR-PROP-OK)
  (:EXPORT-P NIL)
  (:WINDOW-TITLE "Color Selection")
  (:PACKAGE-NAME "GILT")
  (:LEFT 0)
  (:TOP 0)
  (:WIDTH (o-formula (GVL :WINDOW :WIDTH) 459))
  (:HEIGHT (o-formula (GVL :WINDOW :HEIGHT) 367))
  (:parts `(
    (:red-slider ,GARNET-GADGETS:MOTIF-SLIDER
      (:GILT-REF "TYPE-MOTIF-SLIDER")
      (:GROW-P T)
      (:CONSTANT (T))
      (:BOX (286 75 48 200 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 286))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 75))
      (:selection-function ,#'gilt::slider-value)
      (:color-type :red)
      (:val-1 0)
      (:VAL-2 100)
      (:page-incr 10)
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 200)))
    (:green-slider ,GARNET-GADGETS:MOTIF-SLIDER
      (:CONSTANT (T))
      (:GILT-REF "TYPE-MOTIF-SLIDER")
      (:BOX (342 75 39 200 ))
      (:GROW-P T)
      (:selection-function ,#'gilt::slider-value)
      (:color-type :GREEN)
      (:val-1 0)
      (:VAL-2 100)
      (:page-incr 10)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 342))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 75))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 200)))
    (:blue-slider ,GARNET-GADGETS:MOTIF-SLIDER
      (:CONSTANT (T))
      (:GILT-REF "TYPE-MOTIF-SLIDER")
      (:selection-function ,#'gilt::slider-value)
      (:color-type :blue)
      (:val-1 0)
      (:VAL-2 100)
      (:page-incr 10)
      (:GROW-P T)
      (:BOX (398 75 39 200 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 398))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 75))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 200)))
    (:current-color ,OPAL:RECTANGLE
      (:GILT-REF "TYPE-RECTANGLE")
      (:CONSTANT (T :except :filling-style))
      (:filling-style ,my-filling1)
      (:other-filling-style ,my-filling2)
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:BOX (217 17 78 40 ))
      (:GROW-P T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 217))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 17))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 78))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 40))
      (:fast-redraw-p :rectangle)
      (:fast-redraw-filling-style ,opal:motif-gray-fill))
    (:okcancel-button ,GARNET-GADGETS:MOTIF-TEXT-BUTTON-PANEL
      (:CONSTANT (T))
      (:SELECTION-FUNCTION GILT:OKCANCEL-FUNCTION)
      (:GILT-REF "TYPE-OKCANCEL")
      (:ITEMS ("OK" "Cancel" ))
      (:FINAL-FEEDBACK-P NIL)
      (:TEXT-OFFSET 5)
      (:DIRECTION :HORIZONTAL)
      (:BOX (331 23 117 24 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 331))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 23)))
    (:labels ,label-texts)
    (:colors ,color-boxes)
    (:i-feedback ,opal:rectangle
      (:obj-over NIL)
      (:visible ,(o-formula (gvl :obj-over)))
      (:left ,(o-formula (- (gvl :obj-over :left) 2)))
      (:top ,(o-formula (- (gvl :obj-over :top) 2)))
      (:width ,(o-formula (+ (gvl :obj-over :width) 4)))
      (:height ,(o-formula (+ (gvl :obj-over :height) 4)))
      (:line-style ,opal:dashed-line))
    (:feedback ,OPAL:RECTANGLE
      (:GILT-REF "TYPE-RECTANGLE")
      (:FILLING-STYLE NIL)
      (:LINE-STYLE ,OPAL:LINE-4)
      (:obj-over NIL)
      (:visible ,(o-formula (gvl :obj-over)))
      (:left ,(o-formula (- (gvl :obj-over :left) 5)))
      (:top ,(o-formula (- (gvl :obj-over :top) 5)))
      (:width ,(o-formula (+ (gvl :obj-over :width) 10)))
      (:height ,(o-formula (+ (gvl :obj-over :height) 10)))
      (:fast-redraw-p :redraw)
      (:fast-redraw-line-style ,(create-instance NIL opal:line-style
				   (:constant T)
				   (:line-thickness 4)
				   (:foreground-color opal:motif-gray))))
      ))
  (:interactors `(
    (:press ,inter:menu-interactor
      (:start-where
        ,(o-formula (list :element-of (gvl :operates-on :colors))))
      (:feedback-obj ,(o-formula (gvl :operates-on :i-feedback)))
      (:final-feedback-obj ,(o-formula (gvl :operates-on :feedback)))
      (:final-function ,#'gilt::definite-color)
      (:window ,(o-formula (gvl :operates-on :window))))))))
