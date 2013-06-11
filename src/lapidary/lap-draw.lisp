;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file created by GILT V0.4: The Garnet Interface Builder
;;; on Jan 22, 1992, 5:39 PM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "LAPIDARY")

;;; This file uses the following objects:
;;;     TEXT-BUTTON-PANEL from package GARNET-GADGETS
;;;     MULTI-TEXT from package OPAL
;;;     RADIO-BUTTON-PANEL from package GARNET-GADGETS
;;;     RECTANGLE from package OPAL
(dolist (gadget '("radio-buttons-loader"
		  "text-buttons-loader"
		  ))
  (load (merge-pathnames gadget
			 common-lisp-user::Garnet-Gadgets-PathName)))
;;;
;;;     Functions needed from Gilt
(common-lisp-user::garnet-load "gilt:gilt-functions-loader")
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun draw-fct-sel-fct (gadget value)
  (s-value (g-value gadget :parent) :value value))

(setf common-lisp-user::*Garnet-Object-Just-Created* 
(create-instance 'draw-fct-gadget OPAL:AGGREGADGET
  (:value nil)
  (:WINDOW-LEFT 0)
  (:WINDOW-TOP 0)
  (:WINDOW-WIDTH 435)
  (:WINDOW-HEIGHT 300)
  (:FUNCTION-FOR-OK `LAPIDARY::DRAW-FCT-HANDLER)
  (:EXPORT-P T)
  (:WINDOW-TITLE "draw-function")
  (:PACKAGE-NAME "LAPIDARY")
  (:LEFT 0)
  (:TOP 0)
  (:WIDTH (o-formula (GVL :WINDOW :WIDTH) 435))
  (:HEIGHT (o-formula (GVL :WINDOW :HEIGHT) 300))
  (:parts `(
    (nil ,OPAL:RECTANGLE
      (:constant (t))
      (:LEFT 13)
      (:TOP 110)
      (:WIDTH 409)
      (:HEIGHT 169))
    (:primary ,GARNET-GADGETS:RADIO-BUTTON-PANEL
      (:constant (t))
      (:INDENT 0)
      (:H-SPACING 5)
      (:GRAY-WIDTH 3)
      (:TEXT-ON-LEFT-P T)
      (:V-SPACING 5)
      (:FIXED-HEIGHT-P T)
      (:DIRECTION :HORIZONTAL)
      (:PIXEL-MARGIN NIL)
      (:RANK-MARGIN NIL)
      (:TEXT-OFFSET 5)
      (:selection-function draw-fct-sel-fct)
      (:value ,(o-formula (when (member (gvl :parent :value)
					(g-value kr::*schema-self* :items))
				(gvl :parent :value))))
      (:SHADOW-OFFSET 5)
      (:BUTTON-DIAMETER 23)
      (:FONT ,OPAL:DEFAULT-FONT)
      (:FIXED-WIDTH-P T)
      (:ITEMS (:COPY :XOR :AND :OR ))
      (:LEFT 10)
      (:TOP 61))
    (:other ,GARNET-GADGETS:RADIO-BUTTON-PANEL
      (:constant (t))
      (:INDENT 0)
      (:value ,(o-formula (when (null (gvl :parent :primary :value))
				(gvl :parent :value))))
      (:selection-function draw-fct-sel-fct)
      (:H-SPACING 5)
      (:GRAY-WIDTH 3)
      (:TEXT-ON-LEFT-P T)
      (:V-SPACING 5)
      (:FIXED-HEIGHT-P T)
      (:DIRECTION :HORIZONTAL)
      (:PIXEL-MARGIN NIL)
      (:RANK-MARGIN 3)
      (:TEXT-OFFSET 5)
      (:SHADOW-OFFSET 5)
      (:BUTTON-DIAMETER 23)
      (:FONT ,OPAL:DEFAULT-FONT)
      (:FIXED-WIDTH-P T)
      (:ITEMS (:CLEAR :SET :NO-OP :COPY-INVERTED :INVERT :EQUIV :NAND :NOR :AND-INVERTED :AND-REVERSE :OR-INVERTED :OR-REVERSE ))
      (:LEFT 20)
      (:TOP 130))
    (NIL ,OPAL:TEXT
      (:constant (t))
      (:FONT ,(create-instance nil OPAL:FONT
            (:SIZE :VERY-LARGE)
            (:FACE :BOLD-ITALIC)
            (:FAMILY :SERIF)))
      (:STRING "Draw Function")
      (:LEFT 10)
      (:TOP 15))
    (NIL ,OPAL:RECTANGLE
      (:constant (t))
      (:FILLING-STYLE ,OPAL:WHITE-FILL)
      (:LINE-STYLE NIL)
      (:LEFT 45)
      (:TOP 98)
      (:WIDTH 52)
      (:HEIGHT 24))
    (NIL ,OPAL:TEXT
      (:constant (t))
      (:STRING "Others")
      (:LEFT 50)
      (:TOP 102))
    (NIL ,GARNET-GADGETS:TEXT-BUTTON-PANEL
      (:constant (t))
      (:SELECTION-FUNCTION GILT:OKCANCEL-FUNCTION)
      (:ITEMS ("OK" "Apply" "Cancel" ))
      (:GRAY-WIDTH 3)
      (:FINAL-FEEDBACK-P NIL)
      (:TEXT-OFFSET 2)
      (:SHADOW-OFFSET 5)
      (:DIRECTION :HORIZONTAL)
       (:LEFT 197)
      (:TOP 13)))))

)


;;; ===============================
;;; draw the selections with
;;; the designated draw function
;;; ===============================

(defun draw-fct-handler (gadget values)
  (declare (ignore values))
  (let ((value (g-value gadget :value)))
;  (reset-undo)
  (dolist (obj (g-value *selection-info* :selected))
;      (undo-save obj :draw-function)
	  (destroy-constraint obj :draw-function)
	  (s-value obj :draw-function value))))
