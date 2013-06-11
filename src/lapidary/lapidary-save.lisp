;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file created by GILT V0.2: The Garnet Interface Builder
;;; on Dec 5, 1990, 10:49 AM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
============================================================
Change log:
    12/5/90 Brad Myers - hacked from created file
    12/5/90 Gilt - created
============================================================
|#

(in-package "LAPIDARY")

(create-instance 'SAVE-FILE OPAL:AGGREGADGET
  (:WINDOW-TITLE "Save File")
  (:WINDOW-LEFT 100)
  (:WINDOW-TOP 200)
  (:WINDOW-WIDTH 316)
  (:WINDOW-HEIGHT 228)
  (:PACKAGE-NAME "LAPIDARY")
  (:inter nil) ; indicates if interactor is being saved
  (:FUNCTION-FOR-OK `LAPIDARY::DO-SAVE-FILE)
  (:LEFT 0)
  (:TOP 0)
  (:WIDTH (o-formula (GVL :WINDOW :WIDTH) 316))
  (:HEIGHT (o-formula (GVL :WINDOW :HEIGHT) 228))
  (:parts `(
    (NIL ,OPAL:TEXT
      (:FONT ,(create-instance nil OPAL:FONT
            (:SIZE :LARGE)
            (:FACE :BOLD-ITALIC)))
      (:BOX (9 8 35 14 ))
      (:STRING "Saving...")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 9))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 8)))
    (:FILENAME ,GARNET-GADGETS:SCROLLING-LABELED-BOX
      (:MIN-FRAME-WIDTH NIL)
      (:SELECT-FUNCTION NIL)
      (:FIELD-FONT ,OPAL:DEFAULT-FONT)
      (:FIELD-OFFSET 2)
      (:LABEL-OFFSET 5)
      (:LABEL-FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:BOX (20 40 285 19 ))
      (:LABEL-STRING "Filename:")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 20))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 40))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 285)))
    (:GADGET-NAME ,GARNET-GADGETS:SCROLLING-LABELED-BOX
      (:MIN-FRAME-WIDTH NIL)
      (:SELECT-FUNCTION NIL)
      (:FIELD-FONT ,OPAL:DEFAULT-FONT)
      (:FIELD-OFFSET 2)
      (:LABEL-OFFSET 5)
      (:LABEL-FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:LABEL-STRING "Top-level Gadget name:")
      (:BOX (20 70 285 18 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 20))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 70))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 285)))
    (:WIN-TITLE ,GARNET-GADGETS:SCROLLING-LABELED-BOX
      (:MIN-FRAME-WIDTH NIL)
      (:SELECT-FUNCTION NIL)
      (:FIELD-FONT ,OPAL:DEFAULT-FONT)
      (:FIELD-OFFSET 2)
      (:LABEL-OFFSET 5)
      (:LABEL-FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:LABEL-STRING "Window Title:")
      (:BOX (20 130 285 18 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 20))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 130))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 285)))
    (:PACKAGE-NAME ,GARNET-GADGETS:SCROLLING-LABELED-BOX
      (:MIN-FRAME-WIDTH NIL)
      (:SELECT-FUNCTION NIL)
      (:FIELD-FONT ,OPAL:DEFAULT-FONT)
      (:FIELD-OFFSET 2)
      (:LABEL-OFFSET 5)
      (:LABEL-FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:LABEL-STRING "Package name:")
      (:BOX (20 160 285 18 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 20))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 160))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 285)))
    (NIL ,GARNET-GADGETS:TEXT-BUTTON-PANEL
      (:SELECTION-FUNCTION lapidary::OKCANCEL-FUNCTION)
      (:GILT-REF "TYPE-OKCANCEL")
      (:SELECT-FUNCTION LAPIDARY::OKCANCEL-FUNCTION)
      (:ITEMS ("OK" "Cancel" ))
      (:GRAY-WIDTH 3)
      (:FINAL-FEEDBACK-P NIL)
      (:TEXT-OFFSET 2)
      (:SHADOW-OFFSET 5)
      (:DIRECTION :HORIZONTAL)
      (:BOX (188 6 117 29 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 188))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 6)))
    (:EXPORT-P ,GARNET-GADGETS:X-BUTTON-panel
      (:INDENT 0)
      (:V-ALIGN :TOP)
      (:H-SPACING 5)
      (:DIRECTION :VERTICAL)
      (:SELECT-FUNCTION NIL)
      (:H-ALIGN :RIGHT)
      (:GRAY-WIDTH 3)
      (:TEXT-ON-LEFT-P T)
      (:V-SPACING 5)
      (:BUTTON-HEIGHT 20)
      (:FIXED-HEIGHT-P NIL)
      (:PIXEL-MARGIN NIL)
      (:RANK-MARGIN NIL)
      (:TEXT-OFFSET 5)
      (:SHADOW-OFFSET 5)
      (:BUTTON-WIDTH 20)
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:FIXED-WIDTH-P T)
      (:BOX (20 100 197 25 ))
      (:items ("Export Gadgets?"))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 20))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 100)))
    (:WIN-VALIDP ,OPAL:RECTANGLE
      (:visible ,(o-formula (or (gvl :parent :inter)
				(gv *selection-info* :selected))))
      (:DRAW-FUNCTION :AND)
      (:FILLING-STYLE ,OPAL:GRAY-FILL)
      (:LINE-STYLE NIL)
      (:BOX (14 126 297 30 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 14))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 126))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 297))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 30)))
    (:GAD-VALIDP ,OPAL:RECTANGLE
      (:visible ,(o-formula (or (gvl :parent :inter)
				(gv *selection-info* :selected))))
      (:DRAW-FUNCTION :AND)
      (:FILLING-STYLE ,OPAL:GRAY-FILL)
      (:LINE-STYLE NIL)
      (:BOX (14 66 297 30 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 14))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 66))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 297))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 30))))))

