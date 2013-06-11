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
;;; This file created by GILT V0.2: The Garnet Interface Builder
;;; on Dec 5, 1990, 10:55 AM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
============================================================
Change log:
   7/01/93 Andrew Mickish - Moved Show-Read-Dialog here from gilt.lisp
   2/19/92 Brad Myers - added constant definitions
   3/13/91 Osamu Hashimoto - changed grayout from rectangle-covering to :active-p
   2/27/91 Osamu Hashimoto - changed into Motif-version
   12/5/90 Brad Myers - hacked from created file
   12/5/90 Gilt - created
============================================================
|#

(in-package "GILT")

(create-instance 'READ-FILE OPAL:AGGREGADGET
  (:WINDOW-TITLE "Read File")
  (:WINDOW-LEFT 120)
  (:WINDOW-TOP 220)
  (:WINDOW-WIDTH 316)
  (:WINDOW-HEIGHT 139)
  (:window-background-color opal:motif-gray)
  (:PACKAGE-NAME "GILT")
  (:FUNCTION-FOR-OK `GILT::DO-READ-FILE)
  (:LEFT 0)
  (:TOP 0)
  (:WIDTH (o-formula (GVL :WINDOW :WIDTH) 316))
  (:HEIGHT (o-formula (GVL :WINDOW :HEIGHT) 139))
  (:parts `(
    (NIL ,OPAL:TEXT
      (:CONSTANT (T))
      (:FONT ,(create-instance nil OPAL:FONT
	    (:CONSTANT '(T))
            (:SIZE :LARGE)
            (:FACE :BOLD-ITALIC)))
      (:BOX (9 8 35 14 ))
      (:STRING "Reading...")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 9))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 8)))
    (:FILENAME ,GARNET-GADGETS:motif-SCROLLING-LABELED-BOX
      (:CONSTANT (T))
      (:GILT-REF "TYPE-SCROLLING-LABELED-BOX")  ;....
      (:MIN-FRAME-WIDTH NIL)
      (:SELECT-FUNCTION NIL)
      (:FIELD-FONT ,OPAL:DEFAULT-FONT)
      (:FIELD-OFFSET 2)
      (:LABEL-OFFSET 5)
      (:LABEL-FONT ,(create-instance nil OPAL:FONT
            (:CONSTANT '(T))
            (:FACE :BOLD)))
      (:BOX (20 40 285 19 ))
      (:LABEL-STRING "Filename:")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 20))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 40))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 285)))
    (NIL ,GARNET-GADGETS:MOTIF-TEXT-BUTTON-PANEL
      (:CONSTANT (T))
      (:SELECTION-FUNCTION GILT:OKCANCEL-FUNCTION)
      (:GILT-REF "TYPE-OKCANCEL")
      (:SELECT-FUNCTION GILT:OKCANCEL-FUNCTION)
      (:ITEMS ("OK" "Cancel" ))
      (:GRAY-WIDTH 3)
      (:FINAL-FEEDBACK-P NIL)
      (:TEXT-OFFSET 5)
      (:SHADOW-OFFSET 5)
      (:DIRECTION :HORIZONTAL)
      (:BOX (188 6 117 29 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 188))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 6)))
    (:ADD-REPLACE ,GARNET-GADGETS:MOTIF-RADIO-BUTTON-PANEL
      (:CONSTANT (T :except :active-p))
      (:GILT-REF "TYPE-MOTIF-RADIO-BUTTON-PANEL")
      (:INDENT 0)
      (:V-ALIGN :TOP)
      (:H-SPACING 5)
      (:DIRECTION :VERTICAL)
      (:SELECT-FUNCTION NIL)
      (:H-ALIGN :RIGHT)
      (:GRAY-WIDTH 3)
      (:TEXT-ON-LEFT-P T)
      (:V-SPACING 5)
      (:FIXED-HEIGHT-P T)
      (:PIXEL-MARGIN NIL)
      (:RANK-MARGIN NIL)
      (:TEXT-OFFSET 5)
      (:SHADOW-OFFSET 5)
      (:BUTTON-DIAMETER 23)
      (:FONT ,(create-instance nil OPAL:FONT
            (:CONSTANT '(T))
            (:FACE :BOLD)))
      (:FIXED-WIDTH-P T)
      (:BOX (22 68 129 53 ))
      (:ITEMS ("Add to existing objects" "Replace existing objects" ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 22))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 68))))))


;;; This pops up the read dialog box, after determining the default values 
(defun Show-Read-Dialog (&rest args)
  (declare (ignore args))
  (let ((filename *Last-Filename*)
	(add-replace-invalid (if (g-value *objs-agg* :components) NIL T)))
    (set-initial-value read-file :filename filename)
    (s-value (g-value read-file :add-replace) :active-p
	     (not add-replace-invalid))
    (set-initial-value read-file :add-replace "Replace existing objects")
    (show-in-window read-file)))

