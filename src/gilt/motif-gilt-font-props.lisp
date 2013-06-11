;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GILT; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file created by the Garnet Interface Builder
;;; on Jun 28, 1990, 2:01 PM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
=============================================================================
Change Log:
    2/19/92 Brad Myers - added constant definitions
    1/09/92 Andrew Mickish - Created FONT-PROP as a named gadget instead of
              unnamed inside a creator function
    03/04/91 Andrew Mickish - Changed opal:multi-text objects to opal:text
    02/28/91 Osamu Hashimoto - Changed into Motif-version
    02/22/91 Osamu Hashimoto - separated from "implementations"
=============================================================================
|#

(in-package "GILT")

(create-instance 'FONT-PROP opal:aggregadget
  (:window-width 330)
  (:window-height 190)
  (:window-title "Font Selection")
  (:window-background-color opal:motif-gray)
  (:parts `(
    (:OKcancel ,GARNET-GADGETS:MOTIF-TEXT-BUTTON-PANEL
      (:CONSTANT (T))
      (:DIRECTION :HORIZONTAL)
      (:SHADOW-OFFSET 5)
      (:TEXT-OFFSET 5)
      (:final-feedback-p NIL)
      (:GRAY-WIDTH 3)
      (:ITEMS (("OK" Font-Dialog-OK-Func) ("Cancel" Font-Dialog-Cancel-Func) ))
      (:LEFT 194)
      (:TOP 5))
    (NIL ,OPAL:TEXT
      (:CONSTANT (T))
      (:STRING "Font:")
      (:Font ,(create-instance NIL opal:font
		 (:constant '(T))
		 (:face :bold-italic)(:size :large)))
      (:LEFT 2)
      (:TOP 2))
    (:Family ,GARNET-GADGETS:MOTIF-RADIO-BUTTON-PANEL
      (:CONSTANT (T))
      (:ITEMS (:Fixed :Serif :Sans-Serif))
      (:text-on-left-p T)
      (:LEFT 14)
      (:TOP 57))
    (NIL ,OPAL:TEXT
      (:CONSTANT (T))
      (:STRING "Family")
      (:LEFT 16)
      (:TOP 39))
    (NIL ,OPAL:TEXT
      (:CONSTANT (T))
      (:STRING "Face")
      (:LEFT 138)
      (:TOP 39))
    (:face ,GARNET-GADGETS:MOTIF-CHECK-BUTTON-PANEL
      (:CONSTANT (T))
      (:ITEMS (:Italic :Bold))
      (:text-on-left-p T)
      (:LEFT 124)
      (:TOP 64))
    (NIL ,OPAL:TEXT
      (:CONSTANT (T))
      (:STRING "Size")
      (:LEFT 233)
      (:TOP 39))
    (:Size ,GARNET-GADGETS:MOTIF-RADIO-BUTTON-PANEL
      (:CONSTANT (T))
      (:ITEMS (:Small :Medium :Large :Very-Large))
      (:text-on-left-p T)
      (:LEFT 216)
      (:TOP 57))
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:X1 205)
      (:Y1 41)
      (:X2 205)
      (:Y2 175))
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:X1 114)
      (:Y1 39)
      (:X2 114)
      (:Y2 172))
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:X1 10)
      (:Y1 54)
      (:X2 307)
      (:Y2 54)))))


