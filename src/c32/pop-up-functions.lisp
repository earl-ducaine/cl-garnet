;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: C32; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file created by GILT V0.4: The Garnet Interface Builder
;;; on Apr 11, 1991, 4:37 PM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id::                                                     $

;;; This file uses the following objects:
;;;     SCROLLING-MENU from package GARNET-GADGETS

;;; CHANGE LOG
;;;
;;; 09/01/92 Andrew Mickish - Removed extra Gilt variables that caused
;;;            compiler warnings.

#||

(dolist (gadget '("scrolling-menu-loader"
		  ))
  (load (common-lisp-user::garnet-pathnames gadget
			 common-lisp-user::Garnet-Gadgets-PathName)))
;;;
;;;     Functions needed from Gilt
(load (common-lisp-user::garnet-pathnames "gilt-functions-loader"
			 common-lisp-user::Garnet-Gilt-PathName))
;;;
||#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "C32")


(defun ignore-all (&rest stuff)
  (declare (ignore stuff))
  )

      (create-instance 'POP-UP-FUNCTIONS OPAL:AGGREGADGET
	(:WINDOW-LEFT 100)
	(:WINDOW-TOP 300)
	(:WINDOW-WIDTH 197)
	(:WINDOW-HEIGHT 168)
	(:FUNCTION-FOR-OK NIL)
	(:EXPORT-P T)
	(:WINDOW-TITLE "Function:")
	(:PACKAGE-NAME "C32")
	(:LEFT 0)
	(:TOP 0)
	(:WIDTH (o-formula (GVL :WINDOW :WIDTH) 450))
	(:HEIGHT (o-formula (GVL :WINDOW :HEIGHT) 300))
	(:parts
	 `((:menu ,GARNET-GADGETS:SCROLLING-MENU
	    (:GILT-REF "TYPE-SCROLLING-MENU")
	    (:MENU-SELECTION-FUNCTION nil
	     #+COMMENT C32::Insert-Func-Into-Form)
	    (:FINAL-FEEDBACK-P T)
	    (:H-ALIGN :CENTER)
	    (:NUM-VISIBLE 8)
	    (:INT-SCROLL-FEEDBACK-P NIL)
	    (:TITLE NIL)
	    (:ITEMS ("+" "-" "floor" "*" "min" "max" "if" "eq"
			 "opal:gv-center-x" "opal:gv-center-y"
			 "opal:gv-right" "opal:gv-bottom"
			 "opal:gv-center-x-is-center-of"
			 "opal:gv-center-y-is-center-of"
			 "opal:gv-right-is-left-of"
			 "opal:gv-bottom-is-top-of"))
	    (:BOX (0 0 95 273 ))
	    (:LEFT ,(o-formula (FIRST (GVL :BOX)) 0))
	    (:TOP ,(o-formula (SECOND (GVL :BOX)) 0))))))
