;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  GAD-h-boxes
;;;
;;;  This module is a collection of schemata definitions required by horizontal
;;;  scroll bars, horizontal sliders, and the trill device.
;;;
;;;  Written by Andrew Mickish

#|
============================================================
Change log:
  5-29-92  Brad Myers - changed to use new auto-repeat button-interactors
  Andrew Mickish - Changed :visible slots of LEFT-SCR-TRILL,
                   RIGHT-SCR-TRILL, LEFT-PAGE-TRILL, and RIGHT-PAGE-TRILL
                   to consider :visible slot of :parent.
============================================================
|#


(in-package "GARNET-GADGETS")


(create-instance 'LEFT-SCR-TRILL opal:aggregadget
   (:top (o-formula (gv (kr-path 0 :parent) :trill-box-top)))
   (:left (o-formula (gv (kr-path 0 :parent) :left-scr-left)))
   (:height (o-formula (gv (kr-path 0 :parent) :bound-height)))
   (:width (o-formula (gv (kr-path 0 :parent) :trill-width)))
   (:visible (o-formula (and (gv (kr-path 0 :parent) :scr-trill-p)
			     (gv (kr-path 0 :parent) :visible))))
   (:inc-by (o-formula (gv (kr-path 0 :parent) :scr-incr)))
   (:parts
    `((:frame ,trill-frame)
      (:arrow ,left-arrow)))
   (:interactors
    `((:trill ,trill-inter))))


(create-instance 'RIGHT-SCR-TRILL opal:aggregadget
   (:top (o-formula (gv (kr-path 0 :parent) :trill-box-top)))
   (:left (o-formula (gv (kr-path 0 :parent) :right-scr-left)))
   (:height (o-formula (gv (kr-path 0 :parent) :bound-height)))
   (:width (o-formula (gv (kr-path 0 :parent) :trill-width)))
   (:inc-by (o-formula (gv (kr-path 0 :parent) :scr-incr)))
   (:visible (o-formula (and (gv (kr-path 0 :parent) :scr-trill-p)
			     (gv (kr-path 0 :parent) :visible))))
   (:parts
    `((:frame ,trill-frame)
      (:arrow ,right-arrow)))
   (:interactors
    `((:trill ,trill-inter
	      (:extra-function ,#'VAL-2-FN)))))


(create-instance 'LEFT-PAGE-TRILL opal:aggregadget
   (:top (o-formula (gv (kr-path 0 :parent) :trill-box-top)))
   (:left (o-formula (gv (kr-path 0 :parent) :left-page-left)))
   (:height (o-formula (gv (kr-path 0 :parent) :bound-height)))
   (:width (o-formula (gv (kr-path 0 :parent) :trill-width)))
   (:inc-by (o-formula (gv (kr-path 0 :parent) :page-incr)))
   (:visible (o-formula (and (gv (kr-path 0 :parent) :page-trill-p)
			     (gv (kr-path 0 :parent) :visible))))
   (:parts
    `((:frame ,trill-frame)
      (:arrow ,page-left-arrow)))
   (:interactors
    `((:trill ,trill-inter))))


(create-instance 'RIGHT-PAGE-TRILL opal:aggregadget
   (:top (o-formula (gv (kr-path 0 :parent) :trill-box-top)))
   (:left (o-formula (gv (kr-path 0 :parent) :right-page-left)))
   (:height (o-formula (gv (kr-path 0 :parent) :bound-height)))
   (:width (o-formula (gv (kr-path 0 :parent) :trill-width)))
   (:inc-by (o-formula (gv (kr-path 0 :parent) :page-incr)))
   (:visible (o-formula (and (gv (kr-path 0 :parent) :page-trill-p)
			     (gv (kr-path 0 :parent) :visible))))
   (:parts
    `((:frame ,trill-frame)
      (:arrow ,page-right-arrow)))
   (:interactors
    `((:trill ,trill-inter
	      (:extra-function ,#'VAL-2-FN)))))


;;  Tell the world that GAD-h-boxes has been loaded
;;
(setf (get :garnet-modules :GAD-h-boxes) T)
