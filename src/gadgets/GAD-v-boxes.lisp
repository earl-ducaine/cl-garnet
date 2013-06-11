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
;;;  GAD-v-boxes
;;;
;;;  This module is a collection of schemata definitions required by vertical
;;;  sliders and veritcal scroll bars.
;;;
;;;  Written by Andrew Mickish

#|
============================================================
Change log:
  5-29-92  Brad Myers - changed to use new auto-repeat button-interactors
  Andrew Mickish - Changed :visible slots of TOP-SCR-TRILL, BOT-SCR-TRILL,
                   TOP-PAGE-TRILL, and BOT-PAGE-TRILL to consider :visible
                   slot of :parent.
============================================================
|#

(in-package "GARNET-GADGETS")


(create-instance 'TOP-SCR-TRILL opal:aggregadget
   (:left (o-formula (gv (kr-path 0 :parent) :trill-box-left)))
   (:top (o-formula (gv (kr-path 0 :parent) :top-scr-top)))
   (:width (o-formula (gv (kr-path 0 :parent) :bound-width)))
   (:height (o-formula (gv (kr-path 0 :parent) :trill-height)))
   (:visible (o-formula (and (gv (kr-path 0 :parent) :scr-trill-p)
			     (gv (kr-path 0 :parent) :visible))))
   (:inc-by (o-formula (gv (kr-path 0 :parent) :scr-incr)))
   (:parts
    `((:frame ,trill-frame)
      (:arrow ,up-arrow)))
   (:interactors
    `((:trill ,trill-inter))))


(create-instance 'BOT-SCR-TRILL opal:aggregadget
   (:left (o-formula (gv (kr-path 0 :parent) :trill-box-left)))
   (:top (o-formula (gv (kr-path 0 :parent) :bot-scr-top)))
   (:width (o-formula (gv (kr-path 0 :parent) :bound-width)))
   (:height (o-formula (gv (kr-path 0 :parent) :trill-height)))
   (:inc-by (o-formula (gv (kr-path 0 :parent) :scr-incr)))
   (:visible (o-formula (and (gv (kr-path 0 :parent) :scr-trill-p)
			     (gv (kr-path 0 :parent) :visible))))
   (:parts
    `((:frame ,trill-frame)
      (:arrow ,down-arrow)))
   (:interactors
    `((:trill ,trill-inter
	      (:extra-function ,#'VAL-2-FN)))))


(create-instance 'TOP-PAGE-TRILL opal:aggregadget
   (:left (o-formula (gv (kr-path 0 :parent) :trill-box-left)))
   (:top (o-formula (gv (kr-path 0 :parent) :top-page-top)))
   (:width (o-formula (gv (kr-path 0 :parent) :bound-width)))
   (:height (o-formula (gv (kr-path 0 :parent) :trill-height)))
   (:inc-by (o-formula (gv (kr-path 0 :parent) :page-incr)))
   (:visible (o-formula (and (gv (kr-path 0 :parent) :page-trill-p)
			     (gv (kr-path 0 :parent) :visible))))
   (:parts
    `((:frame ,trill-frame)
      (:arrow ,page-up-arrow)))
   (:interactors
    `((:trill ,trill-inter))))


(create-instance 'BOT-PAGE-TRILL opal:aggregadget
   (:left (o-formula (gv (kr-path 0 :parent) :trill-box-left)))
   (:top (o-formula (gv (kr-path 0 :parent) :bot-page-top)))
   (:width (o-formula (gv (kr-path 0 :parent) :bound-width)))
   (:height (o-formula (gv (kr-path 0 :parent) :trill-height)))
   (:inc-by (o-formula (gv (kr-path 0 :parent) :page-incr)))
   (:visible (o-formula (and (gv (kr-path 0 :parent) :page-trill-p)
			     (gv (kr-path 0 :parent) :visible))))
   (:parts
    `((:frame ,trill-frame)
      (:arrow ,page-down-arrow)))
   (:interactors
    `((:trill ,trill-inter
	      (:extra-function ,#'VAL-2-FN)))))


;;  Tell the world that GAD-v-boxes has been loaded
;;
(setf (get :garnet-modules :GAD-v-boxes) T)

