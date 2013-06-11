;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id::                                                             $

;;;  MOTIF-SLIDER-LOADER:  Loads the gadgets module "motif-slider" and
;;;  "parts" modules if required.

#|
==================================================================
Change log:
   1/17/91 Andrew Mickish - Created
==================================================================
|#

(in-package "COMMON-LISP-USER")

;; check first to see if place is set
(unless (boundp 'Garnet-Gadgets-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gadgets-PathName before
  loading Gadgets."))

(unless (get :garnet-modules :motif-slider)
  (format t "Loading Motif-Slider...~%")
  (dolist (pair '((:motif-parts "motif-parts")
		  (:motif-v-scroll-bar "motif-v-scroll-loader")
		  (:motif-slider "motif-slider")))
    (unless (get :garnet-modules (car pair))
      (load (merge-pathnames (cadr pair) Garnet-Gadgets-PathName)
	    :verbose T)))
  (format t "...Done Motif-Slider.~%"))


(setf (get :garnet-modules :motif-slider) t)

