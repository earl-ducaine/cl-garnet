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
;;;  V-SLIDER-LOADER:  Loads the modules "v-slider" and "parts" modules
;;;                        if necessary

#|
==================================================================
Change log:
   06/18/90 Andrew Mickish - Removed "v-slider-parts" references
   03/22/90 Robert Cook - Define the package "GARNET-GADGETS"
                          for the TI Explorer
   01/30/90 Andrew Mickish - Added check before loading v-slider modules
   10/19/89 Andrew Mickish - Created
==================================================================
|#

(in-package "COMMON-LISP-USER")

;; check first to see if place is set
(unless (boundp 'Garnet-Gadgets-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gadgets-PathName before
  loading Gadgets."))

(unless (get :garnet-modules :v-slider)
  (format t "Loading V-Slider...~%")
  (dolist (pair '((:GAD-scroll-parts "GAD-scroll-parts")
		  (:GAD-slider-parts "GAD-slider-parts")
		  (:GAD-v-arrows "GAD-v-arrows")
		  (:GAD-v-boxes "GAD-v-boxes")
		  (:v-slider "v-slider")))
    (unless (get :garnet-modules (car pair))
      (load (merge-pathnames (cadr pair) Garnet-Gadgets-PathName)
	    :verbose T)))
  (format t "...Done V-Slider.~%"))

(setf (get :garnet-modules :v-slider) t)
