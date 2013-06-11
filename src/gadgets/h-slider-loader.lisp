;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;
;;         The Garnet User Interface Development Environment.        ;;
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;

;;; $Id::                                                             $
;;
;;
;;  H-SLIDER-LOADER:  Loads the modules "h-slider" and any necessary
;;                    "parts" modules


;;; ==============================================================
;; Change log:
;;       03/22/90 Robert Cook - Define the package "GARNET-GADGETS"
;;                              for the TI Explorer
;;       01/30/90 Andrew Mickish - Added check before loading h-slider
;;       10/19/89 Andrew Mickish - Created
;;  ==============================================================


(in-package "COMMON-LISP-USER")

;; check first to see if place is set
(unless (boundp 'Garnet-Gadgets-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gadgets-PathName before
  loading Gadgets."))

(unless (get :garnet-modules :h-slider)
  (format t "Loading H-Slider...~%")
  (dolist (pair '((:GAD-scroll-parts "GAD-scroll-parts")
		  (:GAD-slider-parts "GAD-slider-parts")
		  (:GAD-h-arrows "GAD-h-arrows")
		  (:GAD-h-boxes "GAD-h-boxes")
		  (:h-slider "h-slider")))
    (unless (get :garnet-modules (car pair))
      (load (merge-pathnames (cadr pair) Garnet-Gadgets-PathName)
	    :verbose T)))
  (format t "...Done H-Slider.~%"))


(setf (get :garnet-modules :h-slider) t)


