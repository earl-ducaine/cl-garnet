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

#|
==================================================================
Change log:
;;; 10/2/03 RGA --- Added scrolling-unlabelled-box
         06/23/93 Andrew Mickish - Added polyline-functions
         02/24/93 Andrew Mickish - Removed references to compile-opal/inter-p
         02/10/93 Mickish - Added motif-trill-device, menubar-functions, and
                            motif-menubar
         01/10/93 Brad Myers - added standard-edit
         08/17/92 Mickish - Added save-gadget and load-gadget
         08/05/92 Mickish - Added option-button and motif-option-button
         04/08/92 Mickish - Added error-gadget-utils and motif-error-gadget
         04/02/92 McDaniel - New multifont-gadget.
         02/20/92 Andrew Mickish - Moved GARNET-GADGETS package
                         definition into Garnet-Loader.
         05/28/91 Andrew Mickish - Added menubar
	 03/14/91 Edward Pervin - Added motif-gauge
         01/17/91 Andrew Mickish - Added motif gadgets
         08/10/90 Pavan Reddy - Changed "multi-feedback" to "polyline-creator"
         08/07/90 Pavan Reddy - Added "multi-feedback" and "scrolling-window"
         07/16/90 Andrew Mickish - Added "browser-gadget"
         06/18/90 Andrew Mickish - Removed "v-slider-parts" and added
                       "scrolling-input-string" and "scrolling-labeled-box"
	 03/22/90 Robert Cook - Define the package "GARNET-GADGETS"
				for the TI Explorer
         03/26/90 Andrew Mickish - Added scrolling-menu and error-gadget
         01/05/90 Andrew Mickish - Added setf's and provide's
	 1/4/90   Ed Pervin - Added version number
         10/19/89 Andrew Mickish - Updated to include entire gadgets
         08/11/89 Brad Myers - Put into standard form
         08/01/89 Andrew Mickish - created
==================================================================
|#

(in-package :COMMON-LISP-USER)

(defparameter Gadgets-Version-Number "1.0")

(format t "Loading Gadgets...~%")

;; check first to see if place is set
(unless (boundp 'Garnet-Gadgets-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gadgets-PathName before loading Gadgets."))


;; Only loads this file when not compiling all of Garnet.
(unless (get :garnet-modules :multifont)
  (load (merge-pathnames "multifont-loader"
			 #+cmu "opal:"
			 #+(not cmu) Garnet-Opal-PathName)))

;; ---- Load gadgets files

(Defvar Garnet-Gadgets-Files   ;; defvar rather than defparameter so can setq
                               ;; this variable before loading if only want
			       ;; to compile some of these files
  '(
    "GAD-scroll-parts"    ;;  Helper modules containing definitions for 
    "GAD-slider-parts"    ;;    scroll bar and slider objects
    "GAD-v-arrows"
    "GAD-v-boxes"
    "GAD-h-arrows"
    "GAD-h-boxes"

    "v-scroll-bar"
    "h-scroll-bar"
    "v-slider"
    "h-slider"
    "trill-device"       ;;  A horizontal slider without the shaft

    "GAD-button-parts"    ;;  Helper module for button and menu objects
    "x-buttons"
    "text-buttons"
    "radio-buttons"

    "error-gadget-utils"
    "error-gadget"
    "scrolling-menu"

    "scrolling-input-string"
    "scrolling-labeled-box"
    "scrolling-unlabeled-box"

    "gauge"              ;;  Semi-circular gauge
    "menu"
    "menubar-functions" "menubar"
    "labeled-box"        ;;  A box with editable text and a label
    "arrow-line"         ;;  A line/arrowhead combination
    "graphics-selection" ;;  Selection squares for move-grow interaction
    "option-button"
    "popup-menu-button"
    "save-load-functions"
    "save-gadget"
    "load-gadget"

    "browser-gadget"
    "polyline-functions"
    "polyline-creator"
    "multi-selection"

    "scrolling-window-parts"
    "scrolling-window"

    "standard-edit"
    "mouseline"
    
    "prop-value-gadgets"
    "prop-sheet"
    "prop-sheet-win"

    "motif-parts"
    "motif-v-scroll-bar"
    "motif-h-scroll-bar"
    "motif-slider"
    "motif-trill-device"
    "motif-text-buttons"
    "motif-check-buttons"
    "motif-radio-buttons"
    "motif-menu"
    "motif-gauge"
    "motif-scrolling-labeled-box"
    "motif-prop-sheet-win"
    "motif-scrolling-window"
    "motif-error-gadget"
    "motif-option-button"
    "motif-scrolling-menu"
    "motif-save-gadget"
    "motif-load-gadget"
    "motif-menubar"

    "multifont-gadget"
    ))


(dolist (file Garnet-Gadgets-Files)
  (garnet-load (concatenate 'string "gadgets:" file)))


(setf (get :garnet-modules :gadgets)  t)
(setf (get :garnet-modules :GAD-scroll-parts) t)
(setf (get :garnet-modules :GAD-slider-parts) t)
(setf (get :garnet-modules :GAD-v-arrows) t)
(setf (get :garnet-modules :GAD-v-boxes) t)
(setf (get :garnet-modules :GAD-h-arrows) t)
(setf (get :garnet-modules :GAD-h-boxes) t)
(setf (get :garnet-modules :v-scroll-bar) t)
(setf (get :garnet-modules :h-scroll-bar) t)
(setf (get :garnet-modules :v-slider) t)
(setf (get :garnet-modules :h-slider) t)
(setf (get :garnet-modules :trill-device) t)
(setf (get :garnet-modules :GAD-button-parts) t)
(setf (get :garnet-modules :x-buttons) t)
(setf (get :garnet-modules :text-buttons) t)
(setf (get :garnet-modules :radio-buttons) t)
(setf (get :garnet-modules :error-gadget-utils) t)
(setf (get :garnet-modules :error-gadget) t)
(setf (get :garnet-modules :scrolling-menu) t)
(setf (get :garnet-modules :gauge) t)
(setf (get :garnet-modules :menu) t)
(setf (get :garnet-modules :menubar) t)
(setf (get :garnet-modules :labeled-box) t)
(setf (get :garnet-modules :arrow-line) t)
(setf (get :garnet-modules :graphics-selection) t)
(setf (get :garnet-modules :option-button) t)
(setf (get :garnet-modules :popup-menu-button) t)
(setf (get :garnet-modules :save-load-functions) t)
(setf (get :garnet-modules :save-gadget) t)
(setf (get :garnet-modules :load-gadget) t)
(setf (get :garnet-modules :browser-gadget) t)
(setf (get :garnet-modules :polyline-functions) t)
(setf (get :garnet-modules :polyline-creator) t)
(setf (get :garnet-modules :scrolling-window-parts) t)
(setf (get :garnet-modules :scrolling-window) t)
(setf (get :garnet-modules :standard-edit) t)
(setf (get :garnet-modules :mouseline) t)
(setf (get :garnet-modules :scrolling-input-string) t)
(setf (get :garnet-modules :scrolling-labeled-box) t)
(setf (get :garnet-modules :scrolling-unlabeled-box) t)
(setf (get :garnet-modules :multi-selection) t)
(setf (get :garnet-modules :prop-value) t)
(setf (get :garnet-modules :prop-sheet) t)
(setf (get :garnet-modules :prop-sheet-win) t)
(setf (get :garnet-modules :motif-parts) t)
(setf (get :garnet-modules :motif-v-scroll-bar) t)
(setf (get :garnet-modules :motif-h-scroll-bar) t)
(setf (get :garnet-modules :motif-slider) t)
(setf (get :garnet-modules :motif-trill-device) t)
(setf (get :garnet-modules :motif-text-buttons) t)
(setf (get :garnet-modules :motif-check-buttons) t)
(setf (get :garnet-modules :motif-radio-buttons) t)
(setf (get :garnet-modules :motif-gauge) t)
(setf (get :garnet-modules :motif-menu) t)
(setf (get :garnet-modules :motif-menubar) t)
(setf (get :garnet-modules :motif-scrolling-labeled-box) t)
(setf (get :garnet-modules :motif-scrolling-window) t)
(setf (get :garnet-modules :motif-prop-sheet-win) t)
(setf (get :garnet-modules :motif-error-gadget) t)
(setf (get :garnet-modules :motif-option-button) t)
(setf (get :garnet-modules :motif-scrolling-menu) t)
(setf (get :garnet-modules :motif-save-gadget) t)
(setf (get :garnet-modules :motif-load-gadget) t)
(setf (get :garnet-modules :multifont-gadget) t)

(format t "...Done Gadgets.~%")

