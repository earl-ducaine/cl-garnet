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

;;; CHANGE LOG:
;;; 10/5/03 RGA --- Long filenames and MCL.
;;; 10/2/03 RGA --- New compile/load protocol
;;; 10/2/03 RGA --- Added scrolling-unlabeled-box
;;;   7/28/96 RGA --- changed to use garnet-compile/load
;;;  6/23/93 Andrew Mickish - Added "polyline-functions"
;;;  3/20/93 Andrew Mickish - Moved "standard-edit" after "motif-parts" to
;;;                    avoid a compiler warning (reference to Motif-Background)
;;;  2/24/93 Andrew Mickish - Removed references to compile-opal/inter-p
;;;  2/10/93 Mickish - Added motif-trill-device, menubar-functions, and
;;;                    motif-menubar
;;;  1/10/93 Brad Myers - added standard-edit
;;; 08/27/92 Mickish - Added motif-scrolling-menu
;;; 08/17/92 Mickish - Added save-gadget and load-gadget
;;; 08/05/92 Mickish - Added option-button and motif-option-button
;;; 04/08/92 Mickish - Added error-gadget-utils and motif-error-gadget
;;; 04/02/92 McDaniel - New multifont-gadget.
;;; 02/20/92 Mickish - Moved make-package call into Garnet-Loader.
;;; 03/26/91 Pervin - Load compiled files in Lucid.
;;; 03/22/91 Mickish, Pervin - Added all the provides and setf's at end.
;;; 03/15/91 Andrew Mickish - Added scrolling-window-parts and
;;;                           motif-scrolling-window
;;; 03/14/91 Edward Pervin - Added motif-gauge.
;;; 02/28/91 Edward Pervin - Call (gc t) after every load in allegro.
;;;			     Also, added motif-menu.
;;; 01/17/91 Andrew Mickish - Added motif gadgets
;;; 08/10/90 Pavan Reddy - Changed "multi-feedback" to "polyline-creator"
;;; 08/07/90 Pavan Reddy - Added "multi-feedback" and "scrolling-window"
;;; 07/16/90 Andrew Mickish - Added "browser-gadget"
;;; 06/18/90 Andrew Mickish - Removed "v-slider-parts" and added
;;;             "scrolling-input-string" and "scrolling-labeled-box"
;;; 06/05/90 Richardson - Added lispworks
;;; 04/16/90 Pervin - Load each file after compiling in Allegro.
;;; 04/12/90 Mitchell - Added #+allegro (gc t)
;;; 03/26/90 Andrew Mickish - Added scrolling-menu and error-gadget
;;; 03/22/90 Robert Cook - Define the package "GARNET-GADGETS"
;;;			   for the TI Explorer
;;; 02/16/90 - Andrew Mickish - Removed defvar for Garnet-Gadgets-Pathname

(in-package :COMMON-LISP-USER)

(defvar *debug-gadgets-mode* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim
   (if *debug-gadgets-mode*
       (and (boundp 'Garnet-Compile-Debug-Settings)
	    Garnet-Compile-Debug-Settings)
       ;; Global default settings.
       (and (boundp 'Default-Garnet-Proclaim) 
	    Default-Garnet-Proclaim))))


;; Only loads this file when not compiling all of Garnet.
(unless (get :garnet-modules :multifont)
  (load (merge-pathnames "multifont-loader" Garnet-Opal-PathName))) ; The pathname is CORRECT!

(eval-when (:load-toplevel :compile-toplevel :execute)
  (garnet-mkdir-if-needed Garnet-Gadgets-Pathname))
		    
(Defvar Garnet-Gadgets-Files
  '(
    "GAD-scroll-parts"			; Helper modules containing definitions for 
    "GAD-slider-parts"			; scroll bar and slider objects
    "GAD-v-arrows"
    "GAD-v-boxes"
    "GAD-h-arrows"
    "GAD-h-boxes"

    "v-scroll-bar"
    "h-scroll-bar"
    "v-slider"
    "h-slider"
    "trill-device"			; A horizontal slider without the shaft

    "GAD-button-parts"			; Helper module for button and menu objects
    "x-buttons"
    "text-buttons"
    "radio-buttons"

    "error-gadget-utils"
    "error-gadget"
    "scrolling-menu"

    "scrolling-input-string"
    "scrolling-labeled-box"
    "scrolling-unlabeled-box"

    "gauge"				; Semi-circular gauge
    "menu"
    "labeled-box"			; A box with editable text and a label
    "arrow-line"			; A line/arrowhead combination
    "graphics-selection"		; Selection squares for move-grow interaction
    "option-button"
    "popup-menu-button"
    "save-load-functions"
    "save-gadget"
    "load-gadget"

    "browser-gadget"
    "polyline-functions"
    "polyline-creator"
    "multi-selection"
    "menubar-functions" "menubar"

    "scrolling-window-parts"
    "scrolling-window"

    "prop-value-gadgets"
    "prop-sheet"
    "prop-sheet-win"

    "motif-parts"
    "motif-v-scroll-bar"
    "motif-h-scroll-bar"
    "motif-trill-device"
    "motif-slider"
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
    "scrolling-window-multifont"
    "standard-edit"
    "mouseline"
    ))

(dolist (file Garnet-Gadgets-Files)
  (let ((gadget-str (concatenate 'string "gadgets:" file)))
    (garnet-compile gadget-str)
    (garnet-load gadget-str)))

(format t "Copying gadget loader files to ~A~%" Garnet-Gadgets-Pathname)

(garnet-copy-files Garnet-Gadgets-Src Garnet-Gadgets-Pathname
		   '("arrow-line-loader.lisp"
		     "browser-gadget-loader.lisp"
		     "error-gadget-loader.lisp"
		     "gadgets-loader.lisp"
		     "gauge-loader.lisp"
		     "graphics-loader.lisp"
		     "h-scroll-loader.lisp"
		     "h-slider-loader.lisp"
		     "labeled-box-loader.lisp"
		     "menu-loader.lisp"
		     "menubar-loader.lisp"
		     "motif-check-buttons-loader.lisp"
		     "motif-error-gadget-loader.lisp"
		     "motif-gauge-loader.lisp"
		     "motif-h-scroll-loader.lisp"
		     "motif-menu-loader.lisp"
		     "motif-menubar-loader.lisp"
		     "motif-option-button-loader.lisp"
		     "motif-prop-sheet-win-loader.lisp"
		     "motif-radio-buttons-loader.lisp"
		     "motif-save-gadget-loader.lisp"
		     "motif-scrolling-labeled-box-loader.lisp"
		     "motif-scrolling-menu-loader.lisp"
		     "motif-scrolling-window-loader.lisp"
		     "motif-slider-loader.lisp"
		     "motif-text-buttons-loader.lisp"
		     "motif-trill-device-loader.lisp"
		     "motif-v-scroll-loader.lisp"
		     "mouseline-loader.lisp"
		     "multi-selection-loader.lisp"
		     "multifont-gadget-loader.lisp"
		     "option-button-loader.lisp"
		     "polyline-creator-loader.lisp"
		     "popup-menu-button-loader.lisp"
		     "prop-sheet-loader.lisp"
		     "prop-sheet-win-loader.lisp"
		     "prop-value-loader.lisp"
		     "radio-buttons-loader.lisp"
		     "save-gadget-loader.lisp"
		     "scrolling-input-string-loader.lisp"
		     "scrolling-labeled-box-loader.lisp"
		     "scrolling-unlabeled-box-loader.lisp"
		     "scrolling-menu-loader.lisp"
		     "scrolling-window-loader.lisp"
		     "standard-edit-loader.lisp"
		     "text-buttons-loader.lisp"
		     "trill-device-loader.lisp"
		     "v-scroll-loader.lisp"
		     "v-slider-loader.lisp"
		     "x-buttons-loader.lisp"))


(progn
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
  (setf (get :garnet-modules :motif-scrolling-labeled-box) t)
  (setf (get :garnet-modules :motif-scrolling-window) t)
  (setf (get :garnet-modules :motif-scrolling-menu) t)
  (setf (get :garnet-modules :motif-prop-sheet-win) t)
  (setf (get :garnet-modules :motif-error-gadget) t)
  (setf (get :garnet-modules :motif-option-button) t)
  (setf (get :garnet-modules :motif-save-gadget) t)
  (setf (get :garnet-modules :motif-load-gadget) t)
  (setf (get :garnet-modules :multifont-gadget) t)
  (setf (get :garnet-modules :motif-menubar) t)
  )
