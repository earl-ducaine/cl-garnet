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


(in-package "COMMON-LISP-USER")

(defvar *debug-gilt-mode* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim
   (if *debug-gilt-mode*
       (and (boundp 'Garnet-Compile-Debug-Settings)
	    Garnet-Compile-Debug-Settings)
       ;; Global default settings.
       (and (boundp 'Default-Garnet-Proclaim) 
	    Default-Garnet-Proclaim))))



(format t "Compiling Gilt...~%")

;; check first to see if pathname variable is set
(unless (boundp 'Garnet-Gilt-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gilt-PathName before loading this file."))


;; Only loads this file when not compiling all of Garnet.
(unless (or (get :garnet-modules :multifont)
	    (and (boundp 'common-lisp-user::compile-opal-p) common-lisp-user::compile-opal-p
		 (boundp 'common-lisp-user::compile-inter-p) common-lisp-user::compile-inter-p))
  (garnet-load "opal:multifont-loader"))

;; Only loads these file when not compiling all of Garnet.
(dolist (pair '((:text-buttons "text-buttons-loader")
		(:x-buttons "x-buttons-loader")
		(:radio-buttons "radio-buttons-loader")
		(:labeled-box "labeled-box-loader")
		(:scrolling-labeled-box "scrolling-labeled-box-loader")
		(:multi-selection "multi-selection-loader")
		(:motif-text-buttons "motif-text-buttons-loader")
		(:motif-check-buttons "motif-check-buttons-loader")
		(:motif-radio-buttons "motif-radio-buttons-loader")
		(:motif-v-scroll-bar "motif-v-scroll-loader")
		(:motif-h-scroll-bar "motif-h-scroll-loader")
		(:motif-slider "motif-slider-loader")
		(:motif-menu "motif-menu-loader")
		(:motif-gauge "motif-gauge-loader")
		(:motif-scrolling-labeled-box "motif-scrolling-labeled-box-loader")
		(:motif-error-gadget "motif-error-gadget-loader")
		(:motif-prop-sheet-win "motif-prop-sheet-win-loader")
		(:motif-scrolling-window "motif-scrolling-window-loader")
		(:motif-menubar "motif-menubar-loader")
		(:motif-trill-device "motif-trill-device-loader")
		(:standard-edit "standard-edit-loader")
		))
    (unless (get :garnet-modules (car pair))
      (garnet-load (concatenate 'string "gadgets:" (cadr pair)))))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (garnet-mkdir-if-needed Garnet-gilt-Pathname))

(defvar gilt-files
  '(
    "gilt-functions" "path-functions" "filter-functions"
    "gilt-font-imp" "motif-gilt-font-props"
    "gilt-gadget-utils" "gilt-gadgets" "motif-gilt-gadgets"
    "gilt"
     "motif-gilt-save"
     "motif-gilt-read"
    "color-imp" "motif-color-props"
    "line-imp" "motif-line-props"
    "fill-imp" "motif-fill-props"
    "align"
    
    "value-control"
    "enable-control"
    "error-check"
		     ))

(dolist (file gilt-files)
  (let ((gilt-str (concatenate 'string "gilt:" file)))
    (garnet-compile gilt-str)
    (garnet-load gilt-str))
  #+allegroV3.1(common-lisp-user::gc t))

(garnet-copy-files Garnet-Gilt-Src Garnet-Gilt-Pathname
		   '("filter-functions-loader.lisp"
		     "gilt-loader.lisp"
		     "gilt-functions-loader.lisp"
		     "path-functions-loader.lisp"
		     ))


(setf (get :garnet-modules :gilt) t)
(setf (get :garnet-modules :gilt-functions) t)

(format t "... Done Compiling Gilt~%")
