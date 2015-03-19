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


(in-package :COMMON-LISP-USER)

(defvar *debug-gesture-mode* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim
   (if *debug-gesture-mode*
       (and (boundp 'Garnet-Compile-Debug-Settings)
	    Garnet-Compile-Debug-Settings)
       ;; Global default settings.
       (and (boundp 'Default-Garnet-Proclaim) 
	    Default-Garnet-Proclaim))))

(dolist (pair '((:motif-text-buttons "motif-text-buttons-loader")
		(:motif-scrolling-labeled-box "motif-scrolling-labeled-box-loader")
		(:motif-radio-buttons "motif-radio-buttons-loader")
		(:motif-error-gadget "motif-error-gadget-loader")
                (:motif-save-gadget "motif-save-gadget-loader")
                (:motif-scrolling-window "motif-scrolling-window-loader")))
  (unless (get :garnet-modules (car pair))
    (garnet-load (concatenate 'string "gadgets:" (cadr pair)))))

(defpackage :AGATE (:use :KR :INTER :COMMON-LISP)
  (:export DO-GO DO-STOP))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (garnet-mkdir-if-needed Garnet-Gesture-Pathname))


(Defvar Garnet-Gesture-Files
  '(
    "features" 
    "matrix"
    "classify" 
    "gestureinter" 
    "fileio" 
    "train"
    "agate"
    ))

(dolist (file Garnet-Gesture-Files)
  (let ((gfile (concatenate 'string "gesture:" file)))
    (garnet-compile gfile)
    (unless (string= file "agate")
    (garnet-load gfile))))

(garnet-copy-files Garnet-gesture-Src Garnet-gesture-Pathname
		   '("gesture-loader.lisp"))


#+allegro-V3.1 (gc t)

(setf (get :garnet-modules :gesture) t)

