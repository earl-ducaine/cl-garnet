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
;;; Changes:
;;; 12-Oct-90 Osamu - Added demo-xasperate, demo-calculator, demos-controller
;;;  3-Aug-90 Myers - Added demo-fade.
;;;  1-Aug-90 Myers - Added demo-arith.
;;; 23-Jul-90 Mickish - Changed demo-gadgets-loader to demo-gadgets.
;;; 16-Jul-90 Mickish - Added demo-file-browser and demo-schema-browser.
;;; 11-Jun-90 Pervin - Optional double-buffering in go-demos.
;;; 22-Mar-90 Robert Cook - Define all the DEMO packages
;;;                            for the TI Explorer.
;;; 4-Jan-90 Pervin - Added version number
;;; 4-Jan-90 Mickish - Load Demo-Gadgets-Loader instead of Demo-Gadgets
;;;

(in-package "COMMON-LISP-USER")

(defparameter Demos-Version-Number "1.0")

(format t "Loading Demos...~%")

;;; check to see if place is set
(unless (boundp 'Garnet-Demos-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Demos-PathName before loading demos."))



;;;  Load Demos Controller ...

(load (merge-pathnames "demos-controller" Garnet-Demos-PathName)
	:verbose T)

(setf (get :garnet-modules :demos) t)
(format t "...Done Loading Demos.~%")

(defun go-demos ()
  (demos-controller:do-go))

(defun stop-demos()
  (demos-controller:do-stop))

(format t "~%**Use (go-demos) to start all demos 
**   and (stop-demos) to stop all the demos~%")

