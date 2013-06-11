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
#|
============================================================
Change log:
	2/19/92 James Landay - created

============================================================
|#

(in-package :COMMON-LISP-USER)

(defparameter Gesture-Interactor-Version-Number "1.0")

(format t "Loading Gesture Interactor...~%")

;; check first to see if place is set
(unless (boundp 'Garnet-Gesture-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gesture-PathName before loading interactors."))


;; ---- Load gesture stuff 

(Defvar Garnet-Gesture-Files
  '(
	"gestureinter" 
	"classify" 
	"features" 
	"fileio" 
	"matrix"))

(unless (get :garnet-modules :gesture)
  (dolist (file Garnet-Gesture-Files)
    (load (merge-pathnames file Garnet-Gesture-PathName)
	  :verbose T)))

(defpackage :AGATE (:use :KR :INTER :COMMON-LISP)
  (:export DO-GO DO-STOP))

(setf (get :garnet-modules :gesture)  t)
(format t "...Done Gestures.~%")

