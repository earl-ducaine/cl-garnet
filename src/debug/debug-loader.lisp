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
        08-Nov-93 Andrew Mickish - Moved load of dependent files here from
                    inspector.lisp
        27-Aug-93 Andrew Mickish - Added suggest-constants
        10-Jan-93 Brad Myers - Added inspector 
        20-Jan-92 Andrew Mickish - Moved GARNET-DEBUG package
                                   definition into Garnet-Loader
	22-Mar-90 Robert Cook - Define the package "GARNET-DEBUG"
				for the TI Explorer
	 4-Jan-90 ECP Added version number
	18-Oct-89 RBD Created
============================================================
|#

(in-package "COMMON-LISP-USER")

(defparameter Debug-Version-Number "2.0")

(format t "Loading Debugging Tools ...~%")

;; check first to see if pathname variable is set
(unless (boundp 'Garnet-Debug-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Debug-PathName before loading debug tools."))


(unless (get :garnet-modules :multifont)
  (load (merge-pathnames "multifont-loader" Garnet-Opal-PathName)))
(unless (get :garnet-modules :text-buttons)
  (load (merge-pathnames "text-buttons-loader" Garnet-Gadgets-PathName)))
(unless (get :garnet-modules :error-gadget-utils)
  (garnet-load "gg:error-gadget-utils"))

;; ---- Load debugging tools now.

(Defvar Garnet-Debug-Files
  '(
    "debug-fns"
    "objsize"
    "inspector"
    "suggest-constants"))


(unless (get :garnet-modules :debug)
  (dolist (file Garnet-Debug-Files)
	  (load (merge-pathnames file Garnet-Debug-PathName)
		:verbose T)))

(setf (get :garnet-modules :debug)  t)
(format t "...Done Debugging Tools.~%")

