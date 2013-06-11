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
==================================================================
Change log:
         03/22/90 Robert Cook - Define the package "GARNET-GADGETS"
                                for the TI Explorer
         12/7/89 Andrew Mickish - Created
==================================================================
|#

(in-package "COMMON-LISP-USER")

;; check first to see if place is set
(unless (boundp 'Garnet-Gadgets-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gadgets-PathName before loading Gadgets."))


;; Load v-scroll-bar and scrolling-menu modules if necessary

(unless (get :garnet-modules :scrolling-menu)
  (format t "Loading Scrolling-Menu...~%")
  (dolist (file '("v-scroll-loader"
		  "scrolling-menu"))
    (load (merge-pathnames file Garnet-Gadgets-PathName)
	  :verbose T))
  (format t "...Done Scrolling-Menu.~%"))

(setf (get :garnet-modules :scrolling-menu) t)

