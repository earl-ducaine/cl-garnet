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
     2/24/93 Andrew Mickish - Removed references to compile-opal/inter-p
     6/16/92 Rajan Parthasarathy - Added load of scrolling-window-multifont
     3/14/91  Brad Myers - Added Parts file
     7/5/90 Brad Myers - Created
     8/6/90 Ed Pervin - Moved load of h-scroll-loader, v-scroll-loader
			from scrolling-window.lisp to here.
==================================================================
|#

(in-package "COMMON-LISP-USER")

;; check first to see if place is set
(unless (boundp 'Garnet-Gadgets-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gadgets-PathName before loading Gadgets."))

;;; Now load the scrolling-window module
;;;
(unless (get :garnet-modules :scrolling-window)
  (format t "Loading Scrolling-Window...~%")
  (dolist (pair '((:h-scroll-bar "h-scroll-loader")
                  (:v-scroll-bar "v-scroll-loader")
                  (:scrolling-window-parts "scrolling-window-parts")
                  (:scrolling-window "scrolling-window")))
    (unless (get :garnet-modules (car pair))
      (load (merge-pathnames (cadr pair) Garnet-Gadgets-PathName)
            :verbose T)))
  (format t "...Done Scrolling-Window.~%"))

; Load scrolling functions for multifont if multifont files already loaded
; (otherwise they will be loaded by the multifont-loader when needed).
(if (get :garnet-modules :multifont)
    (load (merge-pathnames "scrolling-window-multifont" Garnet-Gadgets-Pathname)
	  :verbose T))

(setf (get :garnet-modules :scrolling-window-parts) t)
(setf (get :garnet-modules :scrolling-window) t)

