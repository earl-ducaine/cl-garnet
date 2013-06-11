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
   02/24/93 Andrew Mickish - Removed references to compile-opal/inter-p
   04/28/92 Andrew Mickish - Created
==================================================================
|#

(in-package "COMMON-LISP-USER")

;; check first to see if place is set
(unless (boundp 'Garnet-Gadgets-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gadgets-PathName before loading Gadgets."))

;; Make sure multifont stuff is loaded.
(unless (get :garnet-modules :multifont)
  (load (merge-pathnames "multifont-loader" Garnet-Opal-PathName)))

;;; Now load the multifont gadget
;;;
(unless (get :garnet-modules :multifont-gadget)
  (format t "Loading Multifont-Gadget...~%")
  (load (merge-pathnames "multifont-gadget" Garnet-Gadgets-PathName)
	:verbose T)
  (format t "...Done Multifont-Gadget.~%"))

(setf (get :garnet-modules :multifont-gadget) t)



