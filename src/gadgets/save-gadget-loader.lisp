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
;;; Save-Gadget-Loader loads in text-buttons, scrolling-menu,
;;; scrolling-labeled-box, save-gadget, load-gadget, and
;;; save-load-functions

#|
==================================================================
Change log:
   05/05/94 Andrew Mickish - Used garnet-load
   08/13/92 Rajan Parthasarathy - Created
==================================================================
|#

(in-package "COMMON-LISP-USER")

;; check first to see if place is set
(unless (boundp 'garnet-gadgets-pathname)
  (error "Load 'Garnet-Loader' first to set Garnet-Gadgets-Pathname before loading Gadgets."))

;;; Now load the gadget
;;;

(unless (get :garnet-modules :save-gadget)
  (format T "Loading Save-Gadget...~%")
  (dolist (pair '((:text-buttons "text-buttons-loader")
		  (:scrolling-menu "scrolling-menu-loader")
		  (:scrolling-labeled-box "scrolling-labeled-box-loader")
		  (:error-gadget "error-gadget-loader")
		  (:save-load-functions "save-load-functions")
	          (:save-gadget "save-gadget")
		  (:load-gadget "load-gadget")
		  ))
    (unless (get :garnet-modules (car pair))
      (garnet-load (concatenate 'string "gadgets:" (cadr pair)))))

  (format T "...Done Save-Gadget.~%"))

(setf (get :garnet-modules :save-load-functions) T)
(setf (get :garnet-modules :save-gadget) T)
(setf (get :garnet-modules :load-gadget) T)

