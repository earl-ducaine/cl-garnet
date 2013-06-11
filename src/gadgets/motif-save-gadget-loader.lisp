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
;;; Motif-Save-Gadget-Loader loads in motif text-buttons, scrolling-menu,
;;; scrolling-labeled-box, save-gadget, load-gadget, and
;;; save-load-functions

#|
==================================================================
Change log:
   08/13/92 Rajan Parthasarathy - Created
==================================================================
|#

(in-package "COMMON-LISP-USER")

;; check first to see if place is set
(unless (boundp 'garnet-gadgets-pathname)
  (error "Load 'Garnet-Loader' first to set Garnet-Gadgets-Pathname before loading Gadgets."))

;;; Now load the gadget
;;;

(unless (get :garnet-modules :motif-save-gadget)
  (format T "Loading Motif-Save-Gadget...~%")
  (dolist (pair '((:motif-text-buttons "motif-text-buttons-loader")
		  (:motif-scrolling-labeled-box "motif-scrolling-labeled-box-loader")
		  (:motif-error-gadget "motif-error-gadget-loader")
		  (:motif-scrolling-menu "motif-scrolling-menu-loader")
		  (:save-load-functions "save-load-functions")
		  (:motif-save-gadget "motif-save-gadget")
		  (:motif-load-gadget "motif-load-gadget")
		  ))
    (unless (get :garnet-modules (car pair))
      (garnet-load (concatenate 'string "gadgets:" (cadr pair)))))

  (format T "...Done Motif-Save-Gadget.~%"))

(setf (get :garnet-modules :motif-save-gadget) T)
(setf (get :garnet-modules :motif-load-gadget) T)
(setf (get :garnet-modules :save-load-functions) T)