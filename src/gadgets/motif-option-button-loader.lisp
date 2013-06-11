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
;;; $Id::                                                             $	


#|
==================================================================
Change log:
   7/29/92 Rajan - Created
==================================================================
|#

(in-package "COMMON-LISP-USER")

;; check first to see if place is set
(unless (boundp 'Garnet-Gadgets-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gadgets-PathName before loading Gadgets."))

;;; Now load the motif-option-button and dependent files
;;;
(unless (get :garnet-modules :motif-option-button)
  (format t "Loading Motif-Option-Button...~%")
  (dolist (pair '((:motif-text-buttons "motif-text-buttons-loader")
		  (:motif-menu "motif-menu-loader")
		  (:motif-option-button "motif-option-button")))
    (unless (get :garnet-modules (car pair))
      (load (merge-pathnames (cadr pair) Garnet-Gadgets-PathName)
	    :verbose T)))
  (format t "...Done Motif-Option-Button.~%"))
 
(setf (get :garnet-modules :motif-option-button) t)
